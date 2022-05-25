------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with BBqueue;         use BBqueue;
with BBqueue.Buffers; use BBqueue.Buffers;

with USB.Utils;
with USB.Logging.Device;

package body USB.Device.Serial is

   Bulk_Buffer_Size : constant := 64;

   ----------------
   -- Initialize --
   ----------------

   overriding
   function Initialize (This                 : in out Default_Serial_Class;
                        Dev                  : in out USB_Device_Stack'Class;
                        Base_Interface_Index :        Interface_Id)
                        return Init_Result
   is
   begin

      USB.Logging.Device.Log_Serial_Init;

      --  Request Interrupt EP --

      if not Dev.Request_Endpoint (Interrupt, This.Int_EP) then
         return Not_Enough_EPs;
      end if;

      This.Int_Buf := Dev.Request_Buffer ((This.Int_EP, EP_In),
                                          Bulk_Buffer_Size);
      if This.Int_Buf = System.Null_Address then
         return Not_Enough_EP_Buffer;
      end if;

      --  Request Bulk EPs --

      if not Dev.Request_Endpoint (Bulk, This.Bulk_EP) then
         return Not_Enough_EPs;
      end if;

      This.Bulk_Out_Buf := Dev.Request_Buffer ((This.Bulk_EP, EP_Out),
                                               Bulk_Buffer_Size);
      if This.Bulk_Out_Buf = System.Null_Address then
         return Not_Enough_EP_Buffer;
      end if;

      This.Bulk_In_Buf := Dev.Request_Buffer ((This.Bulk_EP, EP_In),
                                              Bulk_Buffer_Size);
      if This.Bulk_In_Buf = System.Null_Address then
         return Not_Enough_EP_Buffer;
      end if;

      -- Interface --

      This.Interface_Index := Base_Interface_Index;

      --  Default line coding
      This.Coding.Bitrate := 115_200;
      This.Coding.Stop_Bit := 0;
      This.Coding.Parity := 0;
      This.Coding.Data_Bits := 8;

      return Ok;
   end Initialize;

   --------------------
   -- Get_Class_Info --
   --------------------

   overriding
   procedure Get_Class_Info
     (This                     : in out Default_Serial_Class;
      Number_Of_Interfaces     :    out Interface_Id;
      Config_Descriptor_Length :    out Natural)
   is
      pragma Unreferenced (This);
   begin
      Number_Of_Interfaces := 2;
      Config_Descriptor_Length := 66;
   end Get_Class_Info;

   ----------------------------
   -- Fill_Config_Descriptor --
   ----------------------------

   overriding
   procedure Fill_Config_Descriptor (This : in out Default_Serial_Class;
                                     Data :    out UInt8_Array)
   is
      F : constant Natural := Data'First;

      USB_DESC_TYPE_INTERFACE     : constant := 4;
      USB_DESC_TYPE_ENDPOINT      : constant := 5;
      USB_DESC_CS_INTERFACE       : constant := 16#24#;
      USB_CLASS_CDC : constant := 2;
      USB_CLASS_CDC_DATA : constant := 10;
      CDC_COMM_SUBCLASS_ABSTRACT_CONTROL_MODEL : constant := 2;
      CDC_FUNC_DESC_HEADER : constant := 0;
      CDC_FUNC_DESC_CALL_MANAGEMENT : constant := 1;
      CDC_FUNC_DESC_ABSTRACT_CONTROL_MANAGEMENT : constant := 2;
      CDC_FUNC_DESC_UNION : constant := 6;
   begin
      Data (F + 0 .. F + 65) :=
        (
         -- Interface Associate --
         8, -- bLength
         16#0B#, -- bDescriptorType 0x0B
         UInt8 (This.Interface_Index), -- bFirstInterface
         2, -- bInterfaceCount
         USB_CLASS_CDC, -- bFunctionClass
         CDC_COMM_SUBCLASS_ABSTRACT_CONTROL_MODEL, -- bFunctionSubClass
         0, -- bFunctionProtocol
         0, -- iFunction

         -- CDC Control Interface --
         9, -- bLength
         USB_DESC_TYPE_INTERFACE, -- bDescriptorType
         UInt8 (This.Interface_Index), -- bInterfaceNumber
         0, -- bAlternateSetting
         1, -- bNumEndpoints
         USB_CLASS_CDC, -- bInterfaceClass
         CDC_COMM_SUBCLASS_ABSTRACT_CONTROL_MODEL, -- bInterfaceSubClass
         0, -- bInterfaceProtocol
         UInt8 (This.Iface_Str), -- iInterface (String index)

         -- CDC Header --
         5,
         USB_DESC_CS_INTERFACE,
         CDC_FUNC_DESC_HEADER,
         16#20#,
         16#01#,

         -- CDC Call --
         5,
         USB_DESC_CS_INTERFACE,
         CDC_FUNC_DESC_CALL_MANAGEMENT,
         0,
         UInt8 (This.Interface_Index + 1),

         -- CDC ACM: support line request --
         4,
         USB_DESC_CS_INTERFACE,
         CDC_FUNC_DESC_ABSTRACT_CONTROL_MANAGEMENT,
         2,

         -- CDC Union --
         5,
         USB_DESC_CS_INTERFACE,
         CDC_FUNC_DESC_UNION,
         UInt8 (This.Interface_Index),
         UInt8 (This.Interface_Index + 1),

         -- Endpoint Notification --
         7,
         USB_DESC_TYPE_ENDPOINT,
         16#80# or UInt8 (This.Int_EP), -- In EP
         3, -- Interrupt EP
         16#40#, 0, --  TODO: Max packet size
         16, -- Polling interval

         -- CDC Control Interface --
         9, -- bLength
         USB_DESC_TYPE_INTERFACE, -- bDescriptorType
         UInt8 (This.Interface_Index + 1), -- bInterfaceNumber
         0, -- bAlternateSetting
         2, -- bNumEndpoints
         USB_CLASS_CDC_DATA, -- bInterfaceClass
         0, -- bInterfaceSubClass
         0, -- bInterfaceProtocol
         0, -- iInterface (String index)

         -- Endpoint Data out --
         7,
         USB_DESC_TYPE_ENDPOINT,
         UInt8 (This.Bulk_EP), -- Out EP
         2, -- Bulk EP
         16#40#, 0, --  TODO: Max packet size
         0, -- Polling interval

         -- Endpoint Data in --
         7,
         USB_DESC_TYPE_ENDPOINT,
         16#80# or UInt8 (This.Bulk_EP), -- In EP
         2, -- Bulk EP
         16#40#, 0, --  TODO: Max packet size
         0

        );
   end Fill_Config_Descriptor;

   ---------------
   -- Configure --
   ---------------

   overriding
   function Configure
     (This  : in out Default_Serial_Class;
      UDC   : in out USB_Device_Controller'Class;
      Index : UInt16)
      return Setup_Request_Answer
   is
   begin
      USB.Logging.Device.Log_Serial_Config;

      if Index = 1 then

         UDC.EP_Setup (EP  => (This.Int_EP, EP_In),
                       Typ => Interrupt);
         UDC.EP_Setup (EP  => (This.Bulk_EP, EP_In),
                       Typ => Bulk);
         UDC.EP_Setup (EP  => (This.Bulk_EP, EP_Out),
                       Typ => Bulk);

         This.Setup_RX (UDC);

         return Handled;
      else
         return Not_Supported;
      end if;
   end Configure;

   -------------------
   -- Setup_Request --
   -------------------

   overriding
   function Setup_Read_Request (This  : in out Default_Serial_Class;
                                Req   : Setup_Data;
                                Buf   : out System.Address;
                                Len   : out Buffer_Len)
                                return Setup_Request_Answer
   is
   begin
      Buf := System.Null_Address;
      Len := 0;

      if Req.RType.Typ = Class and then Req.RType.Recipient = Iface then
         case Req.Request is
         when 16#21# => -- GET_LINE_CODING
            Buf := This.Coding'Address;
            Len := This.Coding'Size / 8;
            return Handled;
         when others =>
            raise Program_Error with "Unknown Serial request";
         end case;
      end if;

      return Next_Callback;
   end Setup_Read_Request;

   -------------------------
   -- Setup_Write_Request --
   -------------------------

   overriding
   function Setup_Write_Request (This  : in out Default_Serial_Class;
                                 Req   : Setup_Data;
                                 Data  : UInt8_Array)
                                 return Setup_Request_Answer
   is
   begin
      if Req.RType.Typ = Class and then Req.RType.Recipient = Iface then
         case Req.Request is
         when 16#20# => -- SET_LINE_CODING
            if Data'Length = (This.Coding'Size / 8) then
               declare
                  Dst : UInt8_Array (1 .. This.Coding'Size / 8)
                    with Address => This.Coding'Address;
               begin
                  Dst := Data;
                  return Handled;
               end;
            else
               return Not_Supported;
            end if;
         when 16#22# => -- SET_CONTROL_LINE_STATE
            This.State.DTE_Is_Present := (Req.Value and 1) /= 0;
            This.State.Half_Duplex_Carrier_control := (Req.Value and 2) /= 0;
            return Handled;
         when 16#23# => -- SEND_BREAK
            --  TODO: Break are ignored for now...
            return Handled;
         when others =>
            raise Program_Error with "Unknown Serial request";
         end case;
      end if;

      return Next_Callback;
   end Setup_Write_Request;

   -----------------------
   -- Transfer_Complete --
   -----------------------

   overriding
   procedure Transfer_Complete (This : in out Default_Serial_Class;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   :        EP_Addr;
                                CNT  :        Packet_Size)
   is
   begin
      if EP = (This.Bulk_EP, EP_Out) then

         USB.Logging.Device.Log_Serial_Out_TC;

         --  Move OUT data to the RX queue
         declare
            WG : BBqueue.Buffers.Write_Grant;
         begin
            Grant (This.RX_Queue, WG, BBqueue.Count (CNT));

            if State (WG) = Valid then

               USB.Utils.Copy (Src   => This.Bulk_Out_Buf,
                               Dst   => Slice (WG).Addr,
                               Count => CNT);

               Commit (This.RX_Queue, WG, BBqueue.Count (CNT));
            end if;
         end;

         This.Setup_RX (UDC);

      elsif EP = (This.Bulk_EP, EP_In) then
         USB.Logging.Device.Log_Serial_In_TC;

         Atomic.Clear (This.TX_In_Progress);

         This.Setup_TX (UDC);

      else
         raise Program_Error with "Not expecting transfer on EP";
      end if;
   end Transfer_Complete;

   --------------
   -- Setup_RX --
   --------------

   procedure Setup_RX (This : in out Default_Serial_Class;
                       UDC  : in out USB_Device_Controller'Class)
   is
   begin
      USB.Logging.Device.Log_Serial_Setup_RX;

      UDC.EP_Ready_For_Data (EP      => This.Bulk_EP,
                             Max_Len => Bulk_Buffer_Size,
                             Ready   => True);
   end Setup_RX;

   --------------
   -- Setup_TX --
   --------------

   procedure Setup_TX (This : in out Default_Serial_Class;
                       UDC  : in out USB_Device_Controller'Class)
   is
      RG : BBqueue.Buffers.Read_Grant;
      TX_In_Progress : Boolean;
   begin

      Atomic.Test_And_Set (This.TX_In_Progress, TX_In_Progress);
      if TX_In_Progress then
         return;
      end if;

      USB.Logging.Device.Log_Serial_Setup_TX;

      Read (This.TX_Queue, RG, Bulk_Buffer_Size);

      if State (RG) = Valid then

         --  Copy into IN buffer
         USB.Utils.Copy (Src   => Slice (RG).Addr,
                         Dst   => This.Bulk_In_Buf,
                         Count => Natural (Slice (RG).Length));

         USB.Logging.Device.Log_Serial_Write_Packet;

         --  Send IN buffer
         UDC.EP_Send_Packet (Ep  => This.Bulk_EP,
                             Len => Packet_Size (Slice (RG).Length));

         Release (This.TX_Queue, RG);
      else
         Atomic.Clear (This.TX_In_Progress);
      end if;
   end Setup_TX;

   -----------------
   -- Line_Coding --
   -----------------

   function Line_Coding (This : Default_Serial_Class)
                         return CDC_Line_Coding
   is (This.Coding);

   ---------------------
   -- List_Ctrl_State --
   ---------------------

   function List_Ctrl_State (This : Default_Serial_Class)
                             return CDC_Line_Control_State
   is (This.State);

   --------------------------
   -- Set_Interface_String --
   --------------------------

   procedure Set_Interface_String (This  : in out Default_Serial_Class;
                                   Stack : in out USB_Device_Stack'Class;
                                   Str   :        String)
   is
   begin
      This.Iface_Str := USB.Device.Register_String (Stack,
                                                    USB.To_USB_String (Str));
   end Set_Interface_String;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out Default_Serial_Class;
                   Buf  :        System.Address;
                   Len  : in out UInt32)
   is
      RG : BBqueue.Buffers.Read_Grant;
   begin
      Read (This.RX_Queue, RG, Count (Len));

      if State (RG) = Valid then
         USB.Logging.Device.Log_Serial_Receive;

         Len := UInt32 (Slice (RG).Length);
         USB.Utils.Copy (Src   => Slice (RG).Addr,
                         Dst   => Buf,
                         Count => Len);

         Release (This.RX_Queue, RG);
      else
         Len := 0;
      end if;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out Default_Serial_Class;
                   Str  :    out String;
                   Len  :    out UInt32)
   is
   begin
      Len := Str'Length;
      This.Read (Str'Address, Len);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (This : in out Default_Serial_Class;
                    UDC  : in out USB_Device_Controller'Class;
                    Buf  :        System.Address;
                    Len  : in out UInt32)
   is
      WG : BBqueue.Buffers.Write_Grant;
   begin
      Grant (This.TX_Queue, WG, Count (Len));

      if State (WG) = Valid then

         USB.Logging.Device.Log_Serial_Send;

         Len := UInt32'Min (Len, UInt32 (Slice (WG).Length));

         USB.Utils.Copy (Src   => Buf,
                         Dst   => Slice (WG).Addr,
                         Count => Len);

         Commit (This.TX_Queue, WG);

         This.Setup_TX (UDC);
      else
         Len := 0;
      end if;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write (This : in out Default_Serial_Class;
                    UDC  : in out USB_Device_Controller'Class;
                    Str  :        String;
                    Len  :    out UInt32)
   is
   begin
      Len := Str'Length;

      This.Write (UDC, Str'Address, Len);
   end Write;

end USB.Device.Serial;
