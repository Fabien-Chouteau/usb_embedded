------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with USB.Utils;
with USB.Logging.Device;

with BBqueue;         use BBqueue;
with BBqueue.Buffers; use BBqueue.Buffers;

package body USB.Device.MIDI is

   EP_Buffer_Size : constant := 64;

   type Class_Request_Type is
     (Get_Report, Get_Idle, Get_Protocol, Set_Report, Set_Idle, Set_Protocol);
   for Class_Request_Type use (Get_Report   => 1,
                               Get_Idle     => 2,
                               Get_Protocol => 3,
                               Set_Report   => 9,
                               Set_Idle     => 10,
                               Set_Protocol => 11);

   -------------
   -- Receive --
   -------------

   function Receive (This : in out Default_MIDI_Class;
                     Evt  :    out MIDI_Event)
                     return Boolean
   is
      RG : BBqueue.Buffers.Read_Grant;
   begin

      --  if Logs_Enabled then
      --     USB.Logging.Device.Log_MIDI_Receive;
      --  end if;

      Read (This.RX_Queue, RG, 4);

      if State (RG) = Valid then
         declare
            Src : MIDI_Event with Address => Slice (RG).Addr;
         begin
            Evt := Src;
         end;

         Release (This.RX_Queue, RG);
         return True;
      else
         return False;
      end if;
   end Receive;

   ----------
   -- Send --
   ----------

   procedure Send (This : in out Default_MIDI_Class;
                   UDC  : in out USB_Device_Controller'Class;
                   Evt  :        MIDI_Event)
   is
      WG : BBqueue.Buffers.Write_Grant;
   begin

      USB.Logging.Device.Log_MIDI_Send;

      Grant (This.TX_Queue, WG, 4);

      if State (WG) = Valid then
         declare
            Dst : MIDI_Event with Address => Slice (WG).Addr;
         begin
            Dst := Evt;
         end;

         Commit (This.TX_Queue, WG);
      else
         This.TX_Discarded := This.TX_Discarded + 1;
      end if;

      This.Setup_TX (UDC);
   end Send;

   --------------------------
   -- Set_Interface_String --
   --------------------------

   procedure Set_Interface_String (This  : in out Default_MIDI_Class;
                                   Stack : in out USB_Device_Stack'Class;
                                   Str   :        String)
   is
   begin
      This.Iface_Str := USB.Device.Register_String (Stack,
                                                    USB.To_USB_String (Str));
   end Set_Interface_String;

   --------------
   -- Setup_RX --
   --------------

   procedure Setup_RX (This : in out Default_MIDI_Class;
                       UDC  : in out USB_Device_Controller'Class)
   is
   begin

      USB.Logging.Device.Log_MIDI_Setup_RX;

      UDC.EP_Ready_For_Data (EP      => This.EP,
                             Max_Len => EP_Buffer_Size,
                             Ready   => True);
   end Setup_RX;

   --------------
   -- Setup_TX --
   --------------

   procedure Setup_TX (This : in out Default_MIDI_Class;
                       UDC  : in out USB_Device_Controller'Class)
   is
      RG : BBqueue.Buffers.Read_Grant;

      In_Progress : Boolean;
   begin

      USB.Logging.Device.Log_MIDI_Setup_TX;

      Atomic.Test_And_Set (This.TX_In_Progress, In_Progress);
      if In_Progress then
         return;
      end if;

      Read (This.TX_Queue, RG, EP_Buffer_Size);

      if State (RG) = Valid then

         --  Copy into IN buffer
         USB.Utils.Copy (Src   => Slice (RG).Addr,
                         Dst   => This.EP_In_Buf,
                         Count => Natural (Slice (RG).Length));

         USB.Logging.Device.Log_MIDI_Write_Packet;

         --  Send IN buffer
         UDC.EP_Send_Packet (Ep  => This.EP,
                             Len => Packet_Size (Slice (RG).Length));

         Release (This.TX_Queue, RG);
      else
         Atomic.Clear (This.TX_In_Progress);
      end if;
   end Setup_TX;

   ----------------
   -- Initialize --
   ----------------

   overriding
   function Initialize (This                 : in out Default_MIDI_Class;
                        Dev                  : in out USB_Device_Stack'Class;
                        Base_Interface_Index :        Interface_Id)
                        return Init_Result
   is
   begin

      USB.Logging.Device.Log_MIDI_Init;

      if not Dev.Request_Endpoint (Interrupt, This.EP) then
         return Not_Enough_EPs;
      end if;

      This.EP_Out_Buf := Dev.Request_Buffer ((This.EP, EP_Out),
                                             EP_Buffer_Size);
      if This.EP_Out_Buf = System.Null_Address then
         return Not_Enough_EP_Buffer;
      end if;

      This.EP_In_Buf := Dev.Request_Buffer ((This.EP, EP_In),
                                            EP_Buffer_Size);
      if This.EP_In_Buf = System.Null_Address then
         return Not_Enough_EP_Buffer;
      end if;

      This.Interface_Index := Base_Interface_Index;

      return Ok;
   end Initialize;

   --------------------
   -- Get_Class_Info --
   --------------------

   overriding
   procedure Get_Class_Info
     (This                     : in out Default_MIDI_Class;
      Number_Of_Interfaces     :    out Interface_Id;
      Config_Descriptor_Length :    out Natural)
   is
      pragma Unreferenced (This);
   begin
      Number_Of_Interfaces := 2;
      Config_Descriptor_Length := 92;
   end Get_Class_Info;

   ----------------------------
   -- Fill_Config_Descriptor --
   ----------------------------

   overriding
   procedure Fill_Config_Descriptor (This : in out Default_MIDI_Class;
                                     Data :    out UInt8_Array)
   is
      F : constant Natural := Data'First;

      USB_CLASS_AUDIO : constant := 1;
   begin

      pragma Style_Checks (Off);

      --  B.3.1 Standard AC Interface Descriptor
      --  The AudioControl interface has no dedicated endpoints associated with it. It uses the
      --  default pipe (endpoint 0) for all communication purposes. Class-specific AudioControl
      --  Requests are sent using the default pipe. There is no Status Interrupt endpoint provided.
      --  descriptor follows inline:
      Data (F + 0 .. F + 91) :=
        (9, --  sizeof(usbDescrInterface): length of descriptor in bytes
         Dt_Interface'Enum_Rep, --  descriptor type
         UInt8 (This.Interface_Index), --  index of this interface
         0, --  alternate setting for this interface
         0, --  endpoints excl 0: number of endpoint descriptors to follow
         USB_CLASS_AUDIO, --  Class audio
         1, --  Subclass control
         0, --
         UInt8 (This.Iface_Str), --  string index for interface

         --  B.3.2 Class-specific AC Interface Descriptor
         --  The Class-specific AC interface descriptor is always headed by a Header descriptor
         --  that contains general information about the AudioControl interface. It contains all
         --  the pointers needed to describe the Audio Interface Collection, associated with the
         --  described audio function. Only the Header descriptor is present in this device
         --  because it does not contain any audio functionality as such.
         --  descriptor follows inline:
         9, --  sizeof(usbDescrCDC_HeaderFn): length of descriptor in bytes
         36, --  descriptor type
         1, --  header functional descriptor
         0, 0, --  bcdADC
         9, 0, --  wTotalLength
         1, --
         1, --


         --  B.4 MIDIStreaming Interface Descriptors

         --  B.4.1 Standard MS Interface Descriptor
         --  descriptor follows inline:
         9, --  length of descriptor in bytes
         Dt_Interface'Enum_Rep, --  descriptor type
         UInt8 (This.Interface_Index + 1), --  index of this interface
         0, --  alternate setting for this interface
         2, --  endpoints excl 0: number of endpoint descriptors to follow
         USB_CLASS_AUDIO, --  AUDIO
         3, --  MIDI Streaming
         0, --  unused
         UInt8 (This.Iface_Str), --  string index for interface

         --  B.4.2 Class-specific MS Interface Descriptor
         --  descriptor follows inline:
         7, --  length of descriptor in bytes
         36, --  descriptor type
         1, --  header functional descriptor
         0, 1, --  bcdADC
         65, 0, --  wTotalLength

         --  B.4.3 MIDI IN Jack Descriptor
         --  descriptor follows inline:
         6, --  bLength
         36, --  descriptor type
         2, --  MIDI_IN_JACK desc subtype
         1, --  EMBEDDED bJackType
         1, --  bJackID
         0, --  iJack

         --  descriptor follows inline:
         6, --  bLength
         36, --  descriptor type
         2, --  MIDI_IN_JACK desc subtype
         2, --  EXTERNAL bJackType
         2, --  bJackID
         0, --  iJack

         --  B.4.4 MIDI OUT Jack Descriptor
         --  descriptor follows inline:
         9, --  length of descriptor in bytes
         36, --  descriptor type
         3, --  MIDI_OUT_JACK descriptor
         1, --  EMBEDDED bJackType
         3, --  bJackID
         1, --  No of input pins
         2, --  BaSourceID
         1, --  BaSourcePin
         0, --  iJack

         --  descriptor follows inline:
         9, --  bLength of descriptor in bytes
         36, --  bDescriptorType
         3, --  MIDI_OUT_JACK bDescriptorSubtype
         2, --  EXTERNAL bJackType
         4, --  bJackID
         1, --  bNrInputPins
         1, --  baSourceID (0)
         1, --  baSourcePin (0)
         0, --  iJack

         --  B.5 Bulk OUT Endpoint Descriptors

         --  here 27 ---

         --  B.5.1 Standard Bulk OUT Endpoint Descriptor
         --  descriptor follows inline:
         9, --  bLenght
         Dt_Endpoint'Enum_Rep, --  bDescriptorType = endpoint
         UInt8 (This.EP), --  bEndpointAddress OUT endpoint number 1
         Bulk'Enum_Rep, --  bmAttributes: 2:Bulk, 3:Interrupt endpoint
         EP_Buffer_Size, 0, --  wMaxPacketSize
         10, --  bInterval in ms
         0, --  bRefresh
         0, --  bSyncAddress

         --  B.5.2 Class-specific MS Bulk OUT Endpoint Descriptor
         --  descriptor follows inline:
         5, --  bLength of descriptor in bytes
         37, --  bDescriptorType
         1, --  bDescriptorSubtype
         1, --  bNumEmbMIDIJack
         1, --  baAssocJackID (0)

         --  B.6 Bulk IN Endpoint Descriptors

         --  B.6.1 Standard Bulk IN Endpoint Descriptor
         --  descriptor follows inline:
         9, --  bLenght
         Dt_Endpoint'Enum_Rep, --  bDescriptorType = endpoint
         16#80# or UInt8 (This.EP), --  bEndpointAddress IN endpoint number 1
         Bulk'Enum_Rep, --  Bulk EP
         EP_Buffer_Size, 0, --  wMaxPacketSize
         10, --  bInterval in ms
         0, --  bRefresh
         0, --  bSyncAddress

         --  B.6.2 Class-specific MS Bulk IN Endpoint Descriptor
         --  descriptor follows inline:
         5, --  bLength of descriptor in bytes
         37, --  bDescriptorType
         1, --  bDescriptorSubtype
         1, --  bNumEmbMIDIJack (0)
         3  --  baAssocJackID (0)
        );


   end Fill_Config_Descriptor;

   ---------------
   -- Configure --
   ---------------

   overriding
   function Configure
     (This  : in out Default_MIDI_Class;
      UDC   : in out USB_Device_Controller'Class;
      Index : UInt16)
      return Setup_Request_Answer
   is
   begin
      USB.Logging.Device.Log_MIDI_Config;

      if Index = 1 then

         UDC.EP_Setup (EP  => (This.EP, EP_In),
                       Typ => Bulk);
         UDC.EP_Setup (EP  => (This.EP, EP_Out),
                       Typ => Bulk);

         This.Setup_RX (UDC);

         This.State := Idle;
         return Handled;
      else
         return Not_Supported;
      end if;
   end Configure;

   -------------------
   -- Setup_Request --
   -------------------

   overriding
   function Setup_Read_Request (This  : in out Default_MIDI_Class;
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
         when Get_Report'Enum_Rep =>
            return Not_Supported;
         when Get_Idle'Enum_Rep =>
            return Not_Supported;
         when Get_Protocol'Enum_Rep =>
            return Not_Supported;
         when Set_Report'Enum_Rep =>
            return Not_Supported;
         when Set_Idle'Enum_Rep =>
            This.Idle_State := Utils.High (Req.Value);
            return Handled;
         when Set_Protocol'Enum_Rep =>
            return Not_Supported;
         when others =>
            return Next_Callback;
         end case;
      end if;

      if Req.RType.Typ = Stand
        and then
          Req.Request = Req_Get_Descriptor'Enum_Rep
      then
         declare
            --  Index     : constant UInt8 := Utils.Low (Req.Value);
            Desc_Type : constant UInt8 := Utils.High (Req.Value);

         begin
            case Desc_Type is
               when others =>
                  raise Program_Error with "Unknown desc in MIDI class";
            end case;
         end;
      end if;

      return Next_Callback;
   end Setup_Read_Request;

   -------------------------
   -- Setup_Write_Request --
   -------------------------

   overriding
   function Setup_Write_Request (This  : in out Default_MIDI_Class;
                                 Req   : Setup_Data;
                                 Data  : UInt8_Array)
                                 return Setup_Request_Answer
   is (Not_Supported);

   -----------------------
   -- Transfer_Complete --
   -----------------------

   overriding
   procedure Transfer_Complete (This : in out Default_MIDI_Class;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   :        EP_Addr;
                                CNT  :        Packet_Size)
   is
   begin
      if EP = (This.EP, EP_Out) then

         USB.Logging.Device.Log_MIDI_Out_TC;

         --  Move OUT data to the RX queue
         declare
            WG : BBqueue.Buffers.Write_Grant;
         begin
            Grant (This.RX_Queue, WG, BBqueue.Count (CNT));

            if State (WG) = Valid then

               USB.Utils.Copy (Src   => This.EP_Out_Buf,
                               Dst   => Slice (WG).Addr,
                               Count => CNT);

               Commit (This.RX_Queue, WG, BBqueue.Count (CNT));
            else

               USB.Logging.Device.Log_MIDI_RX_Discarded;

               This.RX_Discarded := This.RX_Discarded + 1;
            end if;
         end;

         This.Setup_RX (UDC);

      elsif EP = (This.EP, EP_In) then

         USB.Logging.Device.Log_MIDI_In_TC;

         Atomic.Clear (This.TX_In_Progress);

         This.Setup_TX (UDC);

      else
         raise Program_Error with "Not expecting transfer on EP";
      end if;
   end Transfer_Complete;

end USB.Device.MIDI;
