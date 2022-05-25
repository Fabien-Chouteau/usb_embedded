------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

with BBqueue;
private with BBqueue.Buffers;
private with Atomic;

package USB.Device.Serial is

   type Default_Serial_Class (TX_Buffer_Size, RX_Buffer_Size : BBqueue.Count)
   is limited new USB_Device_Class with private;

   type CDC_Line_Coding is record
      Bitrate   : UInt32;
      Stop_Bit  : UInt8;
      Parity    : UInt8;
      Data_Bits : UInt8;
   end record
     with Pack, Size => 56;

   type CDC_Line_Control_State is record
      DTE_Is_Present   : Boolean;
      Half_Duplex_Carrier_control : Boolean;
      Reserved : UInt14;
   end record
     with Pack, Size => 16;

   function Line_Coding (This : Default_Serial_Class)
                         return CDC_Line_Coding;

   function List_Ctrl_State (This : Default_Serial_Class)
                             return CDC_Line_Control_State;

   procedure Set_Interface_String (This  : in out Default_Serial_Class;
                                   Stack : in out USB_Device_Stack'Class;
                                   Str   :        String);
   --  Only has effect when called before enumeration

   ------------------------------------
   -- Reading Data from the USB Host --
   ------------------------------------

   procedure Read (This : in out Default_Serial_Class;
                   Buf  :        System.Address;
                   Len  : in out UInt32);

   procedure Read (This : in out Default_Serial_Class;
                   Str  :    out String;
                   Len  :    out UInt32);

   ----------------------------------
   -- Writing Data to the USB Host --
   ----------------------------------

   procedure Write (This : in out Default_Serial_Class;
                    UDC  : in out USB_Device_Controller'Class;
                    Buf  :        System.Address;
                    Len  : in out UInt32);

   procedure Write (This : in out Default_Serial_Class;
                    UDC  : in out USB_Device_Controller'Class;
                    Str  :        String;
                    Len  :    out UInt32);

private

   type Default_Serial_Class (TX_Buffer_Size, RX_Buffer_Size : BBqueue.Count)
   is limited new USB_Device_Class with record
      Interface_Index : Interface_Id;
      Int_EP          : USB.EP_Id;
      Bulk_EP         : USB.EP_Id;
      Iface_Str       : USB.String_Id := Invalid_String_Id;

      Int_Buf         : System.Address := System.Null_Address;
      Bulk_Out_Buf    : System.Address := System.Null_Address;
      Bulk_In_Buf     : System.Address := System.Null_Address;

      TX_Queue : BBqueue.Buffers.Buffer (TX_Buffer_Size);
      RX_Queue : BBqueue.Buffers.Buffer (RX_Buffer_Size);

      TX_In_Progress : aliased Atomic.Flag := Atomic.Init (False);

      Coding : CDC_Line_Coding;
      State  : CDC_Line_Control_State;
   end record;

   procedure Setup_RX (This : in out Default_Serial_Class;
                       UDC  : in out USB_Device_Controller'Class);

   procedure Setup_TX (This : in out Default_Serial_Class;
                       UDC  : in out USB_Device_Controller'Class);

   overriding
   function Initialize (This                 : in out Default_Serial_Class;
                        Dev                  : in out USB_Device_Stack'Class;
                        Base_Interface_Index :        Interface_Id)
                        return Init_Result;

   overriding
   procedure Get_Class_Info
     (This                     : in out Default_Serial_Class;
      Number_Of_Interfaces     :    out Interface_Id;
      Config_Descriptor_Length :    out Natural);

   overriding
   procedure Fill_Config_Descriptor (This : in out Default_Serial_Class;
                                     Data :    out UInt8_Array);
   overriding
   function Configure (This  : in out Default_Serial_Class;
                       UDC   : in out USB_Device_Controller'Class;
                       Index : UInt16)
                       return Setup_Request_Answer;

   overriding
   function Setup_Read_Request (This  : in out Default_Serial_Class;
                                Req   : Setup_Data;
                                Buf   : out System.Address;
                                Len   : out Buffer_Len)
                                return Setup_Request_Answer;

   overriding
   function Setup_Write_Request (This  : in out Default_Serial_Class;
                                 Req   : Setup_Data;
                                 Data  : UInt8_Array)
                                 return Setup_Request_Answer;

   overriding
   procedure Transfer_Complete (This : in out Default_Serial_Class;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   :        EP_Addr;
                                CNT  :        Packet_Size);

end USB.Device.Serial;
