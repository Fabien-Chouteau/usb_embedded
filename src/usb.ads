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

with System.Storage_Elements;
with HAL; use HAL;

with Usb_Embedded_Config;

private with System;

package USB is

   type Packet_Size is range 0 .. 1024;
   subtype Control_Packet_Size is Packet_Size range 0 .. 64;

   subtype EP_Id is UInt4;

   type EP_Dir is (EP_In, EP_Out);

   type EP_Addr is record
      Num : EP_Id;
      Dir : EP_Dir;
   end record;

   type EP_Type is (Control, Isochronous, Bulk, Interrupt);
   for EP_Type use (Control     => 0,
                    Isochronous => 1,
                    Bulk        => 2,
                    Interrupt   => 3);

   type Data_Phase_Transfer_Direction is (Host_To_Device,
                                          Device_To_Host)
     with Size => 1;

   for Data_Phase_Transfer_Direction use (Host_To_Device => 0,
                                          Device_To_Host => 1);

   type Request_Type_Type is (Stand, Class, Vendor, Reserved)
     with Size => 2;
   for Request_Type_Type use (Stand    => 0,
                              Class    => 1,
                              Vendor   => 2,
                              Reserved => 3);

   type Request_Type_Recipient is (Dev, Iface, Endpoint, Other);
   for Request_Type_Recipient use (Dev      => 0,
                                   Iface    => 1,
                                   Endpoint => 2,
                                   Other    => 3);
   type Request_Type is record
      Recipient : Request_Type_Recipient;
      Reserved  : UInt3;
      Typ : Request_Type_Type;
      Dir : Data_Phase_Transfer_Direction;
   end record with Pack, Size => 8;

   type Stand_Request_Type is
     (Req_Get_Status, Req_Clear_Feature, Req_Set_Feature, Req_Set_Address,
      Req_Get_Descriptor, Req_Set_Descriptor, Req_Get_Configuration,
      Req_Set_Configuration, Req_Sync_Feature);
   for Stand_Request_Type use (Req_Get_Status        => 16#00#,
                               Req_Clear_Feature     => 16#01#,
                               Req_Set_Feature       => 16#03#,
                               Req_Set_Address       => 16#05#,
                               Req_Get_Descriptor    => 16#06#,
                               Req_Set_Descriptor    => 16#07#,
                               Req_Get_Configuration => 16#08#,
                               Req_Set_Configuration => 16#09#,
                               Req_Sync_Feature      => 16#12#);

   type Setup_Data is record
      RType   : Request_Type;
      Request : UInt8;
      Value   : UInt16;
      Index   : UInt16;
      Length  : UInt16;
   end record with Pack, Size => 8 * 8;

   function Img (D : Setup_Data) return String
   is ("Type: (" & D.RType.Dir'Img & "," & D.RType.Typ'Img & "," &
         D.RType.Recipient'Img & ")" &
         " Req:" & D.Request'Img &
         " Val:" & D.Value'Img &
         " Index:" & D.Index'Img &
         " Len:" & D.Length'Img);

   function Img (EP : EP_Addr) return String
   is ("["  & EP.Dir'Img & EP.Num'Img & "]");

   type String_Id is new UInt8;
   Invalid_String_Id : constant String_Id := 0;

   type Lang_ID is new UInt16;
   type Interface_Id is new UInt8;

   type Descriptor_Type is
     (Dt_Device, Dt_Configuration, Dt_String, Dt_Interface, Dt_Endpoint,
      Dt_Qualifier, Dt_Interface_Associate, Dt_Cs_Interface);
   for Descriptor_Type use (Dt_Device              => 16#01#,
                            Dt_Configuration       => 16#02#,
                            Dt_String              => 16#03#,
                            Dt_Interface           => 16#04#,
                            Dt_Endpoint            => 16#05#,
                            Dt_Qualifier           => 16#06#,
                            Dt_Interface_Associate => 16#0B#,
                            Dt_Cs_Interface        => 16#24#);

   type Device_Descriptor is record
      bLength            : UInt8;
      bDescriptorType    : UInt8;
      bcdUSB             : UInt16;
      bDeviceClass       : UInt8;
      bDeviceSubClass    : UInt8;
      bDeviceProtocol    : UInt8;
      bMaxPacketSize0    : UInt8;
      idVendor           : UInt16;
      idProduct          : UInt16;
      bcdDevice          : UInt16;
      iManufacturer      : String_Id;
      iProduct           : String_Id;
      iSerialNumber      : String_Id;
      bNumConfigurations : UInt8;
   end record with Pack;

   type Device_Qualifier is record
      bLength            : UInt8;
      bDescriptorType    : UInt8;
      bcdUSB             : UInt16;
      bDeviceClass       : UInt8;
      bDeviceSubClass    : UInt8;
      bDeviceProtocol    : UInt8;
      bMaxPacketSize0    : UInt8;
      bNumConfigurations : UInt8;
      bReserved          : UInt8;
   end record with Pack;

   type String_Descriptor_Zero is record
      bLength            : UInt8;
      bDescriptorType    : UInt8 := 3;
      Str                : String (1 .. 2);
   end record;

   subtype String_Range is UInt8 range 0 .. 253;
   --  The maximum length of a string is limited by the the bLength field of
   --  the String Descriptor. This field is one byte: 0 .. 255, but bLength
   --  encodes to total size of the descriptor including the bLenght and
   --  bDescriptorType fields (one byte each). So the remaining length for
   --  string is 255 - 2.

   type USB_String is array (String_Range range <>) of Character;

   function To_USB_String (Str : String) return USB_String
     with Pre => Str'Length * 2 <= String_Range'Last - String_Range'First + 1;
   --  Convert Ada ASCII string to USB UTF16 string

   type String_Descriptor (bLength : UInt8) is record
      bDescriptorType    : UInt8 := 3;
      Str                : USB_String (3 .. bLength);
   end record with Pack;

   type Setup_Request_Answer is (Handled, Not_Supported, Next_Callback);

   subtype Buffer_Len is System.Storage_Elements.Storage_Offset;

   Verbose : constant Boolean := False;

   Control_Buffer_Size : constant := Usb_Embedded_Config.Control_Buffer_Size;
   Max_Strings : constant := Usb_Embedded_Config.Max_Strings;
   Max_Total_String_Chars : constant := Usb_Embedded_Config.String_Buffer_Size;
end USB;
