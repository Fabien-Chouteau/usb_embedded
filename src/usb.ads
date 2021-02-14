------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

with HAL.USB.Device; use HAL.USB.Device;
with HAL;            use HAL;
with System.Storage_Elements;

private with System;
private with HAL.USB;

package USB is

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
      iManufacturer      : UInt8;
      iProduct           : UInt8;
      iSerialNumber      : UInt8;
      bNumConfigurations : UInt8;
   end record with Pack;

   type String_Descriptor_Zero is record
      bLength            : UInt8;
      bDescriptorType    : UInt8 := 3;
      Str                : String (1 .. 2);
   end record;

   type USB_String is array (UInt8 range <>) of Character;

   type String_Descriptor (bLength : UInt8) is record
      bDescriptorType    : UInt8 := 3;
      Str                : USB_String (3 .. bLength);
   end record with Pack;

   type String_Rec is record
      Index  : UInt8;
      Str    : not null access constant String_Descriptor;
   end record;

   type String_Array is array (Natural range <>) of String_Rec;

   type Setup_Request_Answer is (Handled, Not_Supported, Next_Callback);

   subtype Buffer_Len is System.Storage_Elements.Storage_Offset;

   -- Device Class Interface --

   type USB_Device_Class is interface;
   type Any_USB_Device_Class is access all USB_Device_Class'Class;

   function Configure (This  : in out USB_Device_Class;
                       UDC   : in out USB_Device_Controller'Class;
                       Index : UInt16)
                       return Setup_Request_Answer
   is abstract;

   function Setup_Read_Request (This  : in out USB_Device_Class;
                                Req   : HAL.USB.Setup_Data;
                                Buf   : out System.Address;
                                Len   : out Buffer_Len)
                                return Setup_Request_Answer
   is abstract;

   function Setup_Write_Request (This  : in out USB_Device_Class;
                                 Req   : HAL.USB.Setup_Data;
                                 Data  : UInt8_Array)
                                 return Setup_Request_Answer
   is abstract;

   procedure Transfer_Complete (This : in out USB_Device_Class;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   : HAL.USB.EP_Addr)
   is abstract;

   procedure Data_Ready (This : in out USB_Device_Class;
                         UDC  : in out USB_Device_Controller'Class;
                         EP   : HAL.USB.EP_Id;
                         BCNT : UInt32)
   is abstract;

   -- Device --

   type USB_Device is tagged private;

   function Initialized (This : USB_Device) return Boolean;

   procedure Initalize (This       : in out USB_Device;
                        Controller : not null Any_USB_Device_Controller;
                        Class      : not null Any_USB_Device_Class;
                        Dec        : not null access constant Device_Descriptor;
                        Config     : not null access constant UInt8_Array;
                        Strings    : not null access constant String_Array)
     with Post => This.Initialized;

   procedure Start (This : in out USB_Device)
     with Pre => This.Initialized;

   procedure Reset (This : in out USB_Device)
     with Pre => This.Initialized;

   procedure Poll (This : in out USB_Device)
     with Pre => This.Initialized;

   function Controller (This : USB_Device)
                        return not null Any_USB_Device_Controller
     with Pre => This.Initialized;

private

   type Control_State is (Idle,

                          --  In means Device to Host
                          Data_In,
                          Last_Data_In,
                          Status_In,

                          --  Out means Host to Device
                          Data_Out,
                          Last_Data_Out,
                          Status_Out);

   type Device_State is (Idle, Addressed, Configured, Suspended);

   type USB_Device is tagged record

      --  For better performances this buffer has to be word aligned. So we put
      --  it as the first field of this record.
      RX_Ctrl_Buf : UInt8_Array (1 .. 256);

      UDC     : Any_USB_Device_Controller := null;
      Class   : Any_USB_Device_Class := null;
      Desc    : access constant Device_Descriptor := null;
      Config  : access constant UInt8_Array := null;
      Strings : access constant String_Array := null;

      Dev_Addr  : UInt7 := 0;
      Dev_State : Device_State := Idle;

      Ctrl_Req : HAL.USB.Setup_Data;
      Ctrl_Buf : System.Address;
      Ctrl_Len : Buffer_Len := 0;
      Ctrl_State : Control_State := Idle;
      Ctrl_Need_ZLP : Boolean := False;

   end record;

end USB;
