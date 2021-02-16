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

with Interfaces;

package USB.Device.HID is

   type Default_HID_Class is new USB_Device_Class with private;

   overriding
   procedure Initialize (This            : in out Default_HID_Class;
                         Dev             : in out USB_Device;
                         Interface_Index :        Class_Index);
   overriding
   function Config_Descriptor_Length (This : in out Default_HID_Class)
                                      return Positive;

   overriding
   procedure Fill_Config_Descriptor (This : in out Default_HID_Class;
                                     Data :    out UInt8_Array);
   overriding
   function Configure (This  : in out Default_HID_Class;
                       UDC   : in out USB_Device_Controller'Class;
                       Index : UInt16)
                       return Setup_Request_Answer;

   overriding
   function Setup_Read_Request (This  : in out Default_HID_Class;
                                Req   : Setup_Data;
                                Buf   : out System.Address;
                                Len   : out Buffer_Len)
                                return Setup_Request_Answer;

   overriding
   function Setup_Write_Request (This  : in out Default_HID_Class;
                                 Req   : Setup_Data;
                                 Data  : UInt8_Array)
                                 return Setup_Request_Answer;

   overriding
   procedure Transfer_Complete (This : in out Default_HID_Class;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   : EP_Addr);

   overriding
   procedure Data_Ready (This : in out Default_HID_Class;
                         UDC  : in out USB_Device_Controller'Class;
                         EP   : EP_Id;
                         BCNT : UInt32);

   procedure Set_Move (This : in out Default_HID_Class;
                       X, Y : Interfaces.Integer_8);

   procedure Send_Report (This : in out Default_HID_Class;
                          UDC  : in out USB_Device_Controller'Class);

   function Ready (This : in out Default_HID_Class) return Boolean;

private

   type Class_State is (Stop, Idle, Busy);

   type Default_HID_Class is new USB_Device_Class with record
      Interface_Index : Class_Index;
      EP              : USB.EP_Id;
      Report          : UInt8_Array (1 .. 3) := (others => 0);
      State           : Class_State := Stop;
      Idle_State      : UInt8 := 0;
   end record;

end USB.Device.HID;
