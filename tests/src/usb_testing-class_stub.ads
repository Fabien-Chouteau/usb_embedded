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

--  USB Device Class stub for testing

with System;

with HAL;            use HAL;
with USB;
with USB.Device;     use USB.Device;
with USB.HAL.Device; use USB.HAL.Device;
with USB_Testing.Output; use USB_Testing.Output;

package USB_Testing.Class_Stub is

   type Device_Class_Stub (Output : not null Text_Output_Acc;
                           Number : Positive)
   is new USB.Device.USB_Device_Class
   with private;

   overriding
   function Initialize (This            : in out Device_Class_Stub;
                        Dev             : in out USB_Device_Stack'Class;
                        Interface_Index :        USB.Interface_Id)
                        return Init_Result;

   overriding
   procedure Get_Class_Info
     (This                     : in out Device_Class_Stub;
      Number_Of_Interfaces     :    out USB.Interface_Id;
      Config_Descriptor_Length :    out Natural);

   overriding
   procedure Fill_Config_Descriptor (This : in out Device_Class_Stub;
                                     Data :    out UInt8_Array);

   overriding
   function Configure (This  : in out Device_Class_Stub;
                       UDC   : in out USB_Device_Controller'Class;
                       Index : UInt16)
                       return USB.Setup_Request_Answer;

   overriding
   function Setup_Read_Request (This  : in out Device_Class_Stub;
                                Req   : USB.Setup_Data;
                                Buf   : out System.Address;
                                Len   : out USB.Buffer_Len)
                                return USB.Setup_Request_Answer;

   overriding
   function Setup_Write_Request (This  : in out Device_Class_Stub;
                                 Req   : USB.Setup_Data;
                                 Data  : UInt8_Array)
                                 return USB.Setup_Request_Answer;

   overriding
   procedure Transfer_Complete (This : in out Device_Class_Stub;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   :        USB.EP_Addr;
                                CNT  :        USB.Packet_Size);

private

   type Device_Class_Stub (Output : not null Text_Output_Acc;
                           Number : Positive)
   is new USB.Device.USB_Device_Class with record
      Interface_Index : USB.Interface_Id;
      Ep : USB.EP_Id;
   end record;

end USB_Testing.Class_Stub;
