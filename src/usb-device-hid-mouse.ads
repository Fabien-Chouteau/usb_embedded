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

with Interfaces;

package USB.Device.HID.Mouse is

   Mouse_Report_Size : constant := 3;

   type Instance
   is new Abstract_HID_Class (Mouse_Report_Size)
   with private;

   procedure Set_Move (This : in out Instance;
                       X, Y : Interfaces.Integer_8);
   --  Set the relative movement of the mouse cursor

   procedure Set_Click (This : in out Instance;
                        Btn1, Btn2, Btn3 : Boolean := False);
   --  Set the mouse buttons state

private

   type Instance
   is new Abstract_HID_Class (Mouse_Report_Size)
   with null record;

   HID_Mouse_Report_Desc : aliased constant UInt8_Array :=
     (
      --  https://eleccelerator.com/tutorial-about-usb-hid-report-descriptors/
      16#05#, 16#01#, --  USAGE_PAGE (Generic Desktop)
      16#09#, 16#02#, --  USAGE (Mouse)
      16#a1#, 16#01#, --  COLLECTION (Application)
      16#09#, 16#01#, --    USAGE (Pointer)
      16#a1#, 16#00#, --    COLLECTION (Physical)
      16#05#, 16#09#, --      USAGE_PAGE (Button)
      16#19#, 16#01#, --      USAGE_MINIMUM (Button 1)
      16#29#, 16#03#, --      USAGE_MAXIMUM (Button 3)
      16#15#, 16#00#, --      LOGICAL_MINIMUM (0)
      16#25#, 16#01#, --      LOGICAL_MAXIMUM (1)
      16#95#, 16#03#, --      REPORT_COUNT (3)
      16#75#, 16#01#, --      REPORT_SIZE (1)
      16#81#, 16#02#, --      INPUT (Data,Var,Abs)
      16#95#, 16#01#, --      REPORT_COUNT (1)
      16#75#, 16#05#, --      REPORT_SIZE (5)
      16#81#, 16#03#, --      INPUT (Cnst,Var,Abs)
      16#05#, 16#01#, --      USAGE_PAGE (Generic Desktop)
      16#09#, 16#30#, --      USAGE (X)
      16#09#, 16#31#, --      USAGE (Y)
      16#15#, 16#81#, --      LOGICAL_MINIMUM (-127)
      16#25#, 16#7f#, --      LOGICAL_MAXIMUM (127)
      16#75#, 16#08#, --      REPORT_SIZE (8)
      16#95#, 16#02#, --      REPORT_COUNT (2)
      16#81#, 16#06#, --      INPUT (Data,Var,Rel)
      16#c0#,         --    END_COLLECTION
      16#c0#          --  END_COLLECTION
     );

   overriding
   function Report_Descriptor (This : Instance)
                               return not null Report_Descriptor_Access
   is (HID_Mouse_Report_Desc'Access);

end USB.Device.HID.Mouse;
