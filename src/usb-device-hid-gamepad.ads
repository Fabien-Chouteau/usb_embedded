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

package USB.Device.HID.Gamepad is

   Gamepad_Report_Size : constant := 7;

   subtype Parent is Abstract_HID_Class;

   type Instance
   is new Parent (Gamepad_Report_Size)
   with private;

   type Axis is (X, Y, Z, Rx, Ry, Rz);

   procedure Set_Axis (This  : in out Instance;
                       A     : Axis;
                       Value : Interfaces.Integer_8);
   --  Set value of stick axis

   procedure Set_Buttons (This : in out Instance;
                          Buttons : UInt8);
   --  Set the buttons state

private

   type Instance
   is new Abstract_HID_Class (Gamepad_Report_Size)
   with null record;

   HID_Gamepad_Report_Desc : aliased constant UInt8_Array :=
     (
      --  https://gist.github.com/wereii/b215c799a267af10154087b5d5af3a6b
      16#05#, 16#01#, --  USAGE_PAGE (Generic Desktop)
      16#09#, 16#05#, --  USAGE (Game pad)
      16#a1#, 16#01#, --  COLLECTION (Application)
      16#09#, 16#01#, --    USAGE (Pointer)
      16#a1#, 16#00#, --    COLLECTION (Physical)

      --  Sticks
      --  16#05#, 16#01#, --      USAGE_PAGE (Generic Desktop)
      16#09#, 16#30#, --      USAGE (X)
      16#09#, 16#31#, --      USAGE (Y)
      16#09#, 16#32#, --      USAGE (Z)
      16#09#, 16#33#, --      USAGE (Rx)
      16#09#, 16#34#, --      USAGE (Ry)
      16#09#, 16#35#, --      USAGE (Rz)
      16#15#, 16#81#, --      LOGICAL_MINIMUM (-127)
      16#25#, 16#7f#, --      LOGICAL_MAXIMUM (127)
      16#95#, 16#06#, --      REPORT_COUNT (6)
      16#75#, 16#08#, --      REPORT_SIZE (8)
      16#81#, 16#02#, --      INPUT (Data,Var,Abs)

      --  Buttons
      16#05#, 16#09#, --      USAGE_PAGE (Button)
      16#19#, 16#01#, --      USAGE_MINIMUM (Button 1)
      16#29#, 16#08#, --      USAGE_MAXIMUM (Button 8)
      16#15#, 16#00#, --      LOGICAL_MINIMUM (0)
      16#25#, 16#01#, --      LOGICAL_MAXIMUM (1)
      16#75#, 16#01#, --      REPORT_SIZE (1)
      16#95#, 16#08#, --      REPORT_COUNT (8)
      16#81#, 16#02#, --      INPUT (Data,Var,Abs)
      16#c0#,         --    END_COLLECTION
      16#c0#          --  END_COLLECTION
     );

   overriding
   function Report_Descriptor (This : Instance)
                               return not null Report_Descriptor_Access
   is (HID_Gamepad_Report_Desc'Access);

end USB.Device.HID.Gamepad;
