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

package USB.Device.HID.Keyboard is

   Max_Key_Codes : constant := 6;
   Keyboard_Report_Size : constant := Max_Key_Codes + 2;

   subtype Parent is Abstract_HID_Class;
   type Instance
   is new Parent (Keyboard_Report_Size)
   with private;

   type Modifiers is (Ctrl_Left, Shift_Left, Alt_Left, Meta_Left,
                      Ctrl_Right, Shift_Right, Alt_Right, Meta_Right);

   procedure Set_Modifier (This  : in out Instance;
                           M     : Modifiers;
                           Value : Boolean := True);

   function Key_Codes_Full (This : Instance) return Boolean;
   --  Return True if there is no more room for keycode in the report
   --  (Max_Key_Codes).

   procedure Push_Key_Code (This : in out Instance;
                            Code : UInt8)
     with Pre => not Key_Codes_Full (This);
   --  Push keycode in the report, if not full.
   --
   --  Upon return from the Send_Report procedure, the stack is cleared.

   type LEDs is (Num_Lock, Caps_Lock, Scroll_Lock, Compose, Kana);

   function Is_On (This : Instance; L : LEDs) return Boolean;
   --  Return True if the Host request the given LED to be lit

private

   for Modifiers'Size use 8;
   for Modifiers use (Ctrl_Left   => 2#0000_0001#,
                      Shift_Left  => 2#0000_0010#,
                      Alt_Left    => 2#0000_0100#,
                      Meta_Left   => 2#0000_1000#,
                      Ctrl_Right  => 2#0001_0000#,
                      Shift_Right => 2#0010_0000#,
                      Alt_Right   => 2#0100_0000#,
                      Meta_Right  => 2#1000_0000#);

   for LEDs use (Num_Lock    => 2#0_0001#,
                 Caps_Lock   => 2#0_0010#,
                 Scroll_Lock => 2#0_0100#,
                 Compose     => 2#0_1000#,
                 Kana        => 2#1_0000#);

   type Instance
   is new Abstract_HID_Class (Keyboard_Report_Size)
   with record
      Key_Code_Index : Packet_Size := 0;
      LEDs : UInt8 := 0;
   end record;

   overriding
   procedure Send_Report (This : in out Instance;
                          UDC  : in out USB_Device_Controller'Class)
     with Pre => This.Ready;
   --  We need a special Send_Report implementation here to clear the key codes
   --  stack.

   ----------------
   -- Set_Report --
   ----------------

   overriding
   function Set_Report (This : in out Instance;
                        Typ  :        UInt8;
                        ID   :        UInt8;
                        Data :        UInt8_Array)
                        return Setup_Request_Answer;
   --  This procedure will receive LEDs status report from the host

   HID_Keyboard_Report_Desc : aliased constant UInt8_Array :=
     (
      16#05#, 16#01#,   -- USAGE_PAGE (Generic Desktop)
      16#09#, 16#06#,   -- USAGE (Keyboard)
      16#a1#, 16#01#,   -- COLLECTION (Application)
      16#05#, 16#07#,   --   USAGE_PAGE (Keyboard)
      --  16#85#, 16#01#,   --   REPORT_ID (1)

      --  8 bits Modifier Keys (Shfit, Control, Alt)
      16#19#, 16#e0#,   --   USAGE_MINIMUM (kbd LeftControl)
      16#29#, 16#e7#,   --   USAGE_MAXIMUM (kbd Right Meta)
      16#15#, 16#00#,   --   LOGICAL_MINIMUM (0)
      16#25#, 16#01#,   --   LOGICAL_MAXIMUM (1)
      16#95#, 16#08#,   --   REPORT_COUNT (8)
      16#75#, 16#01#,   --   REPORT_SIZE (1)
      16#81#, 16#02#,   --   INPUT (Data,Var,Abs)

      --  Reserved byte (Don't why this is here, but it looks like all
      --  keyboard report have it...)
      16#95#, 16#01#,   --   REPORT_COUNT (1)
      16#75#, 16#08#,   --   REPORT_SIZE (8)
      16#81#, 16#01#,   --   INPUT (Cnst,Ary,Abs)

      --  6-byte Keycodes
      16#05#, 16#07#,   --   USAGE_PAGE (Keyboard)
      16#19#, 16#00#,   --   USAGE_MINIMUM (Reserved (no event indicated))
      16#29#, 16#65#,   --   USAGE_MAXIMUM (Keyboard Application)
      16#15#, 16#00#,   --   LOGICAL_MINIMUM (0)
      16#25#, 16#65#,   --   LOGICAL_MAXIMUM (101)
      16#95#, Max_Key_Codes,   --   REPORT_COUNT (6)
      16#75#, 16#08#,   --   REPORT_SIZE (8)
      16#81#, 16#00#,   --   INPUT (Data,Ary,Abs)

      --  5-bit LED Indicator:
      --  Kana | Compose | ScrollLock | CapsLock | NumLock
      16#05#, 16#08#,   --   USAGE_PAGE (LEDs)
      16#19#, 16#01#,   --   USAGE_MINIMUM (Num Lock)
      16#29#, 16#05#,   --   USAGE_MAXIMUM (Kana)
      16#95#, 16#05#,   --   REPORT_COUNT (5)
      16#75#, 16#01#,   --   REPORT_SIZE (1)
      --  16#85#, 16#01#,   --   REPORT_ID (1)
      16#91#, 16#02#,   --   OUTPUT (Data,Var,Abs)

      --  LED Padding
      16#95#, 16#01#,   --   REPORT_COUNT (1)
      16#75#, 16#03#,   --   REPORT_SIZE (3)
      16#91#, 16#03#,   --   OUTPUT (Cnst,Var,Abs)
      16#c0#          --  END_COLLECTION
     );

   overriding
   function Report_Descriptor (This : Instance)
                               return not null Report_Descriptor_Access
   is (HID_Keyboard_Report_Desc'Access);

end USB.Device.HID.Keyboard;
