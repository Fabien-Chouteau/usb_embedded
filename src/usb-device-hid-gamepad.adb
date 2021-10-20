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

with Ada.Unchecked_Conversion;

package body USB.Device.HID.Gamepad is

   --------------
   -- Set_Axis --
   --------------

   procedure Set_Axis (This  : in out Instance;
                       A     : Axis;
                       Value : Interfaces.Integer_8)
   is
      function To_UInt8 is new Ada.Unchecked_Conversion (Interfaces.Integer_8,
                                                         UInt8);

   begin
      This.Report (This.Report'First + (case A is
                      when X => 0,
                      when Y => 1,
                      when Z => 2,
                      when Rx => 3,
                      when Ry => 4,
                      when Rz => 5)) := To_UInt8 (Value);
   end Set_Axis;

   -----------------
   -- Set_Buttons --
   -----------------

   procedure Set_Buttons (This : in out Instance;
                          Buttons : UInt8)
   is
   begin
      This.Report (This.Report'Last) := Buttons;
   end Set_Buttons;

end USB.Device.HID.Gamepad;
