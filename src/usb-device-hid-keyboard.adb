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

package body USB.Device.HID.Keyboard is

   ------------------
   -- Set_Modifier --
   ------------------

   procedure Set_Modifier (This  : in out Instance;
                           M     : Modifiers;
                           Value : Boolean := True)
   is
      Mods : UInt8 renames This.Report (This.Report'First);
   begin
      if Value then
         Mods := Mods or M'Enum_Rep;
      else
         Mods := Mods and (not M'Enum_Rep);
      end if;
   end Set_Modifier;

   --------------------
   -- Key_Codes_Full --
   --------------------

   function Key_Codes_Full (This : Instance) return Boolean
   is (This.Key_Code_Index = Max_Key_Codes);

   -------------------
   -- Push_Key_Code --
   -------------------

   procedure Push_Key_Code (This : in out Instance;
                            Code : UInt8)
   is
   begin
      if This.Key_Codes_Full then
         return;
      end if;

      This.Report (This.Report'First + 2 + This.Key_Code_Index) := Code;
      This.Key_Code_Index := This.Key_Code_Index + 1;
   end Push_Key_Code;

   -----------
   -- Is_On --
   -----------

   function Is_On (This : Instance; L : LEDs) return Boolean
   is ((This.LEDs and L'Enum_Rep) /= 0);

   -----------------
   -- Send_Report --
   -----------------

   overriding
   procedure Send_Report (This : in out Instance;
                          UDC  : in out USB_Device_Controller'Class)
   is
   begin
      Parent (This).Send_Report (UDC);

      This.Key_Code_Index := 0;
   end Send_Report;

   ----------------
   -- Set_Report --
   ----------------

   overriding
   function Set_Report (This : in out Instance;
                        Typ  :        UInt8;
                        ID   :        UInt8;
                        Data :        UInt8_Array)
                        return Setup_Request_Answer
   is
   begin
      if Data'Length /= 1
        or else
          Typ /= 2
        or else
          ID /= 0
      then
         return Not_Supported;
      end if;

      This.LEDs := Data (Data'First);
      return Handled;
   end Set_Report;
end USB.Device.HID.Keyboard;
