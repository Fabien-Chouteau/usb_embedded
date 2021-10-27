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

with HAL;
with AAA.Strings;

private with Ada.Strings.Unbounded;

package USB_Testing.Output is

   type Text_Output is tagged private;

   type Text_Output_Acc is access all Text_Output;

   procedure New_Line (This : in out Text_Output);

   procedure Put (This : in out Text_Output;
                  Str  :        String);

   procedure Put_Line (This : in out Text_Output;
                       Str  :        String);

   procedure Hex_Dump (This : in out Text_Output;
                       Data      : HAL.UInt8_Array;
                       Base_Addr : HAL.UInt64 := 0);

   function Dump (This : Text_Output) return AAA.Strings.Vector;

   function Equal (Expected, Actual : AAA.Strings.Vector) return Boolean;

   function Diff (A, B        : AAA.Strings.Vector;
                  A_Name      : String := "A";
                  B_Name      : String := "B";
                  Skip_Header : Boolean := False)
                  return AAA.Strings.Vector;
private

   type Text_Output is tagged record
      Current_Line : Ada.Strings.Unbounded.Unbounded_String;
      Vect         : AAA.Strings.Vector;
   end record;

end USB_Testing.Output;
