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

package USB.Utils is

   function High (V : UInt16) return UInt8
   is (UInt8 (Shift_Right (V, 8) and 16#FF#));

   function Low (V : UInt16) return UInt8
   is (UInt8 (V and 16#FF#));

   procedure Copy (Src, Dst : System.Address; Count : Natural);
   procedure Copy (Src, Dst : System.Address; Count : HAL.UInt32);
   procedure Copy (Src, Dst : System.Address; Count : Packet_Size);

   pragma Inline (Copy);

   --  Basic_RAM_Allocator --

   type Basic_RAM_Allocator (Size : Positive) is private;

   function Allocate (This      : in out Basic_RAM_Allocator;
                      Alignment :        UInt8;
                      Len       :        Packet_Size)
                      return System.Address;

private

   type Basic_RAM_Allocator (Size : Positive) is record
      Buffer : UInt8_Array (1 .. Size);
      Top : Natural := 1;
   end record;

   procedure Align_Top (This      : in out Basic_RAM_Allocator;
                        Alignment :        UInt8);

end USB.Utils;
