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

with System.Storage_Elements; use System.Storage_Elements;

package body USB.Utils is

   ----------
   -- Copy --
   ----------

   procedure Copy (Src, Dst : System.Address; Count : Natural) is
      Src_Arr : UInt8_Array (1 .. Count) with Address => Src;
      Dst_Arr : UInt8_Array (1 .. Count) with Address => Dst;
   begin
      Dst_Arr := Src_Arr;
   end Copy;

   ----------
   -- Copy --
   ----------

   procedure Copy (Src, Dst : System.Address; Count : HAL.UInt32) is
   begin
      Copy (Src, Dst, Natural (Count));
   end Copy;

   ----------
   -- Copy --
   ----------

   procedure Copy (Src, Dst : System.Address; Count : Packet_Size) is
   begin
      Copy (Src, Dst, Natural (Count));
   end Copy;

   --------------
   -- Allocate --
   --------------

   function Allocate (This      : in out Basic_RAM_Allocator;
                      Alignment :        UInt8;
                      Len       :        Packet_Size)
                      return System.Address
   is
   begin

      --  First align the Top pointer according to requested alignment
      Align_Top (This, Alignment);

      --  Check if top is within range, otherwise there is no memory left
      if This.Top not in This.Buffer'Range then
         return System.Null_Address;
      end if;

      declare
         New_Top : constant Natural := This.Top + Natural (Len);
         Ret : System.Address;
      begin

         --  Check if allocation fits in the buffer
         if New_Top - 1 > This.Buffer'Last then
            return System.Null_Address;
         end if;

         Ret := This.Buffer (This.Top)'Address;
         This.Top := New_Top;

         return Ret;
      end;
   end Allocate;

   ---------------
   -- Align_Top --
   ---------------

   procedure Align_Top (This      : in out Basic_RAM_Allocator;
                        Alignment :        UInt8)
   is
   begin

      --  Top is already outside of buffer range. There is no memory available
      --  anymore.
      if This.Top not in This.Buffer'Range then
         return;
      end if;

      declare
         Addr  : constant System.Address := This.Buffer (This.Top)'Address;
         Int   : constant Integer_Address := To_Integer (Addr);
         Align : constant Integer_Address := Integer_Address (Alignment);
         Padding : constant Integer_Address :=
           (Align - (Int mod Align)) mod Align;
      begin

         --  This may result in a Top that is outside the range of allocated
         --  buffer.
         This.Top := This.Top + Natural (Padding);
      end;
   end Align_Top;

end USB.Utils;
