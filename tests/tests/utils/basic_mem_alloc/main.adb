with Ada.Text_IO; use Ada.Text_IO;
with USB.Utils; use USB.Utils;
with System.Storage_Elements; use System.Storage_Elements;
with HAL; use HAL;

procedure Main is

   Alloc : USB.Utils.Basic_RAM_Allocator (256);

   procedure Test (Alignment : UInt8; Len : UInt11) is
      Addr : constant Integer_Address :=
     To_Integer (Allocate (Alloc, Alignment, Len));
   begin
      if Addr = 0 then
         Put ("Allocation failed");
      elsif (Addr mod Integer_Address (Alignment)) /= 0 then
         Put ("Bad alignment");
      else
         Put ("OK");
      end if;

      Put_Line (" - Align:" & Alignment'Img & " Len:" & Len'Img);
   end Test;
begin

   Test (1, 1);
   Test (2, 1);
   Test (4, 1);
   Test (8, 1);
   Test (16, 1);
   Test (32, 1);
   Test (4, 512);

end Main;
