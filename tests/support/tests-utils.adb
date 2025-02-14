with System.Storage_Elements; use System.Storage_Elements;

with AUnit.Assertions; use AUnit.Assertions;

with HAL; use HAL;

with USB.Utils; use USB.Utils;

package body Tests.Utils is

   ---------------------
   -- Basic_Mem_Alloc --
   ---------------------

   procedure Basic_Mem_Alloc (Unused      : in out Null_Fixture)
   is
      Alloc : USB.Utils.Basic_RAM_Allocator (256);

      procedure Test (Alignment   : UInt8;
                      Len         : USB.Packet_Size;
                      Expect_Fail : Boolean)
      is
         Addr : constant Integer_Address :=
           To_Integer (Allocate (Alloc, Alignment, Len));
      begin
         Assert (Addr /= 0 or else Expect_Fail, "Allocation failed");
         Assert ((Addr mod Integer_Address (Alignment)) = 0, "Bad alignment");
      end Test;
   begin
      Test (1, 1, False);
      Test (2, 1, False);
      Test (4, 1, False);
      Test (8, 1, False);
      Test (16, 1, False);
      Test (32, 1, False);
      Test (4, 512, True);
   end Basic_Mem_Alloc;

begin
   Suite.Add_Test (Null_Caller.Create ("Basic Mem Alloc", Basic_Mem_Alloc'Access));
end Tests.Utils;
