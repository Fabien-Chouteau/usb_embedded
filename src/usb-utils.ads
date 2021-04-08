with HAL;

package USB.Utils is

   procedure Copy (Src, Dst : System.Address; Count : Natural);
   procedure Copy (Src, Dst : System.Address; Count : HAL.UInt32);
   procedure Copy (Src, Dst : System.Address; Count : HAL.UInt11);

   pragma Inline (Copy);

   --  Basic_RAM_Allocator --

   type Basic_RAM_Allocator (Size : Positive) is private;

   function Allocate (This      : in out Basic_RAM_Allocator;
                      Alignment :        UInt8;
                      Len       :        UInt11)
                      return System.Address;

private

   type Basic_RAM_Allocator (Size : Positive) is record
      Buffer : UInt8_Array (1 .. Size);
      Top : Natural := 1;
   end record;

   procedure Align_Top (This      : in out Basic_RAM_Allocator;
                        Alignment :        UInt8);

end USB.Utils;
