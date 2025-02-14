with HAL; use HAL;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Containers.Bounded_Vectors;

package body USB_Testing.Output is

   procedure New_Line (This : in out Text_Output) is
   begin
      This.Vect.Append (To_String (This.Current_Line));
      This.Current_Line := To_Unbounded_String ("");
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (This : in out Text_Output; Str : String) is
   begin
      Append (This.Current_Line, Str);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (This : in out Text_Output; Str : String) is
   begin
      This.Put (Str);
      This.New_Line;
   end Put_Line;

   --------------
   -- Hex_Dump --
   --------------

   procedure Hex_Dump (This : in out Text_Output;
                       Data      : HAL.UInt8_Array;
                       Base_Addr : HAL.UInt64 := 0)
   is

      function UInt8_To_Char (Val : UInt8) return Character;
      procedure Start_New_Line;

      --  Hexdump format:
      --         0000_0000_0000_0000: 57 69 6B 69 70 65 64 69 61 2C 20 74 68 65 20 66  Wikipedia, the f
      --  Addr : ^^^^^^^^^^^^^^^^^^^^
      --  Hex  :                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      --  ASCII:                                                                      ^^^^^^^^^^^^^^^^^

      Addr_Len  : constant := 16 + 3 + 1;
      Hex_Len   : constant := 3 * 16;
      ASCII_Len : constant := 1 + 16;
      Str   : String (1 .. Addr_Len + Hex_Len + ASCII_Len) := (others => ' ');
      UInt4_To_Char : constant array (UInt4) of Character
        := (0 =>  '0',
            1 =>  '1',
            2 =>  '2',
            3 =>  '3',
            4 =>  '4',
            5 =>  '5',
            6 =>  '6',
            7 =>  '7',
            8 =>  '8',
            9 =>  '9',
            10 => 'A',
            11 => 'B',
            12 => 'C',
            13 => 'D',
            14 => 'E',
            15 => 'F');

      -------------------
      -- UInt8_To_Char --
      -------------------

      function UInt8_To_Char (Val : UInt8) return Character is
      begin
         case Val is
            when 0 .. 31 | 127 .. 255 =>
               return '.';
            when others =>
               return Character'Val (Val);
         end case;
      end UInt8_To_Char;

      Index : Natural;
      Cnt   : Natural;
      Addr  : Natural := 0;

      --------------------
      -- Start_New_Line --
      --------------------

      procedure Start_New_Line is
         Addr_Val : UInt64 := UInt64 (Addr) + Base_Addr;
      begin

         --  Address
         for X in reverse 1 .. 19 loop
            if X in 5 | 10 | 15 then
               Str (X) := '_';
            else
               Str (X) := UInt4_To_Char (UInt4 (Addr_Val and 16#0F#));
               Addr_Val := Shift_Right (Addr_Val, 4);
            end if;
         end loop;

         Str (20) := ':';
         Str (21 .. Str'Last) := (others => ' ');

         Cnt := 0;
         Index := Str'First + Addr_Len;
      end Start_New_Line;

   begin

      Start_New_Line;

      for Elt of Data loop

         --  Hex
         Str (Index + 1) := UInt4_To_Char (UInt4 (Shift_Right (Elt, 4)));
         Str (Index + 2) := UInt4_To_Char (UInt4 (Elt and 16#0F#));

         --  ASCII
         Str (Str'Last - (15 - Cnt)) := UInt8_To_Char (Elt);

         Index := Index + 3;
         Cnt   := Cnt + 1;
         Addr  := Addr + 1;
         if Cnt = 16 then
            This.Put_Line (Str);
            Start_New_Line;
         end if;
      end loop;

      if Cnt /= 0 then
         This.Put_Line (Str (Str'First .. Str'Last - (16 - Cnt)));
      end if;
   end Hex_Dump;

   ----------
   -- Dump --
   ----------

   function Dump (This : Text_Output) return AAA.Strings.Vector is
      Result : AAA.Strings.Vector := This.Vect;
   begin
      if Length (This.Current_Line) /= 0 then
         Result.Append (To_String (This.Current_Line));
      end if;
      return Result;
   end Dump;

   -----------
   -- Equal --
   -----------

   function Equal (Expected, Actual : AAA.Strings.Vector) return Boolean is
   begin
      if Expected.Count /= Actual.Count then
         return False;
      end if;

      for Index
      in Expected.First_Index .. Expected.Last_Index
      loop
         if Expected.Element (Index) /= Actual.Element (Index) then
            return False;
         end if;
      end loop;

      return True;
   end Equal;

   ----------
   -- Diff --
   ----------

   function Diff (A, B        : AAA.Strings.Vector;
                  A_Name      : String := "A";
                  B_Name      : String := "B";
                  Skip_Header : Boolean := False)
                  return AAA.Strings.Vector
   is
      --  Tentative Myers diff implementation

      Max : constant Integer := A.Count + B.Count;

      type Action_Kind is (Keep, Insert, Remove);

      type Action is record
         Kind  : Action_Kind;
         Index : Positive;
      end record;

      package Action_Vectors is new
        Ada.Containers.Bounded_Vectors (Positive, Action);

      subtype History_Vector
      is Action_Vectors.Vector (Ada.Containers.Count_Type (Max));

      type Frontier is record
         X       : Integer := 0;
         History : History_Vector;
      end record;

      V : array (-Max .. Max) of Frontier;

      K : Integer;
      X, Y : Integer := 0;
      Go_Down : Boolean;

      History : History_Vector;

      Result : AAA.Strings.Vector;
   begin
      if A.First_Index /= 1 then
         raise Program_Error;
      elsif B.First_Index /= 1 then
         raise Program_Error;
      end if;

      V (1).X := 0;

      Main_Loop :
      for D in 0 .. Max loop
         K := -D;
         while K <= D loop
            Go_Down := (K = -D)
              or else
                ((K /= D) and then (V (K - 1).X < V (K + 1).X));

            if Go_Down then
               X := V (K + 1).X;
               History := V (K + 1).History;
            else
               X := V (K - 1).X + 1;
               History := V (K - 1).History;
            end if;

            Y := X - K;

            if Go_Down and then Y in 1 .. B.Count then
               History.Append ((Insert, Y));
            elsif X in 1 .. A.Count then
               History.Append ((Remove, X));
            end if;

            while X in 0 .. A.Count - 1
              and then
                Y in 0 .. B.Count - 1
                and then
                  A.Element (X + 1) = B.Element (Y + 1)
            loop
               X := X + 1;
               Y := Y + 1;
               History.Append ((Keep, X));
            end loop;

            if X >= A.Count and then Y >= B.Count then
               exit Main_Loop;
            else
               V (K).X := X;
               V (K).History := History;
            end if;

            K := K + 2;
         end loop;
      end loop Main_Loop;

      if not Skip_Header then
         Result.Append (String'("--- " & A_Name));
         Result.Append (String'("+++ " & B_Name));
      end if;

      for Elt of History loop
         case Elt.Kind is
            when Keep =>
               Result.Append (String'("  " & A.Element (Elt.Index)));
            when Remove =>
               Result.Append (String'("- " & A.Element (Elt.Index)));
            when Insert =>
               Result.Append (String'("+ " & B.Element (Elt.Index)));
         end case;
      end loop;
      return Result;
   end Diff;

end USB_Testing.Output;
