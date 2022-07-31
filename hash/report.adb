------------------------------------------------------------------
--
--  Report collisions that would occur in an instance of
--  Ada.Containers.Hashed_Maps given a list of hashes.
--
--  Copyright 2007-2022 John B. Matthews
--  Distribution: GPL with GCC Runtime Library Exception
--
------------------------------------------------------------------

pragma License (Modified_GPL);
pragma Warnings ("I"); -- allow internal unit Prime_Numbers
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Prime_Numbers; use Ada.Containers.Prime_Numbers;
with Ada.Float_Text_IO;            use Ada.Float_Text_IO;
with Ada.Text_IO;                  use Ada.Text_IO;

package body Report is

   package Maps is new Ordered_Maps
     (Key_Type => Natural, Element_Type => Natural);
   type Table_Array is array (Natural range <>) of Natural;

   procedure Print (Hash_List : Hash_Vectors.Vector) is
      Hash_Count  : constant Count_Type := Hash_List.Length;
      Size        : constant Hash_Type := To_Prime (Hash_Count) - 1;
      Load_Factor : constant Float := Float (Hash_Count) / Float (Size);
      Table       : Table_Array (0 .. Natural (Size)) := (others => 0);
      Index       : Natural;
      Counts      : Maps.Map;
      Position    : Maps.Cursor;
      Inserted    : Boolean;
      Value       : Natural;

      procedure Print_Count (Position : Maps.Cursor) is
         Key     : constant Natural := Maps.Key (Position);
         Element : constant Natural := Maps.Element (Position);
         Percent : constant Float   :=
           Float (Key * Element) / Float (Hash_Count);
      begin
         Put (Natural'Image (Key) & ":");
         Put (Natural'Image (Element) & " (");
         Put (Percent * 100.0, 1, 2, 0);
         Put_Line ("%)");
      end Print_Count;

   begin
      Put_Line ("Hash count:" & Count_Type'Image (Hash_Count));
      Put_Line ("Table size:" & Natural'Image (Table'Length));
      Put ("Load factor: ");
      Put (Load_Factor * 100.0, 1, 2, 0);
      Put_Line ("%");
      --  Count collisions
      for Element of Hash_List loop
         Index         := Natural (Element mod Table'Length);
         Table (Index) := Table (Index) + 1;
      end loop;
      --  Tally results
      for Element of Table loop
         Counts.Insert (Element, 1, Position, Inserted);
         if not Inserted then
            Value := Maps.Element (Position) + 1;
            Counts.Replace (Element, Value);
         end if;
      end loop;
      Counts.Iterate (Print_Count'Access);
   end Print;

end Report;
