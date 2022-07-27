------------------------------------------------------------------
--
--  Collisions
--  Count collisions that would occur in an instance of
--  Ada.Containers.Hashed_Maps using Ada.Strings.Bounded.Hash
--
--  Copyright 2007-2022 John B. Matthews
--  Distribution: GPL with GCC Runtime Library Exception
--
------------------------------------------------------------------

pragma Warnings ("I"); -- allow internal unit, Prime_Numbers
with Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Prime_Numbers;
with Ada.Containers.Vectors;
with Ada.Float_Text_IO;
with Ada.Strings.Bounded.Hash;
with Ada.Text_IO;

procedure Collisions is

   Max_Word  : constant Positive := 26;
   File_Name : constant String   := "/usr/share/dict/words";

   package ASB is new Ada.Strings.Bounded.Generic_Bounded_Length (Max_Word);
   use type ASB.Bounded_String;
   package Container is new Ada.Containers.Vectors
     (Natural, ASB.Bounded_String);
   function Hash is new Ada.Strings.Bounded.Hash (ASB);
   use type Ada.Containers.Hash_Type;
   package ACOM is new Ada.Containers.Ordered_Maps
     (Key_Type => Natural, Element_Type => Natural);
   type Table_Array is array (Natural range <>) of Natural;

   Word_List  : Container.Vector;
   Word_Count : Ada.Containers.Count_Type;

   procedure Show_Counts (Position : ACOM.Cursor) is
      Key     : constant Natural := ACOM.Key (Position);
      Element : constant Natural := ACOM.Element (Position);
      Percent : constant Float   := Float (Key * Element) / Float (Word_Count);
   begin
      Ada.Text_IO.Put (Natural'Image (Key) & ":");
      Ada.Text_IO.Put (Natural'Image (Element) & " (");
      Ada.Float_Text_IO.Put (Percent * 100.0, 1, 2, 0);
      Ada.Text_IO.Put_Line ("%)");
   end Show_Counts;

begin
   Ada.Text_IO.Put_Line (File_Name);
   --  Read the dictionary into Word_List
   declare
      Dict_File : Ada.Text_IO.File_Type;
      Line      : String (1 .. Max_Word);
      Last      : Natural;
      Word      : ASB.Bounded_String;
   begin
      Ada.Text_IO.Open (Dict_File, Ada.Text_IO.In_File, File_Name);
      while not Ada.Text_IO.End_Of_File (Dict_File) loop
         Ada.Text_IO.Get_Line (Dict_File, Line, Last);
         if Last <= Max_Word then
            Word := ASB.To_Bounded_String (Line (1 .. Last));
            Word_List.Append (Word);
         end if;
      end loop;
      Ada.Text_IO.Close (Dict_File);
   end;
   Word_Count := Word_List.Length;
   --  Examine collisions
   declare
      Size : constant Ada.Containers.Hash_Type :=
        Ada.Containers.Prime_Numbers.To_Prime (Word_Count) - 1;
      Table       : Table_Array (0 .. Natural (Size)) := (others => 0);
      Index       : Natural;
      Load_Factor : Float;
      Counts      : ACOM.Map;
      Position    : ACOM.Cursor;
      Inserted    : Boolean;
      Value       : Natural;
   begin
      --  Hash Word_List elements into Table
      for Element of Word_List loop
         Index         := Natural (Hash (Element) mod Table'Length);
         Table (Index) := Table (Index) + 1;
      end loop;
      Ada.Text_IO.Put_Line
        ("Word count:" & Natural'Image (Natural (Word_Count)));
      Ada.Text_IO.Put_Line ("Table size:" & Natural'Image (Table'Length));
      Load_Factor := Float (Word_Count) / Float (Table'Length);
      Ada.Text_IO.Put ("Load factor: ");
      Ada.Float_Text_IO.Put (Load_Factor * 100.0, 1, 2, 0);
      Ada.Text_IO.Put_Line ("%");
      --  Tally results
      for Element of Table loop
         Counts.Insert (Element, 1, Position, Inserted);
         if not Inserted then
            Value := ACOM.Element (Position) + 1;
            Counts.Replace (Element, Value);
         end if;
      end loop;
      Counts.Iterate (Show_Counts'Access);
   end;
end Collisions;
