------------------------------------------------------------------
--
--  Dictionary: Read a dictionary and construct a map
--  of permutatively equivalent words.
--
--  Copyright 2007-2022 John B. Matthews
--  Distribution: GPL with GCC Runtime Library Exception
--
------------------------------------------------------------------
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Generic_Array_Sort;
with Ada.Text_IO;             use Ada.Text_IO;

package body Dictionary is

   package Count_IO is new Ada.Text_IO.Integer_IO (Count_Type);

   procedure Sort is new Generic_Array_Sort
     (Index_Type => Positive, Element_Type => Character, Array_Type => String);

   -- Sort the characters of Str returning a lower case Key
   function Sort (Str : String) return Unbounded_String is
      Key : String (1 .. Str'Length) := To_Lower (Str);
   begin
      Sort (Key);
      return To_Unbounded_String (Key);
   end Sort;

   -- Read dictionary, one word per line; map words sorted
   -- by character to permutatively equivalent words.
   procedure Read_Dictionary
     (Name : String := File_Name; Count : Count_Type := Reserved)
   is
      Dict_File : File_Type;
      Line      : String (1 .. 32);
      Last      : Natural;
      Word      : Unbounded_String;
      Sorted    : Unbounded_String;
      Position  : Word_Maps.Cursor;
      Inserted  : Boolean;
   begin
      Word_Map.Reserve_Capacity (Count);
      Open (Dict_File, In_File, Name);
      while not End_Of_File (Dict_File) loop
         Get_Line (Dict_File, Line, Last);
         Word   := To_Unbounded_String (Line (1 .. Last));
         Sorted := Sort (Line (1 .. Last));
         Word_Map.Insert (Sorted, Position, Inserted);
         Word_Map (Position).Insert (Word);
      end loop;
      Close (Dict_File);
      Put ("Checking ");
      Count_IO.Put (Word_Map.Length, 0);
      Put_Line (" entries.");
   end Read_Dictionary;

   -- Print each word in a set.
   procedure Print_Words (Words : Set) is
   begin
      for Item of Words loop
         Put (To_String (Item) & " ");
      end loop;
      New_Line;
   end Print_Words;

   -- Find Str in Word_Map; print permutatively equivalent words.
   procedure Print_One (Str : in String) is
      Word : constant Unbounded_String := Sort (Str);
   begin
      Put (Str & ": ");
      if Word_Map.Contains (Word) then
         Print_Words (Word_Map.Element (Word));
      else
         Put_Line ("no match.");
      end if;
   end Print_One;

   -- Print each entry having more than one word in its set.
   procedure Print_All is
   begin
      for Words of Word_Map loop
         if Words.Length > 1 then
            Count_IO.Put (Words.Length, 0);
            Put (" ");
            Print_Words (Words);
         end if;
      end loop;
   end Print_All;

end Dictionary;
