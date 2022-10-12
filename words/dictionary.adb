with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;             use Ada.Text_IO;

package body Dictionary is

   procedure Sort is new Generic_Array_Sort
     (Index_Type => Positive, Element_Type => Character, Array_Type => String);

   package Count_IO is new Ada.Text_IO.Integer_IO (Count_Type);

   package Word_Sets is new Ordered_Sets (Unbounded_String);
   use Word_Sets;
   use type Set;

   package Word_Maps is new Hashed_Maps
     (Key_Type        => Unbounded_String, Element_Type => Set, Hash => Hash,
      Equivalent_Keys => "=");
   use Word_Maps;

   Word_Map : Map;

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
     (Name : String := File_Name; Count : Count_Type := Max_Count)
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
   procedure Print_Word (Position : in Word_Sets.Cursor) is
      Item : constant Unbounded_String := Element (Position);
   begin
      Put (To_String (Item) & " ");
   end Print_Word;

   -- Find Str in Word_Map; print permutatively equivalent words.
   procedure Print_One (Str : in String) is
      Word  : constant Unbounded_String := Sort (Str);
      Words : Set;

   begin
      Put (Str & ": ");
      if Word_Map.Contains (Word) then
         Words := Word_Map.Element (Word);
         Words.Iterate (Print_Word'Access);
         New_Line;
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
            Words.Iterate (Print_Word'Access);
            New_Line;
         end if;
      end loop;
   end Print_All;

end Dictionary;
