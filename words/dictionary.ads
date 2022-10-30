------------------------------------------------------------------
--
--  Dictionary: Read a dictionary and construct a map
--  of permutatively equivalent words.
--
--  Copyright 2007-2022 John B. Matthews
--  Distribution: GPL with GCC Runtime Library Exception
--
------------------------------------------------------------------
with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

package Dictionary is

   File_Name : constant String     := "/usr/share/dict/words";
   Reserved  : constant Count_Type := 220_000; -- default map capacity

   -- Read dictionary, one word per line; map words sorted
   -- by character to permutatively equivalent words.
   procedure Read_Dictionary
     (Name : String := File_Name; Count : Count_Type := Reserved);

   -- Find Str in Word_Map; print permutatively equivalent words.
   procedure Print_One (Str : in String);

   -- Print each entry having more than one word in its set.
   procedure Print_All;

private

   package Word_Sets is new Ordered_Sets (Unbounded_String);
   use Word_Sets;

   package Word_Maps is new Hashed_Maps
     (Key_Type        => Unbounded_String, Element_Type => Set, Hash => Hash,
      Equivalent_Keys => "=");
   use Word_Maps;

   Word_Map : Map;

end Dictionary;
