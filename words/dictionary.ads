------------------------------------------------------------------
--
--  Dictionary: Read a dictionary and construct a map
--  of permutatively equivalent words.
--
--  Copyright 2007-2022 John B. Matthews
--  Distribution: GPL with GCC Runtime Library Exception
--
------------------------------------------------------------------
with Ada.Containers; use Ada.Containers;

package Dictionary is

   File_Name : constant String     := "/usr/share/dict/words";
   Max_Count : constant Count_Type := 220_000;

   -- Read dictionary, one word per line; map words sorted
   -- by character to permutatively equivalent words.
   procedure Read_Dictionary
     (Name : String := File_Name; Count : Count_Type := Max_Count);

   -- Find Str in Word_Map; print permutatively equivalent words.
   procedure Print_One (Str : in String);

   -- Print each entry having more than one word in its set.
   procedure Print_All;

end Dictionary;
