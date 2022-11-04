------------------------------------------------------------------
--
--  Jumble: Find permutations of dictionary words
--
--  Copyright 2007-2022 John B. Matthews
--  Distribution: GPL with GCC Runtime Library Exception
--
------------------------------------------------------------------
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Dictionary;       use Dictionary;
with Dictionary.Anagram;

procedure Jumble is

   function Is_Positive (Index : Positive) return Boolean is
   begin
      return Positive'Value (Argument (Index)) > 0;
   exception
      when others =>
         return False;
   end Is_Positive;

   function Get_Count (Index : Positive) return Natural is
     (Natural'Value (Argument (Index)));

begin
   Read_Dictionary;
   if Argument_Count = 0 then
      Print_All;
   elsif Argument (1) = "-h" then
      Put_Line ("jumble [-h] [word …]");
      Put_Line ("jumble n chars");
   elsif Argument_Count = 2 and then Is_Positive (1) then
      Anagram.Print_More (Get_Count (1), Argument (2));
   else
      for I in 1 .. Argument_Count loop
         Print_One (Argument (I));
      end loop;
   end if;
end Jumble;
