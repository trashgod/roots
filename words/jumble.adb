------------------------------------------------------------------
--
--  Jumble:  Find permutations of dictionary words
--
--  Copyright 2007-2022 John B. Matthews
--  Distribution: GPL with GCC Runtime Library Exception
--
------------------------------------------------------------------
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Dictionary;       use Dictionary;

procedure Jumble is

begin
   Read_Dictionary;
   if Argument_Count = 0 then
      Print_All;
   elsif Argument (1) = "-h" then
      Put_Line ("jumble [word, …]");
   else
      for I in 1 .. Argument_Count loop
         Print_One (Argument (I));
      end loop;
   end if;
end Jumble;
