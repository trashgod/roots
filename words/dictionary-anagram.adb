with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings;
with Ada.Text_IO;             use Ada.Text_IO;

package body Dictionary.Anagram is

   -- Print words of length N comprised of characters in Str.
   procedure Print_More (N : Natural; Str : in String) is

      -- Examine first word in set; the rest are equivalent; ignore case.
      procedure Examine (Words : Set) is
         Word : constant String := To_Lower (To_String (Words.First_Element));

         function Contains (S : String) return Boolean is
            Chars    : String := Str;
            Pattern  : String := " ";
            Position : Natural;
         begin
            for C of S loop
               Pattern (1) := C;
               Position    := Fixed.Index (Chars, Pattern, 1);
               if Position > 0 then
                  Fixed.Overwrite (Chars, Position, " ");
               else
                  return False;
               end if;
            end loop;
            return True;
         end Contains;

      begin
         if Word'Length = N then
            if Contains (Word) then
               for Item of Words loop
                  Put_Line (To_String (Item) & " ");
               end loop;
            end if;
         end if;
      end Examine;

   begin
      Put_Line (Str & ": ");
      for Words of Word_Map loop
         Examine (Words);
      end loop;
   end Print_More;

end Dictionary.Anagram;
