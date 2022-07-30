------------------------------------------------------------------
--
--  Collisions
--  Count collisions that would occur in an instance of
--  Ada.Containers.Hashed_Maps using Ada.Strings.Hash
--
--  Copyright 2007-2022 John B. Matthews
--  Distribution: GPL with GCC Runtime Library Exception
--
------------------------------------------------------------------

with Ada.Strings.Hash; use Ada.Strings;
with Ada.Text_IO;      use Ada.Text_IO;
with Report;

procedure Collisions is

   File_Name : constant String := "/usr/share/dict/words";
   Hash_List : Report.Hash_Vectors.Vector;
   Dict_File : Ada.Text_IO.File_Type;

begin
   Put_Line (File_Name);
   Open (Dict_File, In_File, File_Name);
   while not End_Of_File (Dict_File) loop
      Hash_List.Append (Hash (Get_Line (Dict_File)));
   end loop;
   Close (Dict_File);
   Report.Print (Hash_List);
end Collisions;
