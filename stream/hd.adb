------------------------------------------------------------------
--|
--| Stream Hex Dump
--|
--| Copyright 1995-2023 John B. Matthews
--| Distribution: GPL with GCC Runtime Library Exception
--|
------------------------------------------------------------------

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Streams.Stream_IO; use Ada.Streams;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

procedure HD is
   Hex    : constant array (0 .. 15) of Character := "0123456789abcdef";
   Two_4  : constant                              := 2**4;
   Two_8  : constant                              := 2**8;
   Two_16 : constant                              := 2**16;

   function Hex_Byte (B : Natural) return String is
     (Hex (B / Two_4) & Hex (B mod Two_4)) with
     Inline;

   function Hex_Word (W : Natural) return String is
     (Hex_Byte (W / Two_8) & Hex_Byte (W mod Two_8)) with
     Inline;

   function Hex_Addr (A : Natural) return String is
     (Hex_Byte (A / Two_16) & Hex_Word (A mod Two_16)) with
     Inline;

   -- Output C in hex; update ASCII in S(I) and address A on line break
   procedure Dump (C : Character; S : in out String; I, A : in out Natural) is
      P : Natural;
   begin
      if I = 0 then
         Put (Hex_Addr (A) & ":");
      end if;
      P := Character'Pos (C);
      Put (" " & Hex_Byte (P));
      I := I + 1;
      if P > 31 and P < 127 then -- printable ASCII
         S (I) := C;
      else
         S (I) := '.';
      end if;
      if I = 16 then -- line break
         Put ("  " & S);
         New_Line;
         S := (others => ' ');
         I := 0;
         A := A + 16;
      end if;
   end Dump;

   -- Output ASCII for partial line
   procedure Tail (I : Natural; S : String) is
   begin
      if I > 0 then
         for K in I .. 15 loop
            Put ("   ");
         end loop;
         Put ("  " & S);
         New_Line;
      end if;
   end Tail;

   -- Hex dump a text stream
   procedure Hex_Dump (Stream_Ptr : Text_Streams.Stream_Access) is
      C : Character;
      S : String (1 .. 16) := (others => ' ');
      I : Natural          := 0;
      A : Natural          := 0;
   begin
      loop
         Character'Read (Stream_Ptr, C);
         Dump (C, S, I, A);
      end loop;
   exception
      when End_Error =>
         Tail (I, S);
   end Hex_Dump;

   -- Hex dump a stream
   procedure Hex_Dump (Stream_Ptr : Stream_IO.Stream_Access) is
      C : Character;
      S : String (1 .. 16) := (others => ' ');
      I : Natural          := 0;
      A : Natural          := 0;
   begin
      loop
         Character'Read (Stream_Ptr, C);
         Dump (C, S, I, A);
      end loop;
   exception
      when End_Error =>
         Tail (I, S);
   end Hex_Dump;

   procedure Show_Usage is
   begin
      Put_Line ("HexDump: hd [file ... | stdin]");
   end Show_Usage;

begin
   if Argument_Count = 0 then
      Hex_Dump (Text_Streams.Stream (Current_Input));
   elsif Argument (1) = "-h" then
      Show_Usage;
   else
      declare
         Input_File : Stream_IO.File_Type;
      begin
      for N in 1 .. Argument_Count loop
         Stream_IO.Open (Input_File, Stream_IO.In_File, Argument (N));
         Hex_Dump (Stream_IO.Stream (Input_File));
         Stream_IO.Close (Input_File);
      end loop;
      end;
   end if;
exception
   when Error : others =>
      Put_Line (Exception_Name (Error));
      Show_Usage;
end HD;
