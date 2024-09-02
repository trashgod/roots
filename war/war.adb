-------------------------------------------------------
--
-- Simulate the card game of war.
--
-- Copyright 1998-2024, Gem City Software
-- Distribution:  GPL
--
-------------------------------------------------------
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Numerics.Elementary_Functions;
with Cards;
use type Cards.Value;

procedure War is

   package TIO renames Ada.Text_IO;
   Max_Games : constant Natural := 10_000;
   Max_Plays : constant Natural := 1_200;
   Bin_Size  : constant Natural := 100;
   Plot_Size : constant Natural := 100;

   Deck, Copy : Cards.Pack;           -- the game deck and a copy
   West, East : Cards.Pack;           -- the players' hands

   West_Count : Natural := 0;         -- wins for West
   East_Count : Natural := 0;         -- wins for East
   Longest    : Natural := 0;         -- longest game
   Shortest   : Natural := Max_Plays; -- shortest game
   Game_Count : Natural := 0;         -- game count
   Total      : Natural := 0;         -- total play count
   Total2     : Float   := 0.0;       -- sum of squares
   Sigma      : Float;

   type Histogram is array (0 .. Max_Plays / Bin_Size - 1) of Natural;
   Graph : Histogram := (others => 0);

   procedure Deal (Deck : in Cards.Pack; West, East : in out Cards.Pack) is
      Temp : Cards.Pack := Deck;     -- deal from a copy
   begin
      Cards.Make_Empty (West);
      Cards.Make_Empty (East);
      while not Cards.Empty (Temp) loop
         Cards.Move (Temp, West);
         Cards.Move (Temp, East);
      end loop;
   end Deal;

   procedure Play is
      C1, C2 : Cards.Card;
      Table  : Cards.Pack;
   begin
      Cards.Draw (West, C1);          -- draw from each hand
      Cards.Draw (East, C2);
      if C1.V = C2.V then            -- war!
         while C1.V = C2.V loop      -- play until either wins
            Cards.Place (Table, C1);  -- lay 'em on the table
            Cards.Place (Table, C2);
            Cards.Draw (West, C1);    -- draw two more
            Cards.Draw (East, C2);
         end loop;
      end if;
      if C1.V > C2.V then            -- West wins
         Cards.Place (West, C1);      -- winning card first
         Cards.Place (West, C2);
         while not Cards.Empty (Table) loop
            Cards.Move (Table, West); -- clear the table
         end loop;
      else                           -- East wins
         Cards.Place (East, C2);      -- winning card first
         Cards.Place (East, C1);
         while not Cards.Empty (Table) loop
            Cards.Move (Table, East); -- clear the table
         end loop;
      end if;
   end Play;

   procedure Play_Game is
      Counter : Natural := 0;
      Bin     : Natural;
   begin
      while Counter < Max_Plays loop
         Play;
         Counter := Counter + 1;
      end loop;
   exception
      when Cards.Pack_Empty =>       -- game over, collect stats
         Total       := Total + Counter;
         Total2      := Total2 + Float (Counter * Counter);
         Game_Count  := Game_Count + 1;
         Bin         := Counter / Bin_Size;
         Graph (Bin) := Graph (Bin) + 1;
         if Cards.Empty (East) then
            West_Count := West_Count + 1;
         else
            East_Count := East_Count + 1;
         end if;
         if Counter < Shortest then
            Shortest := Counter;
            Copy     := Deck; -- save this one
         end if;
         if Counter > Longest then
            Longest := Counter;
         end if;
         TIO.Put (ASCII.NUL);
   end Play_Game;

   function Label (J : Natural) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Lower : constant String := Natural'Image (J * Bin_Size);
      Upper : constant String := Natural'Image ((J + 1) * Bin_Size);
   begin
      return
        Tail (Trim (Lower, Left), 4, '0') & "-" &
        Tail (Trim (Upper, Left), 4, '0') & " |*";
   end Label;

begin
   TIO.Put_Line ("Working...");
   for I in 1 .. Max_Games loop
      Cards.Shuffle (Deck);
      Deal (Deck, West, East);
      Play_Game;
   end loop;
   Sigma := Ada.Numerics.Elementary_Functions.Sqrt
       ((Total2 - Float (Total)**2 / Float (Game_Count)) /
        (Float (Game_Count) - 1.0));
   TIO.Put ("Of" & Natural'Image (Max_Games) & " games,");
   TIO.Put (Natural'Image (Game_Count) & " ended before");
   TIO.Put (Natural'Image (Max_Plays) & " plays.");
   TIO.New_Line;
   TIO.Put ("West won:      " & Natural'Image (West_Count));
   TIO.New_Line;
   TIO.Put ("East won:      " & Natural'Image (East_Count));
   TIO.New_Line;
   TIO.Put ("Average length:" & Natural'Image (Total / Game_Count));
   TIO.New_Line;
   TIO.Put ("Standard dev.: " & Natural'Image (Natural (Sigma)));
   TIO.New_Line;
   TIO.Put ("Longest game:  " & Natural'Image (Longest));
   TIO.New_Line;
   TIO.Put ("Shortest game: " & Natural'Image (Shortest));
   TIO.New_Line;
   Deal (Copy, West, East);        -- reconstruct the shortest
   TIO.Put ("W: ");
   Cards.Show (West);
   TIO.New_Line;
   TIO.Put ("E: ");
   Cards.Show (East);
   TIO.New_Line;
   TIO.Put_Line ("Distribution of lengths:");
   for J in Graph'Range loop
      TIO.Put (Label (J));
      for K in 1 .. (Graph (J) * Plot_Size) / Game_Count loop
         TIO.Put ("*");
      end loop;
      TIO.New_Line;
   end loop;
end War;
