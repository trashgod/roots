with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

package body Cards is

   package TIO renames Ada.Text_IO;
   package Any_Card is new Ada.Numerics.Discrete_Random (Card_Range);
   G : Any_Card.Generator;

   procedure Make_Empty (P : in out Pack) is
   begin
      P.Size := 0;
      P.Head := 1;
      P.Tail := 0;
   end Make_Empty;

   procedure Shuffle (P : in out Pack) is
      J         : Card_Range;
      Temporary : Card;
   begin
      Make_Empty (P);
      for S in Suit'Range loop
         for V in Value'Range loop
            Place (P, (V, S));
         end loop;
      end loop;
      for I in reverse Card_Range loop
         J          := Any_Card.Random (G, Card_Range'First, I);
         Temporary  := P.Card (J);
         P.Card (J) := P.Card (I);
         P.Card (I) := Temporary;
      end loop;
   end Shuffle;

   procedure Draw (P : in out Pack; C : out Card) is
   begin
      if Empty (P) then
         raise Pack_Empty;
      else
         C      := P.Card (P.Head);
         P.Size := P.Size - 1;
         P.Head := P.Head + 1;
         if P.Head > Card_Range'Last then
            P.Head := Card_Range'First;
         end if;
      end if;
   end Draw;

   procedure Place (P : in out Pack; C : in Card) is
   begin
      if Full (P) then
         raise Pack_Full;
      else
         P.Size := P.Size + 1;
         P.Tail := P.Tail + 1;
         if P.Tail > Card_Range'Last then
            P.Tail := Card_Range'First;
         end if;
         P.Card (P.Tail) := C;
      end if;
   end Place;

   procedure Move (P1, P2 : in out Pack) is
      C : Card;
   begin
      Draw (P1, C);
      Place (P2, C);
   end Move;

   function Empty (P : in Pack) return Boolean is
   begin
      return P.Size = 0;
   end Empty;

   function Full (P : in Pack) return Boolean is
   begin
      return P.Size = Card_Range'Last;
   end Full;

   procedure Show (P : in Pack) is
      Temp : Pack := P;
      C    : Card;
   begin
      while not Empty (Temp) loop
         Draw (Temp, C);
         case C.V is
            when Deuce =>
               TIO.Put ("2 ");
            when Trey =>
               TIO.Put ("3 ");
            when Four =>
               TIO.Put ("4 ");
            when Five =>
               TIO.Put ("5 ");
            when Six =>
               TIO.Put ("6 ");
            when Seven =>
               TIO.Put ("7 ");
            when Eight =>
               TIO.Put ("8 ");
            when Nine =>
               TIO.Put ("9 ");
            when Ten =>
               TIO.Put ("T ");
            when Jack =>
               TIO.Put ("J ");
            when Queen =>
               TIO.Put ("Q ");
            when King =>
               TIO.Put ("K ");
            when Ace =>
               TIO.Put ("A ");
         end case;
      end loop;
   end Show;

begin
   Any_Card.Reset (G);
end Cards;
