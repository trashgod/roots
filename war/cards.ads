------------------------------------------------------
--
-- Simulate a pack of playing cards, suitable for war.
--
------------------------------------------------------

package Cards is

   type Pack is private;

   Pack_Empty : exception; -- attempt to Draw from an empty Pack
   Pack_Full  : exception; -- attempt to Place on a full Pack

   type Value is
     (Deuce, Trey, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King,
      Ace);

   type Suit is (Hearts, Clubs, Diamonds, Spades); -- not required

   type Card is record
      V : Value;
      S : Suit;
   end record;

   procedure Make_Empty (P : in out Pack);
   -- Make P empty

   procedure Shuffle (P : in out Pack);
   -- Shuffle a fresh pack

   procedure Draw (P : in out Pack; C : out Card);
   -- Draw one card C from the top of pack P

   procedure Place (P : in out Pack; C : in Card);
   -- Place the card C on the bottom of pack P

   procedure Move (P1, P2 : in out Pack);
   -- Draw from P1 and Place on P2;

   function Empty (P : in Pack) return Boolean;
   -- Returns true if P is empty

   function Full (P : in Pack) return Boolean;
   -- Returns true if P is full
   procedure Show (P : in Pack);

private

   subtype Card_Range is Positive range 1 .. 52;
   type Card_Store is array (Card_Range) of Card;
   -- a ring buffer holding up to 52 cards
   type Pack is record
      Size : Natural := 0;
      Head : Natural := 1;
      Tail : Natural := 0;
      Card : Card_Store;
   end record;

end Cards;
