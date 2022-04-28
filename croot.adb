------------------------------------------------------------------
--|
--| Complex_Arrays.Roots
--| 
--| Author: John B. Matthews
--| Distribution: GPL
--| Last Modified: April 2022
--|
------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Arrays;
with Generic_Roots;

procedure Croot is
   package TIO renames Ada.Text_IO;
   package IIO renames Ada.Integer_Text_IO;
   package FIO renames Ada.Long_Float_Text_IO;
   package NRA is new Ada.Numerics.Generic_Real_Arrays (Long_Float);
   package NCT is new Ada.Numerics.Generic_Complex_Types (Long_Float);
   package NCA is new Ada.Numerics.Generic_Complex_Arrays (NRA, NCT);
   procedure Roots is new Generic_Roots(Long_Float, NRA, NCT, NCA);
   use type NCT.Complex;
   
   Epsilon : constant Long_Float := 1.0 / (10.0 ** Long_Float'Digits); 

   procedure Sort (A : in out NCA.Complex_Vector) is
      Temp    : NCT.Complex;
      Swapped : Boolean;
   begin
      loop
         Swapped := False;
         for I in A'First .. A'Last - 1 loop
            if NCT.Re (A (I + 1)) < NCT.Re (A (I)) then
               Temp := A (I);
               A (I) := A (I + 1);
               A (I + 1) := Temp;
               Swapped := True;
            end if;
         end loop;
         exit when not Swapped;
      end loop;
   end Sort;

   procedure Show_Poly(P : NCA.Complex_Vector) is
      Precision : constant Natural := 2;
   begin
      TIO.Put("Poly: ");
      for I in reverse P'Range loop
         if I = P'Last then
            if NCT.Re(P(I)) >= 0.0 then
               TIO.Put(" ");
            end if;
            FIO.Put(NCT.Re(P(I)), 0, Precision, 0);
         else
            FIO.Put(abs(NCT.Re(P(I))), 0, Precision, 0);
         end if;
         if I /= P'First then
            TIO.Put("x^");
            IIO.Put(I, 0);
            if NCT.Re(P(I - 1)) < 0.0 then
               TIO.Put(" - ");
            else
               TIO.Put(" + ");
            end if;
         end if;
      end loop;
      TIO.New_Line;
   end Show_Poly;
   
   procedure Show_Real(X : Long_Float) is
   begin
      if abs(X) > Epsilon then
         FIO.Put(X);
      else
         FIO.Put(0.0);
      end if;
   end;

   procedure Show_Roots(R : in out NCA.Complex_Vector; pair : Boolean) is
      Re, Im : Long_Float;
      Index  : Natural := R'First;
   begin
      Sort(R);
      while Index <= R'Last loop
         Re := NCT.Re(R(Index));
         Im := NCT.Im(R(Index));
         if abs(Im - 0.0) < Epsilon then
            TIO.Put("Real: ");
            Show_Real(Re);
         else
            if pair then
               TIO.Put("Comp: ");
               Show_Real(Re);
               TIO.Put(" +-");
               Show_Real(abs(Im));
               TIO.Put("i");
               Index := Index + 1;
            else
               TIO.Put("Comp: ");
               Show_Real(Re);
               if Im > 0.0 then
                  TIO.Put(" +");
               else
                  TIO.Put(" -");
               end if;
               Show_Real(abs(Im));
               TIO.Put("i");
            end if;
         end if;
         Index := Index + 1;
         TIO.New_Line;
      end loop;
   end Show_Roots;
   
   function Eval (P : NCA.Complex_Vector; X : NCT.Complex) return NCT.Complex is
      Result : NCT.Complex := P (P'Last);
   begin
      for I in reverse 0 .. P'Last - 1 loop
         Result := Result  * X + P (I);
      end loop;
      return Result;
   end Eval;
   
   procedure Validate(P, R : NCA.Complex_Vector) is
      Zero   : constant NCT.Complex := NCT.Compose_From_Cartesian(0.0);
      Error  : Long_Float := NCT.Re(Zero);
      Result : NCT.Complex := Zero;
   begin
      for I in R'Range loop
         Result := Eval(P, R(I)) - Zero;
         Error := Long_Float'Max(Error, abs(NCT.Re(Result)));
         Error := Long_Float'Max(Error, abs(NCT.Im(Result)));
      end loop;
      TIO.Put("Largest error: ");
      if Error > Epsilon then
         TIO.Put("=");
         FIO.Put(Error);
      else
         TIO.Put("< 1.0E-15");
      end if;
      TIO.New_Line;
   end Validate;

   -- Real coefficients
   procedure Show_Result(A : in NRA.Real_Vector) is
      P : NCA.Complex_Vector(0 .. A'Length - 1);
      R : NCA.Complex_Vector(1 .. P'Length - 1);
   begin
      P := NCA.Compose_From_Cartesian(A);
      Roots(P, R);
      Show_Poly(P);
      Show_Roots(R, True);
      Validate(P, R);
      TIO.New_Line;
   end;

   -- Complex coefficients
   procedure Show_Result(A, B : in NRA.Real_Vector) is
      P : NCA.Complex_Vector(0 .. A'Length - 1);
      R : NCA.Complex_Vector(1 .. P'Length - 1);
   begin
      P := NCA.Compose_From_Cartesian(A, B);
      Roots(P, R);
      Show_Roots(R, False);
      Validate(P, R);
      TIO.New_Line;
   end;

   procedure Show_AOP(N : Natural) is
      P : constant NRA.Real_Vector(0 .. N) := (others => 1.0);
   begin
      Show_Result(P);
   end Show_AOP;

begin
   TIO.Put_Line("Ada.Numerics.Long_Complex_Arrays.Roots:");
   -- different real roots (x - 1)(x - 2) = x^2  - 3x + 2
   Show_Result((2.0, -3.0, 1.0));

   -- identical real roots (x - 1)(x - 1) = x^2  - 2x + 1
   Show_Result((1.0, -2.0, 1.0));

   -- complex conjugate roots (x + 2i)(x - 2i) = x^2 + 4
   Show_Result((4.0, 0.0, 1.0));

   -- more complex roots (2x + i)(2x - i) = 4x^2 + 1
   Show_Result((1.0, 0.0, 4.0));

   -- three real roots x(x - 1)(x - 2) = x^3  - 3x^2 + 2x
   Show_Result((0.0, 2.0, -3.0, 1.0));

   -- (x + 2)(x - 2)(x + 3)(x - 3) = x^4 + -13x^2 + 36
   Show_Result((36.0, 0.0, -13.0, 0.0, 1.0));

   -- x(x + 2)(x - 2)(x + 3)(x - 3) = x^5 - 13x^3 + 36x
   Show_Result((0.0, 36.0, 0.0, -13.0, 0.0, 1.0));

   for I in 2 .. 15 loop
      Show_AOP(I);
   end loop;
   Show_AOP(37);

   TIO.Put_Line("Poly: (1 + 3i)x^2 + (2 + 2i)x + (3 + i)");
   Show_Result((3.0, 2.0, 1.0), (1.0, 2.0, 3.0));

   TIO.Put_Line("Poly: (1 + i)x^3 + (2 + i)x^2 + (3 + i)x + (4 + i)");
   Show_Result((4.0, 3.0, 2.0, 1.0), (1.0, 1.0, 1.0, 1.0));

   TIO.Put_Line("Poly: (1 + i)x^3 + (2 + 2i)x^2 + (3 + 3i)x + (4 + 4i)");
   Show_Result((4.0, 3.0, 2.0, 1.0), (4.0, 3.0, 2.0, 1.0));

end Croot;