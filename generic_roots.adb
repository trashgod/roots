pragma License (Modified_GPL);
------------------------------------------------------------------
--
--  Generic_Roots
--
--  Copyright 2007-2022 John B. Matthews
--  Distribution: GPL with GCC Runtime Library Exception
--
--  This is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  As a special exception under Section 7 of GPL version 3, you are
--  granted additional permissions described in the GCC Runtime Library
--  Exception, version 3.1, as published by the Free Software Foundation.
--
--  This is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Foobar. If not, see <https://www.gnu.org/licenses/>.

------------------------------------------------------------------
--
--  Implementation notes:

--  This is a stand-alone, experimental implementation of AI-346.
--
--  This implementation uses the Durand-Kerner-Weierstrass method
--  to find the complex roots of a polynomial with complex
--  coefficients. The method requires a monic polynomial; some
--  error may occur when dividing by the leading coefficient.
--
--  Set Debug := True to enable diagnostic output.
--

with Ada.Text_IO;

procedure Generic_Roots (P : Complex_Vector; R : out Complex_Vector) is
   use Complex_Arrays;

   Debug     : constant Boolean   := False;
   Epsilon   : constant Real'Base := 1.0 / (10.0**Real'Digits);
   Max_Count : constant Natural   := 999;

   --  Use Durand-Kerner-Weierstrass method
   procedure OrderN (P : Complex_Vector; R : out Complex_Vector);

   --  Determine if the components of two vectors are unchanging
   function Done (A, B : Complex_Vector) return Boolean;

   --  Debug output
   procedure Dump (A : Complex_Vector);

   --  Evaluate the polynomial P at X by Horner's method
   function Eval (P : Complex_Vector; X : Complex) return Complex;

   function Done (A, B : Complex_Vector) return Boolean is
      Unchanged : Boolean := True;
      Change    : Complex;
   begin
      for I in A'Range loop
         Change    := A (I) - B (I);
         Unchanged :=
           Unchanged and abs (Re (Change)) < Epsilon and
           abs (Im (Change)) < Epsilon;
      end loop;
      return Unchanged;
   end Done;

   procedure Dump (A : Complex_Vector) is
   begin
      for I in A'Range loop
         Ada.Text_IO.Put (I'Img & ": ");
         Ada.Text_IO.Put (Re (A (I))'Img);
         Ada.Text_IO.Put (Im (A (I))'Img);
         Ada.Text_IO.New_Line;
      end loop;
   end Dump;

   function Eval (P : Complex_Vector; X : Complex) return Complex is
      Result : Complex := P (P'Last);
   begin
      for I in reverse 0 .. P'Last - 1 loop
         Result := Result * X + P (I);
      end loop;
      return Result;
   end Eval;

   procedure OrderN (P : Complex_Vector; R : out Complex_Vector) is
      Count  : Natural                           := 1;
      P0     : constant Complex_Vector (P'Range) := P / P (P'Last);
      A0, A1 : Complex_Vector (R'Range);
      One    : constant Complex := Compose_From_Cartesian (1.0);
      Result : Complex := Compose_From_Cartesian (0.4, 0.9);
   begin
      --  initialize A0
      A0 (A0'First) := One;
      for I in A0'First + 1 .. A0'Last loop
         A0 (I) := A0 (I - 1) * Result;
      end loop;

      --  iterate
      loop
         for I in A0'Range loop
            Result := One;
            for J in A0'Range loop
               if I /= J then
                  Result := Result * (A0 (I) - A0 (J));
               end if;
            end loop;
            A1 (I) := A0 (I) - (Eval (P0, A0 (I)) / Result);
         end loop;
         Count := Count + 1;
         exit when Count > Max_Count or Done (A0, A1);
         A0 := A1;
      end loop;

      --  report results
      R := A1;

      if Debug then
         Ada.Text_IO.Put_Line ("Iterations: " & Count'Img);
         Dump (A1);
      end if;
   end OrderN;

begin

   if P'First /= 0 or P'First = P'Last or R'First /= 1 or P'Last /= R'Last then
      raise Constraint_Error;
   end if;

   if P'Last > 1 then
      OrderN (P, R);
   end if;

end Generic_Roots;
