pragma License (Modified_GPL);
------------------------------------------------------------------
--
--  Generic_Roots Specification
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
--  Foobar is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Foobar. If not, see <https://www.gnu.org/licenses/>.
--
--  This specification is modeled on AI95-00346:
--  http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AIs/AI-00346.TXT
------------------------------------------------------------------

with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Arrays;
generic
   type Real is digits <>;
   with package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays (Real);
   with package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Real);
   use Complex_Types;
   with package Complex_Arrays is new Ada.Numerics.Generic_Complex_Arrays (Real_Arrays, Complex_Types);
   use Complex_Arrays;
procedure Generic_Roots (P : Complex_Vector; R : out Complex_Vector);
--  The array P defines a polynomial P = P[0] + P[1]*z + P[2]*z**2 + ...
--  + P[n]*z**n where n is P'Last. Constraint_Error is raised if P'First
--  is not zero and if P'Last is not greater than zero. Constraint_Error
--  is also raised if the bounds of the out parameter R are not one and
--  P'Last.
--
--  The components R[1] .. R[n] are the roots of the equation P=0
--  in some order.
