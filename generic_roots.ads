--  <A HREF="http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AIs/AI-00346.TXT">
--  AI95-00346</A>
--
--  Implementation Requirements
--
--  The accuracy of these subprograms is implementation defined.
--
--  Implementation Permissions
--
--  The nongeneric equivalent subprograms may, but need not, be actual
--  instantiations of the generic subprograms for the appropriate
--  predefinedtype.
--
--  Implementation Advice
--
--  Implementations should implement these subprograms using established
--  techniques such as Laguerre's method.

pragma License (Unrestricted);
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Arrays;
generic
type Real is digits <>;
with package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays (Real);
with package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Real); use Complex_Types;
with package Complex_Arrays is new Ada.Numerics.Generic_Complex_Arrays (Real_Arrays, Complex_Types); use Complex_Arrays;
procedure Generic_Roots
              (P : Complex_Vector;
               R : out Complex_Vector);
--  The array P defines a polynomial P = P[0] + P[1]*z + P[2]*z**2 + ...
--  + P[n]*z**n where n is P'Last. Constraint_Error is raised if P'First
--  is not zero and if P'Last is not greater than zero. Constraint_Error
--  is also raised if the bounds of the out parameter R are not one and
--  P'Last.
--
--  The components R[1] .. R[n] are the roots of the equation P=0
--  in some order.
