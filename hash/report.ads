------------------------------------------------------------------
--
--  Report collisions that would occur in an instance of
--  Ada.Containers.Hashed_Maps given a list of hashes.
--
--  Copyright 2007-2022 John B. Matthews
--  Distribution: GPL with GCC Runtime Library Exception
--
------------------------------------------------------------------

pragma License (Modified_GPL);
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

package Report is
   package Hash_Vectors is new Vectors (Natural, Hash_Type);
   procedure Print (Hash_List : Hash_Vectors.Vector);
end Report;
