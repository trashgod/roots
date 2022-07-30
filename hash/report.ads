with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

package Report is
   package Hash_Vectors is new Vectors (Natural, Hash_Type);
   procedure Print (Hash_List : Hash_Vectors.Vector);
end Report;
