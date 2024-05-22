with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Greetings is

   function Call_Hello return chars_ptr with
     Import => True, Convention => C, External_Name => "hello";
   function C_Hello return String is (Value (Call_Hello));

end Greetings;
