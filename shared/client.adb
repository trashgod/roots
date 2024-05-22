with Greetings;
with Ada.Text_IO;

procedure Client is

begin
   Ada.Text_IO.Put_Line (Greetings.Ada_Hello);
   Ada.Text_IO.Put_Line (Greetings.C_Hello);
end Client;
