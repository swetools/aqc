with Ada.Assertions;
package body AQC.SelfTest.Checks is

   procedure Simple_Check is
   begin
      Ada.Assertions.Assert (True);
   end Simple_Check;

   procedure Failure is
   begin
      Ada.Assertions.Assert (False);
   end Failure;

end AQC.SelfTest.Checks;
