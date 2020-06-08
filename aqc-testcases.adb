with Ada.Assertions;
with Ada.Exceptions;
with AQC.TAP;
package body AQC.Testcases is
   N_Testcase : Natural := 0;

   package body Testcase is
   begin
      N_Testcase := N_Testcase + 1;
      Check;
      AQC.TAP.Verdict (N_Testcase, TAP.Ok, Title);
   exception
      when E : Program_Error | Constraint_Error |
        Storage_Error | Tasking_Error =>
         AQC.TAP.Bailout (Ada.Exceptions.Exception_Name (E) & ": " &
                            Ada.Exceptions.Exception_Message (E));
      when Unimplemented_Testcase =>
         AQC.TAP.Verdict (N_Testcase, TAP.Todo, Title);
      when E : Credits_Exhausted =>
         AQC.TAP.Verdict (N_Testcase, TAP.Skip, Title,
                          Ada.Exceptions.Exception_Message (E));
      when E : Ada.Assertions.Assertion_Error =>
         AQC.TAP.Verdict (N_Testcase, TAP.Fail, Title,
                          Ada.Exceptions.Exception_Message (E));
      when E : others =>
         AQC.TAP.Verdict (N_Testcase, TAP.Fail,
                          Title,
                          Ada.Exceptions.Exception_Name (E) & ": " &
                            Ada.Exceptions.Exception_Message (E));
   end Testcase;

   package body BTestcase is
      procedure Check;

      procedure Check is
      begin
         Ada.Assertions.Assert (Predicate);
      end Check;

      package TC is new Testcase (Title => Title, Check => Check)
        with Unreferenced;
   end BTestcase;

   package body Fail_Testcase is
      procedure Check_Failure;

      procedure Check_Failure is
      begin
         Check;
         raise Ada.Assertions.Assertion_Error with "should fail";
      exception
         when Ada.Assertions.Assertion_Error =>
            null;
      end Check_Failure;

      package TC is new Testcase (Title => Title, Check => Check_Failure)
        with Unreferenced;
   end Fail_Testcase;

   package body Stub_Testcase is
      procedure Stub;

      procedure Stub is
      begin
         raise Unimplemented_Testcase;
      end Stub;

      package TC is new Testcase (Title => Title, Check => Stub)
        with Unreferenced;
   end Stub_Testcase;

   package body VTestcase is
      procedure Check;

      procedure Check is
      begin
         for X of Var loop
            Ada.Assertions.Assert (Property (X));
         end loop;
      end Check;

      package TC is new Testcase (Title => Title, Check => Check)
        with Unreferenced;
   end VTestcase;

   procedure End_Tests is
   begin
      AQC.TAP.Test_Plan (1, N_Testcase);
   end End_Tests;

end AQC.Testcases;
