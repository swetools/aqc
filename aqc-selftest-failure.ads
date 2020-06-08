with AQC.Testcases;
use AQC.Testcases;
with AQC.SelfTest.Checks;
private package AQC.SelfTest.Failure is

   package Missing_Test is
      new Stub_Testcase (Title => "A missing test");

   package Failing_Test is
      new Fail_Testcase (Title => "A trivial expected-failure test",
                         Check => AQC.SelfTest.Checks.Failure);

end AQC.SelfTest.Failure;
