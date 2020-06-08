with AQC.Testcases;
use AQC.Testcases;
with AQC.SelfTest.Checks;
use AQC.SelfTest.Checks;
private package AQC.SelfTest.Simple is

   package Simple_Test_TC is new Testcase (Title => "A simple test",
                                           Check => Simple_Check);

   package Simple_Test_TC_Default_Name is
      new Testcase (Check => Simple_Check);

   package Bool_Test_TC is new BTestcase (Title => "A boolean test",
                                          Predicate => Bool_Check);

end AQC.SelfTest.Simple;
