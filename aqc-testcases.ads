with AQC.Variables.Variable_Type;
with GNAT.Source_Info;
package AQC.Testcases is

   generic
      with procedure Check;
      Title : String := GNAT.Source_Info.Enclosing_Entity;
   package Testcase is
   end Testcase;

   generic
      with function Predicate return Boolean;
      Title : String;
   package BTestcase is
   end BTestcase;

   generic
      with procedure Check;
      Title : String;
   package Fail_Testcase is
   end Fail_Testcase;

   generic
      Title : String;
   package Stub_Testcase is
   end Stub_Testcase;

   generic
      Title : String;
      with package Variable_Type is new AQC.Variables.Variable_Type (<>);
      Var : Variable_Type.Variable'Class;
      with function Property (X : Variable_Type.T) return Boolean;
   package VTestcase is
   end VTestcase;

   procedure End_Tests;

end AQC.Testcases;
