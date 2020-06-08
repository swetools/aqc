with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with AQC.Variables;
with AQC.Variables.Instances;
with AQC.Testcases;
use AQC.Testcases;
private package AQC.SelfTest.Variables is

   pragma Warnings (Off, "condition");
   function Param_Check (X : Integer) return Boolean is (X <= Integer'Last);
   pragma Warnings (On, "condition");

   Int_Var :
     constant AQC.Variables.Instances.Integer_Variable.Variable :=
     (Name => Ada.Strings.Unbounded.To_Unbounded_String ("X"),
      others => <>);

   package Param_Test_TC is
      new VTestcase (Title => "A trivial parametric test",
                     Variable_Type => AQC.Variables.Instances.Integer_Variable,
                     Var => Int_Var,
                     Property => Param_Check);

   pragma Warnings (Off, "condition");
   function Bool_Param_Check (X : Boolean) return Boolean is (X or not X);
   pragma Warnings (On, "condition");

   Bool_Var :
     constant AQC.Variables.Instances.Bool_Variable.Variable :=
     (Name => Ada.Strings.Unbounded.To_Unbounded_String ("B"),
      others => <>);

   package Bool_Param_Test_TC is
      new VTestcase (Title => "A trivial boolean parametric test",
                     Variable_Type => AQC.Variables.Instances.Bool_Variable,
                     Var => Bool_Var,
                     Property => Bool_Param_Check);

   String_Var :
     constant AQC.Variables.Instances.ISO_646_String_Variable.Variable :=
     (Name => Ada.Strings.Unbounded.To_Unbounded_String ("S"),
      others => <>);

   function Str_Check (X : Ada.Strings.Unbounded.Unbounded_String)
                      return Boolean
   is (Ada.Characters.Handling.Is_ISO_646
         (Ada.Strings.Unbounded.To_String (X)));

   package String_Param_Test_TC is
      new VTestcase (Title => "A trivial boolean parametric test",
                     Variable_Type => AQC.Variables.
                       Instances.ISO_646_String_Variable,
                     Var => String_Var,
                     Property => Str_Check);

end AQC.SelfTest.Variables;
