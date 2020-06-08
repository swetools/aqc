with Ada.Strings.Unbounded;
with AQC.Variables.Classifier_Type;
with AQC.Variables.Instances;
with AQC.SelfTest.Variables;
with AQC.Testcases;
use AQC.Testcases;
private package AQC.SelfTest.Classifiers is

   function Bool_Identity (X : Boolean) return Boolean is (X);

   package Trivial_Bool_Classifier is
      new
     AQC.Variables.Classifier_Type (Variable_Type =>
                                      AQC.Variables.Instances.Bool_Variable,
                                    Class => Boolean,
                                    Get_Class => Bool_Identity);

   Bool_Cls :
     constant Trivial_Bool_Classifier.Classifier :=
     (Name => Ada.Strings.Unbounded.To_Unbounded_String ("BC"),
      others => <>);

   package Bool_Param_Class_Test_TC is
      new VTestcase (Title =>
                       "A trivial classified boolean parametric test",
                     Variable_Type => AQC.Variables.Instances.Bool_Variable,
                     Var => Bool_Cls,
                     Property => AQC.SelfTest.Variables.Bool_Param_Check);

   Bool_Cls_Min :
     constant Trivial_Bool_Classifier.Classifier :=
     (Name => Ada.Strings.Unbounded.To_Unbounded_String ("BC"),
      Min_Iterations => 1,
      others => <>);

   package Bool_Param_Class_Min_Test_TC is
      new VTestcase (Title =>
                       "A trivial classified boolean parametric test " &
                       "(minimal)",
                     Variable_Type => AQC.Variables.Instances.Bool_Variable,
                     Var => Bool_Cls_Min,
                     Property => AQC.SelfTest.Variables.Bool_Param_Check);

end AQC.SelfTest.Classifiers;
