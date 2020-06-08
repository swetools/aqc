with Ada.Strings.Unbounded;
with AQC.Generators.Filter_Generator;
with AQC.Generators.Subtype_Generator;
with AQC.Generators.Instances;
with AQC.Variables.Variable_Type;
with AQC.Testcases;
use AQC.Testcases;
private package AQC.SelfTest.Filters is

   function Is_Even (X : Integer) return Boolean is (X mod 2 = 0);

   function Even_Generator is
      new AQC.Generators.Filter_Generator (T => Integer,
                                           Base_Generator =>
                                             AQC.Generators.Instances.
                                             Arbitrary_Integer.Generator,
                                           Predicate => Is_Even);

   package Even_Integer_Variable is
      new
     AQC.Variables.Variable_Type (T => Integer,
                                  Generator => Even_Generator,
                                  Image => Integer'Image);

   Even_Int_Var :
     constant Even_Integer_Variable.Variable :=
     (Name => Ada.Strings.Unbounded.To_Unbounded_String ("X"),
      others => <>);

   package Filter_Test_TC is
      new VTestcase (Title => "A filtering parametric test",
                     Variable_Type => Even_Integer_Variable,
                     Var => Even_Int_Var,
                     Property => Is_Even);

   subtype Even_Integer is Integer with
     Dynamic_Predicate => Is_Even (Even_Integer);

   function Even_Subtype_Generator is
      new AQC.Generators.Subtype_Generator (T => Even_Integer,
                                            Base_Generator =>
                                              AQC.Generators.Instances.
                                              Arbitrary_Integer.Generator);

   package Subtype_Even_Integer_Variable is
      new
     AQC.Variables.Variable_Type (T => Even_Integer,
                                  Generator => Even_Subtype_Generator,
                                  Image => Even_Integer'Image);

   Even_Sub_Int_Var :
     constant Subtype_Even_Integer_Variable.Variable :=
     (Name => Ada.Strings.Unbounded.To_Unbounded_String ("X"),
      others => <>);

   package Subtyping_Test_TC is
      new VTestcase (Title => "A subtyping parametric test",
                     Variable_Type => Subtype_Even_Integer_Variable,
                     Var => Even_Sub_Int_Var,
                     Property => Is_Even);

end AQC.SelfTest.Filters;
