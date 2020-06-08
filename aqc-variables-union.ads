with Ada.Numerics.Float_Random;
with AQC.Generators;
with AQC.Generators.Choice;
with AQC.Variables.Variable_Type;
generic
   type T is private;
   with package Variable_Type1 is
     new AQC.Variables.Variable_Type (T => T, others => <>);
   with package Variable_Type2 is
     new AQC.Variables.Variable_Type (T => T, others => <>);
   Threshold : Ada.Numerics.Float_Random.Uniformly_Distributed := 0.5;
   with function Image (X : T) return String is Variable_Type1.Image;
package AQC.Variables.Union is

   package Choice is new AQC.Generators.Choice (T => T,
                                                Generator1 =>
                                                  Variable_Type1.Generator,
                                                Generator2 =>
                                                  Variable_Type2.Generator,
                                                Threshold => Threshold);
   package Variable_Type is
      new AQC.Variables.Variable_Type (T => T,
                                       Generator => Choice.Generator,
                                       Image => Image);

end AQC.Variables.Union;
