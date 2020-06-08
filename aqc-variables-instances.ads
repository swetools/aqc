with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with AQC.Generators.Instances;
with AQC.Generators.Strings;
with AQC.Generators.Strings.Instances;
with AQC.Variables.Variable_Type;
package AQC.Variables.Instances is

   package Bool_Variable is
      new
     AQC.Variables.Variable_Type (T => Boolean,
                                  Generator =>
                                    AQC.Generators.Instances.
                                    Arbitrary_Boolean.Generator,
                                  Image => Boolean'Image);

   package Integer_Variable is
      new
     AQC.Variables.Variable_Type (T => Integer,
                                  Generator =>
                                    AQC.Generators.Instances.
                                    Arbitrary_Integer.Generator,
                                  Image => Integer'Image);

   package ISO_646_String_Variable is
      new
     AQC.Variables.Variable_Type (T => Ada.Strings.Unbounded.Unbounded_String,
                                  Generator =>
                                    AQC.Generators.Strings.Instances.
                                    Unbounded_ISO_646_Generator,
                                  Image => AQC.Generators.Strings.Image);

   package UTF_8_String_Variable is
      new
     AQC.Variables.Variable_Type (T => Ada.Strings.Unbounded.Unbounded_String,
                                  Generator =>
                                    AQC.Generators.Strings.Instances.
                                    Unbounded_UTF_8_Generator,
                                  Image => AQC.Generators.Strings.Image);

   package Wide_Wide_String_Variable is
      new
     AQC.Variables.Variable_Type (T => Ada.Strings.Wide_Wide_Unbounded.
                                    Unbounded_Wide_Wide_String,
                                  Generator =>
                                    AQC.Generators.Strings.Instances.
                                    Wide_Wide_Plain_Unbounded_Generator,
                                  Image => AQC.Generators.Strings.Image);

end AQC.Variables.Instances;
