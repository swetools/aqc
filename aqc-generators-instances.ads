with Ada.Characters.Handling;
with AQC.Generators.Discrete;
package AQC.Generators.Instances is

   package Arbitrary_Integer is new AQC.Generators.Discrete (Integer);

   package Arbitrary_Positive is new AQC.Generators.Discrete (Positive);

   package Arbitrary_Natural is new AQC.Generators.Discrete (Natural);

   package Arbitrary_Boolean is new AQC.Generators.Discrete (Boolean);

   package Arbitrary_ISO_646 is
      new AQC.Generators.Discrete (Ada.Characters.Handling.ISO_646);

   package Arbitrary_Wide_Wide_Character is
      new AQC.Generators.Discrete (Wide_Wide_Character);

end AQC.Generators.Instances;
