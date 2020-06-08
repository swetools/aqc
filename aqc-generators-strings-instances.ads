package AQC.Generators.Strings.Instances is

   function Unbounded_ISO_646_Generator is
      new Unbounded_Generator (ISO_646_Generator);

   function Unbounded_UTF_8_Generator is
      new Unbounded_Generator (UTF_8_Generator);

   function Wide_Wide_Plain_Unbounded_Generator is
      new Wide_Wide_Unbounded_Generator (Wide_Wide_Generator);

end AQC.Generators.Strings.Instances;
