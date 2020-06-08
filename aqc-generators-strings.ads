with Ada.Characters.Handling;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with AQC.Generators.Array_Generator;
with AQC.Generators.Instances;
package AQC.Generators.Strings is

   subtype ISO_646_String is String
     with Dynamic_Predicate =>
     Ada.Characters.Handling.Is_ISO_646 (ISO_646_String);

   function Image (X : String) return String;
   function Image (X : Wide_Wide_String) return String;

   function Image (X : Ada.Strings.Unbounded.Unbounded_String) return String
   is (Image (Ada.Strings.Unbounded.To_String (X)));

   function Image (X : Ada.Strings.Wide_Wide_Unbounded.
                     Unbounded_Wide_Wide_String)
                  return String
   is (Image (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (X)));

   function Wide_Wide_Generator is
      new AQC.Generators.Array_Generator (Element_Type => Wide_Wide_Character,
                                          Index_Type => Positive,
                                          T => Wide_Wide_String,
                                          Element_Generator =>
                                            AQC.Generators.Instances.
                                            Arbitrary_Wide_Wide_Character.
                                            Generator);

   function ISO_646_Generator is
      new AQC.Generators.Array_Generator (Element_Type => Character,
                                          Index_Type => Positive,
                                          T => ISO_646_String,
                                          Element_Generator =>
                                            AQC.Generators.Instances.
                                            Arbitrary_ISO_646.Generator);

   function UTF_8_Generator (Credits : in out Credit)
                            return Ada.Strings.UTF_Encoding.UTF_8_String;


   generic
      with function Generator (Credits : in out Credit) return String;
   function Unbounded_Generator (Credits : in out Credit)
                                return Ada.Strings.Unbounded.
     Unbounded_String;

   generic
      with function Generator (Credits : in out Credit)
                              return Wide_Wide_String;
   function Wide_Wide_Unbounded_Generator (Credits : in out Credit)
                                          return Ada.Strings.
     Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

end AQC.Generators.Strings;
