with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
package body AQC.Generators.Strings is

   function Image (X : String) return String is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for C of X loop
         if not Ada.Characters.Handling.Is_Graphic (C) then
            Ada.Strings.Unbounded.Append (Result, "<" & C'Image & ">");
         else
            Ada.Strings.Unbounded.Append (Result, C);
         end if;
      end loop;
      return Ada.Strings.Unbounded.To_String (Result);
   end Image;

   function Image (X : Wide_Wide_String) return String is
   begin
      return Image (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (X));
   end Image;

   function UTF_8_Generator (Credits : in out Credit)
                            return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      return Ada.Strings.UTF_Encoding.
        Wide_Wide_Strings.Encode (Wide_Wide_Generator (Credits));
   end UTF_8_Generator;


   function Unbounded_Generator (Credits : in out Credit)
                                return Ada.Strings.Unbounded.
     Unbounded_String is
   begin
      return Ada.Strings.Unbounded.To_Unbounded_String (Generator (Credits));
   end Unbounded_Generator;

   function Wide_Wide_Unbounded_Generator (Credits : in out Credit)
                                          return Ada.Strings.
     Wide_Wide_Unbounded.Unbounded_Wide_Wide_String is
   begin
      return Ada.Strings.Wide_Wide_Unbounded.
        To_Unbounded_Wide_Wide_String (Generator (Credits));
   end Wide_Wide_Unbounded_Generator;

end AQC.Generators.Strings;
