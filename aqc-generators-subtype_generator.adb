function AQC.Generators.Subtype_Generator (Credits : in out Credit) return T is
   Value : T'Base;
begin
   loop
      Value := Base_Generator (Credits);
      exit when Value in T;
   end loop;
   return Value;
end AQC.Generators.Subtype_Generator;
