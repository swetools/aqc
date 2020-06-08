function AQC.Generators.Filter_Generator (Credits : in out Credit) return T is
   Value : T;
begin
   loop
      Value := Base_Generator (Credits);
      exit when Predicate (Value);
   end loop;
   return Value;
end AQC.Generators.Filter_Generator;
