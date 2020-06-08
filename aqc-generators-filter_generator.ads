generic
   type T is private;
   with function Base_Generator (Credits : in out Credit) return T;
   with function Predicate (X : T) return Boolean;
function AQC.Generators.Filter_Generator (Credits : in out Credit) return T;
