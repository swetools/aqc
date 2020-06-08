generic
   type T is (<>);
   with function Base_Generator (Credits : in out Credit) return T'Base;
function AQC.Generators.Subtype_Generator (Credits : in out Credit) return T;
