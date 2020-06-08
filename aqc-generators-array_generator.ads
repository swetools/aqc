generic
   type Element_Type is private;
   type Index_Type is (<>);
   type T is array (Index_Type range <>) of Element_Type;
   with function Element_Generator (Credits : in out Credit)
                                   return Element_Type;
   Single_Credit : Boolean := True;
   Distribute_Credits : Boolean := True;
function AQC.Generators.Array_Generator (Credits : in out Credit) return T;
