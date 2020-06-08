with Ada.Numerics.Discrete_Random;
with AQC.Generators.Utils;
use AQC.Generators.Utils;
function AQC.Generators.Array_Generator (Credits : in out Credit) return T
is
begin
   Consume_Credits (Credits);
   declare
      subtype Upper_Limit is Index_Type'Base range
        Index_Type'Pred (Index_Type'First) ..
        Index_Type'Min (Index_Type'Val (Index_Type'Pos (Index_Type'First) +
                                          Credits - 1),
                        Index_Type'Last);
      package Random is new Ada.Numerics.Discrete_Random (Upper_Limit);
      Random_Gen : Random.Generator;
   begin
      Random.Reset (Random_Gen);
      declare
         X : T (Index_Type'First .. Random.Random (Random_Gen));
      begin
         for I in X'Range loop
            declare
               Element_Credits : Credit :=
                 (if Distribute_Credits then
                 Credits / X'Length else Credits);
            begin
               if Element_Credits = 0
               then
                  Element_Credits := 1;
               end if;
               X (I) := Element_Generator (Element_Credits);
            end;
         end loop;
         if not Single_Credit and X'Length > 1 then
            Consume_Credits (Credits, X'Length - 1);
         end if;
         return X;
      end;
   end;
end AQC.Generators.Array_Generator;
