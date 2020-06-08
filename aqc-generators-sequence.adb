with Ada.Numerics.Discrete_Random;
with AQC.Generators.Utils;
use AQC.Generators.Utils;
package body AQC.Generators.Sequence is

   package Random is new Ada.Numerics.Discrete_Random (Index_Type);

   Default_Random_Generator : Random.Generator;

   function Generator (Credits : in out Credit) return T
   is
   begin
      Consume_Credits (Credits);
      return Values (Random.Random (Default_Random_Generator));
   end Generator;

begin

   Random.Reset (Default_Random_Generator);

end AQC.Generators.Sequence;
