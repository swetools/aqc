with Ada.Numerics.Float_Random;
generic
   type T is private;
   with function Generator1 (Credits : in out Credit) return T;
   with function Generator2 (Credits : in out Credit) return T;
   Threshold : Ada.Numerics.Float_Random.Uniformly_Distributed := 0.5;
package AQC.Generators.Choice is

   function Generator (Credits : in out Credit) return T;

end AQC.Generators.Choice;
