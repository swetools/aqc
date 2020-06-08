package body AQC.Generators.Choice is

   Choice_Random_Generator : Ada.Numerics.Float_Random.Generator;

   function Generator (Credits : in out Credit) return T is
      P : constant Ada.Numerics.Float_Random.Uniformly_Distributed :=
        Ada.Numerics.Float_Random.Random (Choice_Random_Generator);
   begin
      if P < Threshold then
         return Generator1 (Credits);
      else
         return Generator2 (Credits);
      end if;
   end Generator;

begin
   Ada.Numerics.Float_Random.Reset (Choice_Random_Generator);
end AQC.Generators.Choice;
