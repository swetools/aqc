with AQC.TAP;
with Ada.Exceptions;
package body AQC.Variables.Variable_Type is

   function Make_Sample (V : Variable;
                         Iterations : Positive;
                         Credits : Credit) return Sample;

   function Make_Sample (V : Variable; Iterations : Positive; Credits : Credit)
                        return Sample is
   begin
      declare
         Current_Credits : Credit := Credits;
         Value : constant T := Generator (Current_Credits);
      begin
         AQC.TAP.Log (Ada.Strings.Unbounded.To_String (V.Name) & " = " &
                        Image (Value));
         return Sample'(Has_Value => True,
                        Credits => Current_Credits,
                        Need_Iterations => Iterations - 1,
                        Current => Value);
      end;
   exception
      when E : Credits_Exhausted =>
         raise Credits_Exhausted with
           Ada.Strings.Unbounded.To_String (V.Name) & ": " &
           Ada.Exceptions.Exception_Message (E);
   end Make_Sample;

   function First (V : Variable) return Sample'Class is
   begin
      return Make_Sample (V, V.Min_Iterations, V.Max_Credits);
   end First;

   function Next (V : Variable; S : Sample'Class) return Sample'Class is
   begin
      if S.Need_Iterations = 0 then
         return Sample'(Has_Value => False,
                        Credits => 0,
                        Need_Iterations => 0,
                        Current => <>);
      else
         return Make_Sample (V, S.Need_Iterations, S.Credits);
      end if;
   end Next;

end AQC.Variables.Variable_Type;
