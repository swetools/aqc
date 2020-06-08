with AQC.TAP;
package body AQC.Variables.Classifier_Type is

   function First (CV : Classifier) return Sample'Class is
      First_Sample : Sample := Sample (First (Variable (CV)));
      First_Class : constant Class := Get_Class (First_Sample.Current);
      First_Bins : Class_Counters;
   begin
      First_Bins (First_Class) := 1;
      if First_Sample.Need_Iterations = 0 and First_Bins'Length > 0 then
         First_Sample.Need_Iterations := 1;
      end if;
      return Classified_Sample'(First_Sample with Bins => First_Bins);
   end First;

   function Next (CV : Classifier; S : Sample'Class) return Sample'Class is
      Next_Sample : Sample := Sample (Next (Variable (CV), S));
      CS : Classified_Sample := Classified_Sample (S);
   begin
      if not Is_Valid_Sample (Next_Sample) then
         for I in CS.Bins'Range loop
            AQC.TAP.Log (I'Image & " = " & CS.Bins (I)'Image);
         end loop;
         return Classified_Sample'(Next_Sample with others => <>);
      else
         declare
            Current_Class : constant Class := Get_Class (Next_Sample.Current);
         begin
            CS.Bins (Current_Class) := CS.Bins (Current_Class) + 1;
            if Next_Sample.Need_Iterations = 0 and
              (for some X of CS.Bins => X = 0)
            then
               Next_Sample.Need_Iterations := 1;
            end if;
            return Classified_Sample'(Next_Sample with Bins => CS.Bins);
         end;
      end if;
   end Next;

end AQC.Variables.Classifier_Type;
