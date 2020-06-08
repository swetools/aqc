with AQC.Variables.Variable_Type;
generic
   with package Variable_Type is new AQC.Variables.Variable_Type (<>);
   type Class is (<>);
   with function Get_Class (X : Variable_Type.T) return Class;
package AQC.Variables.Classifier_Type is
   use Variable_Type;

   type Class_Counters is array (Class) of Natural
     with Default_Component_Value => 0;

   type Classified_Sample is new Sample with
      record
         Bins : Class_Counters := (others => 0);
      end record;

   type Classifier is new Variable with null record;

   overriding
   function First (CV : Classifier) return Sample'Class;

   overriding
   function Next (CV : Classifier; S : Sample'Class) return Sample'Class
     with Pre => S in Classified_Sample;

   function Self_Iterator (CV : Classifier)
                          return Sample_Iterator.Forward_Iterator'Class
   is (CV);

end AQC.Variables.Classifier_Type;
