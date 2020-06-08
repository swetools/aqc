with AQC.Generators.Sequence;
with AQC.Variables.Variable_Type;
generic
   type T is private;
   type Index_Type is (<>);
   type Seq_Type is array (Index_Type) of T;
   Values : Seq_Type;
   with function Image (X : T) return String;
package AQC.Variables.Sequence is

   package Sequence is new AQC.Generators.Sequence (T => T,
                                                    Index_Type => Index_Type,
                                                    Seq_Type => Seq_Type,
                                                    Values => Values);
   package Variable_Type is
      new AQC.Variables.Variable_Type (T => T,
                                       Generator => Sequence.Generator,
                                       Image => Image);

end AQC.Variables.Sequence;
