generic
   type T is private;
   type Index_Type is (<>);
   type Seq_Type is array (Index_Type) of T;
   Values : Seq_Type;
package AQC.Generators.Sequence is

   function Generator (Credits : in out Credit) return T;

end AQC.Generators.Sequence;
