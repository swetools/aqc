with Ada.Iterator_Interfaces;
with Ada.Strings.Unbounded;
with AQC.Generators;
with AQC.Config;
use AQC.Generators;
generic
   type T is private;
   with function Generator (Credits : in out Credit) return T;
   with function Image (X : T) return String;
package AQC.Variables.Variable_Type is

   type Sample is tagged record
      Has_Value : Boolean;
      Credits : Credit;
      Need_Iterations : Natural;
      Current : T;
   end record;

   function Is_Valid_Sample (X : Sample) return Boolean
   is (X.Has_Value);

   function Is_Valid_Sample_Class (X : Sample'Class) return Boolean
   is (Is_Valid_Sample (X));

   package Sample_Iterator is
      new Ada.Iterator_Interfaces (Cursor => Sample'Class,
                                   Has_Element => Is_Valid_Sample_Class);

   type Variable is new Sample_Iterator.Forward_Iterator with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
         Min_Iterations : Positive := AQC.Config.Min_Iterations;
         Max_Credits : Credit := Credit (AQC.Config.Max_Credits);
      end record
        with Default_Iterator => Self_Iterator,
             Iterator_Element => T,
             Constant_Indexing => Retrieve;

   overriding
   function First (V : Variable) return Sample'Class;

   overriding
   function Next (V : Variable; S : Sample'Class) return Sample'Class;

   function Retrieve (V : Variable; S : Sample'Class) return T
   is (S.Current) with Pre => Is_Valid_Sample_Class (S);

   function Self_Iterator (V : Variable)
                          return Sample_Iterator.Forward_Iterator'Class
   is (V);

end AQC.Variables.Variable_Type;
