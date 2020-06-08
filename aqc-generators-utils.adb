package body AQC.Generators.Utils is

   procedure Consume_Credits (Have : in out Credit; Need : Credit := 1) is
   begin
      if Have >= Need
      then
         Have := Have - Need;
      else
         raise Credits_Exhausted with
           ("Not enough credits, need " & Need'Image &
              ", have only " & Have'Image);
      end if;
   end Consume_Credits;

end AQC.Generators.Utils;
