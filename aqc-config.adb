with Ada.Characters.Latin_1;
with GNAT.Command_Line;
with AQC.TAP;
package body AQC.Config is
begin
   loop
      case GNAT.Command_Line.Getopt ("l: m:") is
         when Ada.Characters.Latin_1.NUL => exit;
         when 'l' =>
            Max_Credits := Positive'Value (GNAT.Command_Line.Parameter);
         when 'm' =>
            Min_Iterations := Positive'Value (GNAT.Command_Line.Parameter);
         when others =>
            pragma Assert (False);
      end case;
   end loop;
exception
   when GNAT.Command_Line.Invalid_Switch =>
      AQC.TAP.Bailout ("Invalid switch " & GNAT.Command_Line.Full_Switch);
   when GNAT.Command_Line.Invalid_Parameter =>
      AQC.TAP.Bailout ("Missing parameter for " &
                         GNAT.Command_Line.Full_Switch);
   when Constraint_Error =>
      AQC.TAP.Bailout ("Invalid parameter value for " &
                         GNAT.Command_Line.Full_Switch);
end AQC.Config;
