with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Task_Identification;
use Ada.Task_Identification;
package body AQC.TAP is

   procedure Log (Msg : String) is
   begin
      Ada.Text_IO.Put_Line ("# " & Msg);
   end Log;

   procedure Verdict (N : Positive;
                      Status : Outcome;
                      Msg : String;
                      Details : String := "") is
   begin
      case Status is
         when Ok | Skip =>
            Ada.Text_IO.Put ("ok ");
         when Fail | Todo =>
            Ada.Text_IO.Put ("not ok ");
      end case;
      Ada.Integer_Text_IO.Put (N, 0);
      Ada.Text_IO.Put (' ');
      Ada.Text_IO.Put (Msg);
      if Details'Length > 0 then
         Ada.Text_IO.Put (": " & Details);
      end if;
      case Status is
         when Skip =>
            Ada.Text_IO.Put (" # SKIP " & Msg);
         when Todo =>
            Ada.Text_IO.Put (" # TODO " & Msg);
         when others =>
            null;
      end case;
      Ada.Text_IO.New_Line;
   end Verdict;

   procedure Bailout (Msg : String := "") is
   begin
      Ada.Text_IO.Put_Line ("Bail out! " & Msg);
      Abort_Task (Environment_Task);
   end Bailout;

   procedure Test_Plan (Min : Positive; Max : Positive) is
   begin
      Ada.Integer_Text_IO.Put (Min, 0);
      Ada.Text_IO.Put ("..");
      Ada.Integer_Text_IO.Put (Max, 0);
      Ada.Text_IO.New_Line;
   end Test_Plan;

end AQC.TAP;
