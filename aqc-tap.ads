private package AQC.TAP is
   type Outcome is (Ok, Fail, Skip, Todo);

   procedure Log (Msg : String);
   procedure Verdict (N : Positive;
                      Status : Outcome;
                      Msg : String;
                      Details : String := "");
   procedure Bailout (Msg : String := "") with Unreferenced;

   procedure Test_Plan (Min : Positive; Max : Positive);
end AQC.TAP;
