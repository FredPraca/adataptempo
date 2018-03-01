with Parse_Args; use Parse_Args;
with Parse_Args.Generic_Discrete_Options;

with ZanyBlue.Text.Generic_Integers;

with TapTempo; use TapTempo;

package Options is
   package Precision_Option is new Parse_Args.Generic_Discrete_Options
     (Precision,
      5);

   package Precision_Formatting is
      new ZanyBlue.Text.Generic_Integers (Precision);

   Precision_Option_Name   : constant String := "precision";
   Reset_Time_Option_Name  : constant String := "reset-time";
   Sample_Size_Option_Name : constant String := "sample-size";

   procedure Prepare_Options (Parser : out Argument_Parser);

end Options;
