with Options_Messages.options_Strings; use Options_Messages.options_Strings;

package body Options is

   use Precision_Formatting;

   procedure Prepare_Options (Parser : out Argument_Parser) is
   begin
      --  Options creation
      Parser.Add_Option
      (Make_Boolean_Option (False), "help", 'h', Usage =>
         Format_displayHelp);
      Parser.Add_Option
      (Options.Precision_Option
         .Make_Option, Precision_Option_Name, 'p', Usage =>
         Format_precision (+Precision'Last));
      Parser.Add_Option
      (Make_Positive_Option (5), Reset_Time_Option_Name, 'r', Usage =>
         Format_resetTime);
      Parser.Add_Option
      (Make_Positive_Option (1), Sample_Size_Option_Name, 's', Usage =>
         Format_sampleSize);
      Parser.Set_Prologue (Format_prologue);
   end Prepare_Options;
end Options;
