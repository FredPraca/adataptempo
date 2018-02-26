package body Options is
   
   procedure Prepare_Options(Parser : out Argument_Parser) Is
   begin
      -- Options creation
      Parser.Add_Option(Make_Boolean_Option (False), "help", 'h', Usage =>
		      "display this help message");
      Parser.Add_Option(Options.Precision_Option.Make_Option, Precision_Option_Name, 'p', Usage =>
		      "set the decimal precision of the tempo display, default is 5 and max is " &
		      Precision'Image (Precision'Last));
      Parser.Add_Option(Make_Positive_Option (5), Reset_Time_Option_Name, 'r', Usage =>
		      "set the time in second to reset the computation. default is 5 seconds");
      Parser.Add_Option(Make_Positive_Option (1), Sample_Size_Option_Name, 's', Usage =>
		      "set the number of samples needed to compute the tempo, default is 1 sample");
      Parser.Set_Prologue ("An Ada version of taptempo.");
   end Prepare_Options;   
end Options;
