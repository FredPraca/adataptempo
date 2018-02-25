with TapTempo; use TapTempo;
with Options;

with Parse_Args; use Parse_Args;

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;

procedure AdaTapTempo is
   
   Precision_Option_Name : constant String := "precision";
   Reset_Time_Option_Name : constant String := "reset-time";
   Sample_Size_Option_Name : constant String := "sample-size";
     
   AP : Argument_Parser;
begin
   -- Options creation
   AP.Add_Option(Make_Boolean_Option (False), "help", 'h', Usage =>
      "display this help message");
   AP.Add_Option(Options.Precision_Option.Make_Option, Precision_Option_Name, 'p', Usage =>
      "set the decimal precision of the tempo display, default is 5 and max is " &
      Precision'Image (Precision'Last));
   AP.Add_Option(Make_Positive_Option (5), Reset_Time_Option_Name, 'r', Usage =>
      "set the time in second to reset the computation. default is 5 seconds");
   AP.Add_Option(Make_Positive_Option (1), Sample_Size_Option_Name, 's', Usage =>
      "set the number of samples needed to compute the tempo, default is 1 sample");
   AP.Set_Prologue ("An Ada version of taptempo.");
   
   -- We parse the command line
   AP.Parse_Command_Line;

   if AP.Parse_Success and then AP.Boolean_Value ("help") then
      AP.Usage;
   elsif AP.Parse_Success then
      declare
	 -- If success, let's build the tap tempo
         Tempo : Tap_Tempo := Build(Count_Type (Integer_Value (AP, Sample_Size_Option_Name)),
				    Seconds (Integer_Value (AP, Reset_Time_Option_Name)),
				    Options.Precision_Option.Value (AP, Precision_Option_Name));
      begin
	 -- Let's run !!
         Run (Tempo);
      end;
   else
      Put_Line("Error while parsing command-line arguments: " & AP.Parse_Message);
   end if;

end AdaTapTempo;
