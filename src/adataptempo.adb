with TapTempo; use TapTempo;
with Options;

with Parse_Args; use Parse_Args;

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;

procedure AdaTapTempo is
     
   AP : Argument_Parser;
begin
   Options.Prepare_Options(AP);
   
   -- We parse the command line
   AP.Parse_Command_Line;

   if AP.Parse_Success and then AP.Boolean_Value ("help") then
      AP.Usage;
   elsif AP.Parse_Success then
      declare
	 -- If success, let's build the tap tempo
         Tempo : Tap_Tempo := Build(Count_Type (Integer_Value (AP, Options.Sample_Size_Option_Name)),
				    Seconds (Integer_Value (AP, Options.Reset_Time_Option_Name)),
				    Options.Precision_Option.Value (AP, Options.Precision_Option_Name));
      begin
	 -- Let's run !!
         Run (Tempo);
      end;
   else
      Put_Line("Error while parsing command-line arguments: " & AP.Parse_Message);
   end if;

end AdaTapTempo;
