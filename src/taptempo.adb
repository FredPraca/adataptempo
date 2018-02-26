with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Characters.Latin_1;
with Ada.Containers;      use Ada.Containers;

package body TapTempo is
   
   -- Time vector type
   package Time_Vectors is new Ada.Containers.Vectors(Index_Type   => Positive,
						      Element_Type => Time);
   use Time_Vectors;
   
   Time_Vector          : Time_Vectors.Vector;
      
   function Is_Reset_Time_Elapsed
     (Reset_Time_In_Second : Seconds;
      Current_Time         : Time;
      Last_Time            : Time) return Boolean
   is
      Elapsed_Time : Time_Span := Current_Time - Last_Time;
   begin
      return Elapsed_Time > Ada.Real_Time.Seconds (Integer (Reset_Time_In_Second));
   end Is_Reset_Time_Elapsed;

   function Compute_BPM
     (Current_Time    : Time;
      Last_Time       : Time;
      Occurence_Count : Count_Type) return Float
   is
      Elapsed_Time : Time_Span := Current_Time - Last_Time;
      Mean_Time    : Time_Span := Elapsed_Time / Integer (Occurence_Count);
   begin
      return Float (60.0 / To_Duration (Mean_Time));
   end Compute_BPM;

   ---------
   -- Run --
   ---------
   procedure Run
     (Size                 : Sample_Size := 5;
      Reset_Time_In_Second : Seconds     := 5;
      Precision_Needed     : Precision   := 0)
   is
      Should_Continue : Boolean := True;
   begin
      Put_Line ("Hit enter key for each beat (q to quit).");

      Continue_Loop :
      while Should_Continue loop
         declare
            Key : Character;
         begin
            Get_Key_Loop :
            loop
               Get_Immediate (Key);
               if (Key = 'q') then
                  Should_Continue := False;
                  Put_Line ("Bye Bye!");
                  exit Get_Key_Loop;
               end if;

               exit when (Key = Ada.Characters.Latin_1.LF);
            end loop Get_Key_Loop;
         end;

         if (Should_Continue) then
            declare
               Current_Time : Time := Clock;
            begin
               -- Reset if the hit diff is too big.
               if (not Is_Empty (Time_Vector)
		     and then Is_Reset_Time_Elapsed(Reset_Time_In_Second,Current_Time,First_Element (Time_Vector)))
               then
                  -- Clear the history
                  Clear (Time_Vector);
               end if;

               Append (Time_Vector, Current_Time);
               if (Length (Time_Vector) > 1) then
                  declare
                     BPM : Float := Compute_BPM(Last_Element (Time_Vector),
						First_Element (Time_Vector),
						Length (Time_Vector) - 1);
                  begin
                     New_Line;
                     Put ("Tempo: ");
                     if (Precision_Needed = 0) then
                        Put (Integer (BPM));
                     else
                        Put (BPM, 3, Integer (Precision_Needed), 0);
                     end if;
                     Put_Line (" BPM");
                  end;
               else
                  New_Line;
                  Put_Line
                    ("[Hit enter key one more time to start bpm computation...]");
               end if;

               while (Length (Time_Vector) > Size) loop
                  Delete_First (Time_Vector);
               end loop;

            end;

         end if;

      end loop Continue_Loop;

   end Run;

end TapTempo;
