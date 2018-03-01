with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Characters.Latin_1;
with TapTempo_Messages.taptempo_Prints; use TapTempo_Messages.taptempo_Prints;

package body TapTempo is

   -----------
   -- Build --
   -----------

   function Build
     (Size                 : Sample_Size := 5;
      Reset_Time_In_Second : Seconds     := 5;
      Precision_Needed     : Precision   := 0) return Tap_Tempo
   is
      Returned : Tap_Tempo;
   begin
      Returned.Size                 := Size;
      Returned.Reset_Time_In_Second := Reset_Time_In_Second;
      Returned.Precision_Needed     := Precision_Needed;
      return Returned;
   end Build;

   function Is_Reset_Time_Elapsed
     (Tempo        : Tap_Tempo;
      Current_Time : Time;
      Last_Time    : Time) return Boolean;

   function Compute_BPM
     (Current_Time    : Time;
      Last_Time       : Time;
      Occurence_Count : Count_Type) return Float;

   function Is_Reset_Time_Elapsed
     (Tempo        : Tap_Tempo;
      Current_Time : Time;
      Last_Time    : Time) return Boolean
   is
      Elapsed_Time : constant Time_Span := Current_Time - Last_Time;
   begin
      return Elapsed_Time > Ada.Real_Time.Seconds (
                      Integer (Tempo.Reset_Time_In_Second)
                      );
   end Is_Reset_Time_Elapsed;

   function Compute_BPM
     (Current_Time    : Time;
      Last_Time       : Time;
      Occurence_Count : Count_Type) return Float
   is
      Elapsed_Time : constant Time_Span := Current_Time - Last_Time;
      Mean_Time    : constant Time_Span :=
             Elapsed_Time / Integer (Occurence_Count);
   begin
      return Float (60.0 / To_Duration (Mean_Time));
   end Compute_BPM;

   ---------
   -- Run --
   ---------

   procedure Run (Tempo : in out Tap_Tempo) is
      Should_Continue : Boolean := True;
   begin
      Print_hitKey;

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
                  Print_byeBye;
                  exit Get_Key_Loop;
               end if;

               exit Get_Key_Loop when (Key = Ada.Characters.Latin_1.LF);
            end loop Get_Key_Loop;
         end;

         if (Should_Continue) then
            declare
               Current_Time : constant Time := Clock;
            begin
               --  Reset if the hit diff is too big.
               if (not Is_Empty (Tempo.Time_Vector)
                   and then Is_Reset_Time_Elapsed (
                           Tempo,
                           Current_Time,
                           First_Element (Tempo.Time_Vector)))
               then
                  --  Clear the history
                  Clear (Tempo.Time_Vector);
               end if;

               Append (Tempo.Time_Vector, Current_Time);
               if (Length (Tempo.Time_Vector) > 1) then
                  declare
                     BPM : constant Float := Compute_BPM (
                           Last_Element (Tempo.Time_Vector),
                           First_Element (Tempo.Time_Vector),
                           Length (Tempo.Time_Vector) - 1);
                  begin
                     New_Line;
                     Print_tempo (With_NL => False);
                     if (Tempo.Precision_Needed = 0) then
                        Put (Integer (BPM));
                     else
                        Put (BPM, 3, Integer (Tempo.Precision_Needed), 0);
                     end if;
                     Put_Line (" BPM");
                  end;
               else
                  New_Line;
                  Print_startKey;
               end if;

               while (Length (Tempo.Time_Vector) > Tempo.Size) loop
                  Delete_First (Tempo.Time_Vector);
               end loop;

            end;

         end if;

      end loop Continue_Loop;

   end Run;

end TapTempo;
