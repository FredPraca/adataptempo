with Ada.Containers; use Ada.Containers;

package TapTempo is

   subtype Sample_Size is Count_Type;
   type Seconds is new Positive;
   type Precision is range 0 .. 5;

   procedure Run
     (Size                 : Sample_Size := 5;
      Reset_Time_In_Second : Seconds     := 5;
      Precision_Needed     : Precision   := 0);

end TapTempo;
