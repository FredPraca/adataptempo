with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

with Ada.Real_Time; use Ada.Real_Time;

package TapTempo is
   type Tap_Tempo is private;

   subtype Sample_Size is Count_Type;
   type Seconds is new Positive;
   type Precision is range 0 .. 5;

   function Build
     (Size                 : Sample_Size := 5;
      Reset_Time_In_Second : Seconds     := 5;
      Precision_Needed     : Precision   := 0) return Tap_Tempo;

   procedure Run (Tempo : in out Tap_Tempo);

private

   -- Time vector type
   package Time_Vectors is new Ada.Containers.Vectors(Index_Type   => Positive,
						      Element_Type => Time);
   use Time_Vectors;

   type Tap_Tempo is record
      Size                 : Sample_Size := 1;
      Reset_Time_In_Second : Seconds     := 1;
      Precision_Needed     : Precision   := 1;
      Time_Vector          : Time_Vectors.Vector;
   end record;

end TapTempo;
