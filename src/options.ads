with Parse_Args.Generic_Discrete_Options;
with TapTempo; use TapTempo;

package Options is
   package Precision_Option is new Parse_Args.Generic_Discrete_Options(Precision, 5);
end Options;
