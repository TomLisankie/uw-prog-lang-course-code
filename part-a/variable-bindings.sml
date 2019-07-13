(* ML Variable Bindings and Expressions video *)

(* these are variable bindings *)
val x = 34;
val y = 17;

val z = (x + y) + (y + 2);

(* There's two environments, the static environment and the dynamic environment *)
(* Static environment maps each variable/name to a type *)
(* Dynamic environment maps each variable/name to a value of the corresponding type of the variable/name in the static environment *)

(* conditional *)
val abs_of_z = if z < 0 then 0 - z else z;
(* type checking a conditional *)
(* 1. Make sure expression after if is a boolean *)
(* 2. Make sure the expressions of the branches are all of the same type *)
(* 3. Add that type to the static environment *)
(* Dynamic binding just binds to the value of whichever expression is returned by the if expression*)

(* calling a function *)
val abs_of_z_simpler = abs z; (* <-- abs is a function, z is the argument being passed in *)
