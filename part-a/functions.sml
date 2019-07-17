

fun pow(x : int, y : int) =
    if y = 0
    then 1
    else x * pow(x, y - 1);

fun cube(x : int) =
    pow(x, 3);

(* functions seem to basically just be a way of generalizing an expression so that you can plug in different values into the expression without writing them again. *)

(* A function is already a value. The function name and its value are added to the environment so that later expressions can call it. The function body is not evaluated until the function is called. *)
