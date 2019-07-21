(* 1 *)
fun alternate (xs : int list) =
    let
	fun negate (x : int) = ~x;
	fun alternate_signs (xs : int list, subtract : bool) =
	    if null xs
	    then []
	    else
		(if subtract
		then negate (hd xs)
		 else (hd xs))
		:: (alternate_signs ((tl xs), not subtract));
	fun sum_list (xs : int list) =
	    if null xs
	    then 0
	    else (hd xs) + (sum_list (tl xs));
    in
	sum_list (alternate_signs (xs, false))
    end;

(* 2 *)
fun min_max (xs : int list) =
    let
	fun min (xs : int list) =
	    if null (tl xs)
	    then (hd xs)
	    else
		let
		    val minimum = min (tl xs)
		in
		    if (hd xs) < minimum
		    then (hd xs)
		    else minimum
		end
	fun max (xs : int list) =
	    if null (tl xs)
	    then (hd xs)
	    else
		let
		    val maximum = max (tl xs)
		in
		    if (hd xs) > maximum
		    then (hd xs)
		    else maximum
		end
    in
	(min xs, max xs)
    end;
