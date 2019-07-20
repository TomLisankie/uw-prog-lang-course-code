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
