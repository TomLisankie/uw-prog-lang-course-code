fun old_max (xs : int list) =
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else
	let
	    val tl_ans = old_max (tl xs)
	in
	    if hd xs > tl_ans
	    then hd xs
	    else tl_ans
	end;

(* fn : int list -> int option *)
fun new_max_1 (xs : int list) =
    if null xs
    then NONE
    else
	let
	    val tl_ans = new_max_1 (tl xs)
	in
	    if isSome tl_ans andalso  valOf tl_ans > hd xs
	    then tl_ans
	    else SOME (hd xs)
	end;

(* Same as new_max_2 except it avoids using valOf *)
fun new_max_2 (xs : int list) =
    if null xs
    then NONE
    else
	let
	    fun max_nonempty (xs : int list) =
		if null (tl xs)
		then hd xs
		else
		    let val tl_ans = max_nonempty (tl xs)
		    in
			if hd xs > tl_ans
			then hd xs
			else tl_ans
		    end
	in
	    SOME (max_nonempty xs)
	end;
