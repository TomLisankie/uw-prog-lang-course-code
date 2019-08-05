(* 1 *)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
    let
	val year1 = (#1 date1)
	val month1 = (#2 date1)
	val day1 = (#3 date1)
	val year2 = (#1 date2)
	val month2 = (#2 date2)
	val day2 = (#3 date2)
    in
	if year1 < year2 then
	    true
	else
	    if year1 > year2 then
		false
	    else
		if month1 < month2 then
		    true
		else
		    if month1 > month2 then
			false
		    else
			if day1 < day2 then
			    true
			else
			    false
    end;

(* 2 *)
fun number_in_month (dates : (int * int * int) list, month : int) =
    let
	fun count_months (dates : (int * int * int) list, count : int) =
	    if null dates then
		count
	    else
		let
		    val current_month = (#2 (hd dates))
		in
		    if current_month = month then
			count_months ((tl dates), count + 1)
		    else
			count_months ((tl dates), count)
		end;
    in
	count_months (dates, 0)
    end;

(* 3 *)
fun number_in_months (dates : (int * int * int) list, months : int list) =
    let
	fun count_all_months (months : int list, total : int) =
	    if null months then
		total
	    else
		let
		    val month = (hd months)
		in
		    count_all_months ((tl months), total + number_in_month (dates, month))
		end;
    in
	count_all_months (months, 0)
    end;

(* 4 *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
    let
	fun make_date_list (dates : (int * int * int) list, filtered_dates : (int * int * int) list) =
	    if null dates then
		filtered_dates
	    else
		let
		    val current_date = (hd dates)
		    val current_month = (#2 current_date)
		in
		    if current_month = month then
			make_date_list ((tl dates), current_date :: filtered_dates)
		    else
			make_date_list ((tl dates), filtered_dates)
		end;
    in
	make_date_list (dates, [])
    end;

(* 5 *)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
    let
	fun make_date_list (months : int list, filtered_dates : (int * int * int) list) =
	    if null months then
		filtered_dates
	    else
		let
		    val current_month = (hd months)
		in
		    make_date_list ((tl months), filtered_dates @ dates_in_month (dates, current_month))
		end;
    in
	make_date_list (months, [])
    end;

(* 6 *)
fun get_nth (items : string list, n : int) =
    if n = 1 then
	(hd items)
    else
	get_nth ((tl items), n - 1);

(* 7 *)
fun date_to_string (date : (int * int * int)) =
    let
	val month_strings = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth (month_strings, (#2 date)) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end;

(* 8 *)
fun number_before_reaching_sum (sum : int, xs : int list) =
    let
	fun adding (current_sum : int, items : int list, prev_num : int) =
	    let
		val num_to_add = (hd items)
		val next_sum = current_sum + num_to_add
	    in
		if next_sum >= sum then
		    prev_num
		else
		    adding (next_sum, (tl items), num_to_add)
	    end
    in
	adding (0, xs, 0)
    end;

(* 9 *)
fun what_month (day : int) =
    if day > 365 orelse day < 1 then
	0
    else
	let
	    val month_day_counts = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	    (* Could not figure out for the life of me how to properly use the function developed in problem 8 for this problem *)
	    fun find_month_num (sum : int, month : int, days_in_months : int list) =
		if sum >= day then
		    month
		else
		    find_month_num (sum + (hd days_in_months), month + 1, (tl days_in_months))
	in
	    find_month_num (0, 0, month_day_counts)
	end;

(* 10 *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2 then
	[]
    else
	let
	    fun make_month_list (current_day : int, month_list : int list) =
		if current_day = (day2 + 1) then
		    month_list
		else
		    month_list @ make_month_list (current_day + 1, [what_month (current_day)])
	in
	    make_month_list (day1, [])
	end;

(* 11 *)
fun oldest (dates : (int * int * int) list) =
    if null dates then
	NONE
    else	
	let
	    fun find_oldest (current_oldest : (int * int * int), dates_being_examined : (int * int * int) list) =
		if null dates_being_examined then
		    SOME current_oldest
		else
		    let
			val examine = (hd dates_being_examined)
		    in
			if null dates_being_examined then
			    SOME current_oldest
			else
			    if is_older (examine, current_oldest) then
				find_oldest (examine, (tl dates_being_examined))
			    else
				find_oldest(current_oldest, (tl dates_being_examined))
		    end
	in
	    find_oldest((hd dates), dates)
	end;
