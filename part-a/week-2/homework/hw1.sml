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
