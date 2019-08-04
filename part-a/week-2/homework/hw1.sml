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
