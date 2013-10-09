fun is_older (date1 : int * int * int, date2 : int * int * int ) =
    let
        fun in_days (date : int * int * int) =
            #1(date) * 365 + #2(date) * 31 + #3(date)
    in
        in_days(date1) < in_days(date2)
    end

fun number_in_month (dates : (int * int * int) list, month : int) =
    let
        fun number_in_month_iter (dates : (int * int * int) list, month : int, accu : int) =
            if null dates then
                accu
            else if #2(hd(dates)) = month then
                number_in_month_iter(tl(dates), month, accu + 1)
            else
                number_in_month_iter(tl(dates), month, accu)
    in
        number_in_month_iter(dates, month, 0)
    end

fun number_in_months (dates : (int * int * int) list, months : int list) =
    let
        fun number_in_months_iter (dates : (int * int * int) list, months : int list, accu : int) =
            if null months then
                accu
            else
                number_in_months_iter(dates, tl(months), accu + number_in_month(dates, hd(months)))
    in
        number_in_months_iter(dates, months, 0)
    end

fun dates_in_month (dates : (int * int * int) list, month : int) =
    let
        fun dates_in_month_iter(dates: (int * int * int) list, month : int, accu: (int * int * int) list) =
            if null dates then
                accu
            else if #2(hd(dates)) = month then
                dates_in_month_iter(tl(dates),month, hd(dates) :: accu)
            else
                dates_in_month_iter(tl(dates),month, accu)
    in
        dates_in_month_iter(dates, month, [])
    end

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    let
        fun dates_in_months_iter (dates : (int * int * int) list, months : int list, accu : (int * int * int) list) =
            if null months then
                accu
            else
                dates_in_months_iter(dates, tl(months), accu @ dates_in_month(dates, hd(months)))
    in
        dates_in_months_iter(dates, months, [])
    end

fun get_nth(strings : string list, n : int) =
    if n = 1 then
        hd(strings)
    else
        get_nth(tl(strings), n - 1)

fun date_to_string (date : int * int * int) =
    let
        val months = [
            "January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "Semptember",
            "October",
            "November",
            "December"
        ]
    in
        get_nth(months, #2(date)) ^ " " ^ Int.toString(#3(date)) ^ ", " ^ Int.toString(#1(date))
    end

fun number_before_reaching_sum (sum : int, numbers : int list) =
    let
        fun number_before_reaching_sum_iter(sum : int, numbers : int list, accu : int, current_sum : int) =
            if null numbers orelse hd(numbers) + current_sum >= sum then
                accu
            else
                number_before_reaching_sum_iter(sum, tl(numbers), accu + 1, current_sum + hd(numbers))
    in
        number_before_reaching_sum_iter(sum, numbers, 0, 0)
    end

fun what_month(day_of_year : int) =
    number_before_reaching_sum(day_of_year, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1

fun oldest(dates : (int : int : int) list) =
    fun_oldest_iter(dates : (int : int : int) list, current_oldest : (int * int * int)) =
        if null dates then
            NONE
        else if null tl(dates) then
            SOME hd(dates)
        else
(* test suite *)

val is_older1 = is_older((2013,8,10),(2014,8,10)) = true;
val is_older2 = is_older((2013,8,10),(2013,9,10)) = true;
val is_older3 = is_older((2013,8,10),(2013,8,11)) = true;
val is_older4 = is_older((2013,8,10),(2013,8,10)) = false;

val number_in_month1 = number_in_month([(2012,2,28),(2013,12,1)],2) = 1;
val number_in_month2 = number_in_month([(2012,2,28),(2018,2,13),(2013,12,1)],2) = 2;
val number_in_month3 = number_in_month([(2013,3,14),(2012,10,30)],8) = 0;
val number_in_month4 = number_in_month([],8) = 0;
val number_in_month5 = number_in_month([(2012,2,28)],2) = 1;

val number_in_months1 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3;

val dates_in_month1 = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)];

val dates_in_months1 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val get_nth1 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there";

val date_to_string1 = date_to_string((2013, 6, 1)) = "June 1, 2013";

val number_before_reaching_sum1 = number_before_reaching_sum(10, [1,2,3,4,5]);

val whatwhat = what_month(70);
val whatmonth1 = what_month(70) = 3;

(*val test10 = month_range(31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
*)
