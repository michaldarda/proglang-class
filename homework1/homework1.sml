fun is_older(date1 : int * int * int, date2 : int * int * int ) =
    let
        fun in_days (date : int * int * int) =
            #1(date) * 365 + #2(date) * 31 + #3(date)
    in
        in_days(date1) < in_days(date2)
    end

fun number_in_month(dates : (int * int * int) list, month : int) =
    let
        fun number_in_month_iter (dates : (int * int * int) list, month : int, accu : int) =
            if null(dates)
            then accu
            else if #2(hd(dates)) = month
            then number_in_month_iter(tl(dates), month, accu + 1)
            else number_in_month_iter(tl(dates), month, accu)
    in
        number_in_month_iter(dates, month, 0)
    end

fun number_in_months(dates : (int * int * int) list, months : int list) =
    let
        fun number_in_months_iter(months : int list, accu : int) =
            if null(months)
            then accu
            else number_in_months_iter(tl(months), accu + number_in_month(dates, hd(months)))
    in
        number_in_months_iter(months, 0)
    end

fun dates_in_month(dates : (int * int * int) list, month : int) =
    let
        fun dates_in_month_iter(dates: (int * int * int) list, accu: (int * int * int) list) =
            if null(dates)
            then accu
            else if #2(hd(dates)) = month
            then dates_in_month_iter(tl(dates), hd(dates) :: accu)
            else dates_in_month_iter(tl(dates), accu)
    in
        rev(dates_in_month_iter(dates, []))
    end

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    let
        fun dates_in_months_iter(dates : (int * int * int) list, months : int list, accu : (int * int * int) list) =
            if null months
            then accu
            else dates_in_months_iter(dates, tl(months), accu @ dates_in_month(dates, hd(months)))
    in
        dates_in_months_iter(dates, months, [])
    end

fun get_nth(strings : string list, n : int) =
    if n = 1
    then hd(strings)
    else get_nth(tl(strings), n - 1)

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
        ];

        val month_name = get_nth(months, #2(date));
        val day = Int.toString(#3(date));
        val year = Int.toString(#1(date));
    in
        month_name ^ " " ^ day ^ ", " ^ year
    end

fun number_before_reaching_sum (sum : int, numbers : int list) =
    let
        fun number_before_reaching_sum_iter(numbers : int list, current_sum : int, count : int) =
            if null(numbers) orelse hd(numbers) + current_sum >= sum
            then count
            else number_before_reaching_sum_iter(tl(numbers), current_sum + hd(numbers), count + 1)
    in
        number_before_reaching_sum_iter(numbers, 0, 0)
    end

fun what_month(day_of_year : int) =
    let val months_lengths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day_of_year, months_lengths) + 1
    end


fun month_range(day1 : int, day2 : int) =
    let
        fun count (from : int, to : int) =
            if from > to
            then []
            else if from = to
            then to :: []
            else from :: count(from + 1, to)

        fun month_range_iter(days : int list, accu : int list) =
            if null days
            then accu
            else month_range_iter(tl(days), what_month(hd(days)) :: accu)
    in
        rev(month_range_iter(count(day1, day2), []))
    end

fun oldest(dates : (int * int * int) list) =
    if null(dates)
    then NONE
    else let fun oldest_iter(dates : (int * int * int) list, current_oldest : (int * int * int)) =
        if null(dates)
        then current_oldest
        else if is_older(hd(dates), current_oldest)
        then oldest_iter(tl(dates), hd(dates))
        else oldest_iter(tl(dates), current_oldest)
    in
        SOME(oldest_iter(tl(dates), hd(dates)))
    end

