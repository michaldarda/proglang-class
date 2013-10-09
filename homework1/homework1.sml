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

(*fun oldest(dates : (int : int : int) list) =
    fun_oldest_iter(dates : (int : int : int) list, current_oldest : (int * int * int)) =
        if null dates then
            NONE
        else if null tl(dates) then
            SOME hd(dates)
        else*)
