use "homework1.sml";

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

(*val test10 = month_range(31, 34) = [1,2,2,2]*)

(*val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)*)

