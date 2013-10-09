use "homework1.sml";

val is_older_test1 = is_older((2013,8,10),(2014,8,10)) = true;
val is_older_test2 = is_older((2013,8,10),(2013,9,10)) = true;
val is_older_test3 = is_older((2013,8,10),(2013,8,11)) = true;
val is_older_test4 = is_older((2013,8,10),(2013,8,10)) = false;

val number_in_month_test1 = number_in_month([(2012,2,28),(2013,12,1)],2) = 1;
val number_in_month_test2 = number_in_month([(2012,2,28),(2018,2,13),(2013,12,1)],2) = 2;
val number_in_month_test3 = number_in_month([(2013,3,14),(2012,10,30)],8) = 0;
val number_in_month_test4 = number_in_month([],8) = 0;
val number_in_month_test5 = number_in_month([(2012,2,28)],2) = 1;

val number_in_months_test1 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3;

val dates_in_month_test1 = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)];
val dates_in_month_test2 = dates_in_month([(1,2,98),(3,5,201),(1,17,83),(3,2,2412),(1,2,342),(1,2,98),(6,7,8)], 2) = [(1,2,98),(3,2,2412),(1,2,342),(1,2,98)];
val dates_in_month_test3 = dates_in_month([(1,2,98),(3,2,2412),(1,2,342),(1,2,98)], 2) = [(1,2,98),(3,2,2412),(1,2,342),(1,2,98)];

val dates_in_months_test1 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val get_nth_test1 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there";

val date_to_string_test1 = date_to_string((2013, 6, 1)) = "June 1, 2013";

val number_before_reaching_sum_test1 = number_before_reaching_sum(10, [1,2,3,4,5]);

val what_month_test1 = what_month(70) = 3;

val month_range_test1 = month_range(31, 34) = [1,2,2,2];
val month_range_test2 = month_range(5, 3) = [];

val oldest_test1 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31);
val oldest_test2 = oldest([(2012,2,28)]) = SOME (2012,2,28);
val oldest_test3 = oldest([]) = NONE;
