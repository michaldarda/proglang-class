use "hw3provided.sml";

exception fail;
val test_count = ref 0;
val fail_count = ref 0;

fun test f args actual expected =
(
  if actual <> expected then
    (print "***\n*** FAILURE:\n***\n" ; fail_count := !fail_count + 1)
  else
    ( )
  ; test_count := !test_count + 1
  ; (actual = expected, args, actual, expected)
);

(* test normal flow *)
fun test_f1 f a1    expected = test f (a1)     (f a1)    expected;
fun test_f2 f a1 a2 expected = test f (a1, a2) (f a1 a2) expected;

(* test exceptional flow *)
fun test_e1 f a1    e = ((f a1)    ; raise fail) handle e => e;
fun test_e2 f a1 a2 e = ((f a1 a2) ; raise fail) handle e => e;

test_f1 only_capitals [ ] [ ];
test_f1 only_capitals ["a"] [ ];
test_f1 only_capitals ["a", "Bx", "Cx"] ["Bx", "Cx"];

test_f1 longest_string1 [ ] "";
test_f1 longest_string1 ["a", "bb"] "bb";
test_f1 longest_string1 ["aa", "bb"] "aa";

test_f1 longest_string2 [ ] "";
test_f1 longest_string2 ["a", "bb"] "bb";
test_f1 longest_string2 ["aa", "bb"] "bb";

test_f1 longest_string3 [ ] "";
test_f1 longest_string3 ["a", "bb"] "bb";
test_f1 longest_string3 ["aa", "bb"] "aa";

test_f1 longest_string4 [ ] "";
test_f1 longest_string4 ["a", "bb"] "bb";
test_f1 longest_string4 ["aa", "bb"] "bb";

test_f1 longest_capitalized [ ] "";
test_f1 longest_capitalized ["ax"] "";
test_f1 longest_capitalized ["Ax", "Bxx"] "Bxx";
test_f1 longest_capitalized ["ax", "Bx"] "Bx";
test_f1 longest_capitalized ["Ax", "Bx"] "Ax";

test_f1 rev_string "" "";
test_f1 rev_string "a" "a";
test_f1 rev_string "abcd" "dcba";

fun qq n = if n = 30 then NONE else SOME n;
test_f2 first_answer qq [1] 1;
test_f2 first_answer qq [30, 30, 30, 1, 4] 1;
test_e2 first_answer qq [ ] NoAnswer;
test_e2 first_answer qq [30, 30] NoAnswer;

fun qq n = if n = 30 then NONE else SOME [n];
test_f2 all_answers qq [ ] (SOME [ ]);
test_f2 all_answers qq [1, 2] (SOME [2, 1]);
test_f2 all_answers qq [1, 2, 30] NONE;

test_f1 count_wildcards Wildcard 1;
test_f1 count_wildcards (Variable "x") 0;
test_f1 count_wildcards UnitP 0;
test_f1 count_wildcards (ConstP 17) 0;
test_f1 count_wildcards (TupleP [Wildcard, UnitP, Wildcard]) 2;
test_f1 count_wildcards (ConstructorP("boo", TupleP [Wildcard, Variable "x"])) 1;

test_f1 count_wild_and_variable_lengths Wildcard 1;
test_f1 count_wild_and_variable_lengths (Variable "boo") 3;
test_f1 count_wild_and_variable_lengths (TupleP [Wildcard, Variable "boo"]) 4;

test_f1 count_some_var ("x", Wildcard) 0;
test_f1 count_some_var ("x", (Variable "x")) 1;
test_f1 count_some_var ("x", (Variable "y")) 0;
test_f1 count_some_var ("x", (TupleP [Variable "x", Variable "y", Variable "x"])) 2;

test_f1 check_pat Wildcard true;
test_f1 check_pat (Variable "x") true;
test_f1 check_pat (TupleP [Variable "x", Variable "y"]) true;
test_f1 check_pat (TupleP [Variable "x", Variable "x"]) false;

test_f1 match (Unit, Wildcard) (SOME [ ]);
test_f1 match (Const 1, Wildcard) (SOME [ ]);
test_f1 match (Tuple [Unit, Const 1], Wildcard) (SOME [ ]);
test_f1 match (Constructor ("x", Unit), Wildcard) (SOME [ ]);

test_f1 match (Const 1, Variable "x") (SOME [("x", Const 1)]);
test_f1 match (Unit, Variable "x") (SOME [("x", Unit)]);
test_f1 match (Tuple [Unit, Unit], Variable "x") (SOME[("x",Tuple [Unit, Unit])]);
test_f1 match (Constructor("x", Unit), Variable "x") (SOME[("x", Constructor("x", Unit))]);

test_f1 match (Const 1, ConstP 1) (SOME [ ]);
test_f1 match (Const 1, ConstP 2) NONE;
test_f1 match (Tuple [ ], ConstP 1) NONE;
test_f1 match (Constructor("x", Unit), ConstP 1) NONE;
test_f1 match (Tuple [Unit], TupleP [UnitP]) (SOME [ ]);
test_f1 match (Tuple [Unit], TupleP [UnitP, UnitP]) NONE;

test_f1 match (Tuple [ ], TupleP [ ]) (SOME [ ]);
test_f1 match (Tuple [Unit], TupleP [UnitP]) (SOME [ ]);
test_f1 match (Tuple [Unit], TupleP [Variable "x"]) (SOME [("x", Unit)]);
test_f1 match (Tuple [Unit], TupleP [ConstP 1]) NONE;

test_f1 match (Constructor ("a", Unit), ConstructorP ("a", UnitP)) (SOME [ ]);
test_f1 match (Constructor ("a", Unit), ConstructorP ("a", Variable "x")) (SOME [("x", Unit)]);
test_f1 match (Constructor ("a", Unit), ConstructorP ("b", Variable "x")) NONE;
test_f1 match (Constructor ("a", Unit), ConstructorP ("a", ConstP 1)) NONE;

test_f2 first_match (Const 7) [ConstP 7] (SOME [ ]);
test_f2 first_match (Const 7) [ConstP 0] NONE;
test_f2 first_match (Const 7) [Variable "a", Variable "b"] (SOME [("a", Const 7)]);

print( "\n\n*****************\n"
       ^"Number of tests: " ^ (Int.toString (!test_count))
       ^" Failed tests: " ^ (Int.toString (!fail_count))
       ^"\n");
