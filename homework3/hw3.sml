(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals =
	List.filter(fn s => Char.isUpper(String.sub(s, 0)))

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val longest_string1 =
	List.foldl(fn(x, y) => if String.size(x) > String.size(y) then x else y) ""

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test21 = longest_string1 ["A","bc", "xd", "C"] = "bc"

val longest_string2 =
	List.foldl(fn(x, y) => if String.size(x) >= String.size(y) then x else y) ""

val test4 = longest_string2 ["A","bc", "xd", "C"] = "xd"

fun longest_string_helper f =
  List.foldl (fn (x, y) =>
    if f(String.size x, String.size y) then x else y) ""

val longest_string3 = longest_string_helper(fn(x,y) => x > y)
val longest_string4 = longest_string_helper(fn(x,y) => x >= y)

val test5 = longest_string3 ["A","bc", "xd", "C"] = "bc"
val test6 = longest_string4 ["A","bc", "xd", "C"] = "xd"

val longest_capitalized =
	longest_string3 o only_capitals

val test7 = longest_capitalized ["A","bc","C"] = "A";

val rev_string =
	String.implode o rev o String.explode

val test8 = rev_string "abc" = "cba";
val test9 = rev_string "michal" = "lahcim";

fun first_answer f xs =
	case xs of
		[] => raise NoAnswer
	| x :: xs' =>
		case f x of
			NONE => first_answer f xs'
		|	SOME v => v

val test10 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

fun all_answers f xs =
	let
		fun all_answers_helper(xs, acc) =
			case xs of
				[] => SOME(acc)
			| x :: xs' =>
				case f x of
					SOME(x) => all_answers_helper(xs', x @ acc)
				|	NONE => NONE
	in
		all_answers_helper(xs, [])
	end

val test11 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val count_wildcards =
	g (fn _ => 1) (fn _ => 0)
val test9a = count_wildcards Wildcard = 1;

fun count_wild_and_variable_lengths(p) =
	count_wildcards(p) + (g (fn _ => 0) (fn v => String.size(v)) p)

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

fun count_some_var(s, p) =
	g (fn _ => 0) (fn (v) => if s = v then 1 else 0) p

val test9c = count_some_var ("x", Variable("x")) = 1;

fun check_pat(p) =
	let
		fun strings_from_vars(p) =
			case p of
				Variable(s) => [s]
			| TupleP(xs) => List.foldl (fn (x,y) => y @ strings_from_vars(x) ) [] xs
			| _ => []

		fun has_repeats(xs) =
			case xs of
				[] => true
			|	(x :: xs') => not(List.exists (fn y => x = y) xs')
	in
		has_repeats(strings_from_vars(p))
	end

fun match(_, Wildcard) = SOME []
  | match(v, Variable s) = SOME [(s, v)]
  | match(Unit, UnitP) = SOME []
  | match(Const x, ConstP y) = if x = y then SOME [] else NONE
  | match(Tuple vs, TupleP ps) =
      if List.length(vs) = List.length(ps)
      then all_answers match(ListPair.zip(vs, ps))
      else NONE
  | match (Constructor(s1,v), ConstructorP(s2, p)) =
      if s1 = s2 then
        match(v, p)
      else
        NONE
  | match _ = NONE

fun first_match v ps =
  SOME(first_answer(fn x => match(v, x)) ps)
  handle NoAnswer => NONE
