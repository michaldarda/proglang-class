(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

fun all_except_option (s : string, l : string list) =
  let fun all_except_option_iter(l, acc) =
    case l of
      [] => NONE
    | x::xs => if same_string (x, s)
                then SOME(rev acc @ xs)
                else all_except_option_iter(xs, x :: acc)
    in
      all_except_option_iter(l : string list, [])
    end

fun get_substitutions1(substitutions : string list list, s : string) =
  case substitutions of
    [] => []
  | (x :: xs) =>
    case all_except_option(s, x) of
      SOME(xs') => xs' @ get_substitutions1(xs, s)
    | NONE => get_substitutions1(xs, s)

fun get_substitutions2(substitutions : string list list, s : string) =
  let
    fun get_substitutions_iter(substitutions : string list list, accu : string list) =
      case substitutions of
        [] => accu
      | x :: xs =>
        case all_except_option(s, x) of
          SOME(xs') => get_substitutions_iter(xs, xs' @ accu)
        | NONE => get_substitutions_iter(xs, accu)
  in
    get_substitutions_iter(substitutions, [])
  end

fun similar_names(substitutions : string list list, full_name : {first:string,middle:string,last:string}) =
  case full_name of
    {first = x, middle = y, last = z} =>
      let
        fun all_combinations(names : string list) =
          case names of
            [] => []
          | (name :: xs) => {first = name, middle = y, last = z} :: all_combinations(xs)
      in
        full_name :: all_combinations(get_substitutions1(substitutions, x))
      end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(s : suit, r : rank) =
  case s of
    Spades   => Black
  | Clubs    => Black
  | Diamonds => Red
  | Hearts   => Red

fun card_value(s : suit, r : rank) =
  case r of
    Num(n) => n
  | Ace    => 11
  | _      => 10

fun remove_card(cs : card list, c : card, e : exn) =
  case cs of
    [] => raise e
  | x :: xs => if c = x
               then xs
               else remove_card(xs, c, e)

fun all_same_color(cs : card list) =
  case cs of
    [] => true
  | _ :: [] => true
  | head :: (neck :: rest) => if card_color(head) = card_color(neck)
                              then all_same_color(neck :: rest)
                              else false

fun sum_cards(cs : card list) =
  let
    fun sum_cards_iter(cs : card list, accu : int) =
      case cs of
        [] => accu
      | x :: xs => sum_cards_iter(xs, card_value(x) + accu)
  in
    sum_cards_iter(cs, 0)
  end

fun score(cs : card list, goal : int) =
  let
    val computed_score = sum_cards(cs)
    val score = if computed_score >= goal
                then 3 * (computed_score - goal)
                else goal - computed_score
    val final_score = if all_same_color(cs)
                      then score div 2
                      else score
  in
    final_score
  end

fun officiate(cl : card list, ml: move list, goal : int) =
  let
    fun officiate_helper(cl : card list, ml : move list, hands : card list) =
      case ml of
        [] => score(hands, goal)
      | (x :: xs) => case x of
                      Discard(c) => officiate_helper(cl, xs, remove_card(hands, c, IllegalMove))
                    | Draw => case cl of
                                [] => score(hands, goal)
                              | x' :: [] => score(x' :: hands, goal)
                              | x' :: xs' => let
                                              val s = score( x' :: hands, goal)
                                            in
                                              if s > goal
                                              then s
                                              else officiate_helper(xs', xs, x' :: hands)
                                            end
  in
    officiate_helper(cl, ml, [])
  end
