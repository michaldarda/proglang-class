use("hw3.sml");

val test11 = all_except_option("string", ["string"]) = SOME []
val test12 = all_except_option("string", ["string list"]) = NONE
val test13 = all_except_option("string", ["string", "list"]) = SOME ["list"]
val test14 = all_except_option("Fred", ["Fred", "Frederick", "Freddie"]) = SOME ["Frederick", "Freddie"]
val test15 = all_except_option("Fred", ["Jenniffer", "Elizabeth", "Bettie"]) = NONE
val test16 = all_except_option("Jeff", ["Geoff","Jeff","Jeffrey"]) = SOME(["Geoff", "Jeffrey"])

val test2 = get_substitutions1([["foo"],["there"]], "foo") = []
val test21 = get_substitutions1([["foo", "bar"],["there"]], "foo") = ["bar"]
val test22 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test23 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test3 = get_substitutions2([["foo"],["there"]], "foo") = []
val test31 = get_substitutions2([["foo", "bar"],["there"]], "foo") = ["bar"]
val test32 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Freddie","F","Fredrick"]
val test33 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Geoff", "Jeffrey","Jeffrey"]

val test4 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
      [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
       {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color((Clubs, Num 2)) = Black
val test6 = card_value((Clubs, Num 2)) = 2
val test7 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true
val test81 = all_same_color([(Hearts, Ace), (Spades, Ace)]) = false
val test82 = all_same_color([(Hearts, Ace), (Diamonds, Ace)]) = true

val test9 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4

val test10 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test101 = score([(Hearts, Num 2),(Hearts, Num 4)],10) = 2
val test102 = score([(Hearts, Num 10),(Hearts, Num 10)], 20) = 0

val test1112 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test121 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
             = 3

val test111 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                        [Draw,Discard(Hearts,Jack)],
                        42);
              false)
             handle IllegalMove => true)
