fun max(acc, x)=
      if (x > acc) 
      then
        SOME x
      else
        SOME acc

val two = max(2,1)

fun andalsocheck(x)=
    if x = 1 andalso false
    then 2
    else if x = 3
    then 3
    else if x = 9
    then 2
    else 4
    

fun f x =
    x + 1

fun g x =
    x div 2

val p = f o g
val three = p 5

val one = 1
val somone = SOME(1)

val inner = valOf(somone)
val equal = (one = inner)

val rnd1 = []
val rd2 = 2@rnd1
(*
val test_lst = SOME([])
val test_rslt = SOME(valOf(test_lst)@valOf(SOME(3)))
*)