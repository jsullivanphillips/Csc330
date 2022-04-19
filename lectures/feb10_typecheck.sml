(*
    f : T1 -> T2 [must be a function, all functions take 1 arg]
    x : T1

    y : T3
    z : T4

    T1 = T3 * T4 [else pattern match doesnt type check]
    T3 = int [abs has type int -> int]
    T4 = int [because we added z to an int]
    so T1 = int * int
    (abs y) + z : int, so let-expression : int, 
    so body : int, therefore T2 = int
    f : int * int -> int
*)
fun f x = 
    let val (y,z) = x in
        (abs y) + z
    end


(*
--------------------------
    sum : T1 -> T2
    xs : T1

    [pattern match T1]
    x : T3
    xs' : T3 list

    T1 = T3 list
    T2 = int [because 0 might be return]
    T3 = int [because x:T3 and we add x to something]

    so sum : int list -> int
*)
fun sum xs =
    case xs of
        [] => 0
      | x::xs' => x + (sum xs')
(*
-------------------------
    length : T1 -> T2
    xs : T1

    [patern matching]
    x : T3
    xs' : T3 list

    T1 = T3 list
    T2 = int

    T3 list -> int
    'a list -> int
*)
fun length xs =
    case xs of
        [] => 0
      | x::xs' => 1 + (length xs')

(*
------------------------------
    f : T1 * T2 * T3 -> T4
    x : T1
    y : T2
    z : T3

    T4 = T1 * T2 * T3
    T4 = T2 * T1 * T3
    only way those can both be true is if T1 = T2
    put it all together:
    f : T1 * T1 * T3 -> T1 * T1 * T3
    f : 'a * 'a * 'b -> 'a * 'a * 'b
*)
fun f (x,y,z)=
    if true
    then (x,y,z)
    else (y,x,z)

(*
----------------------
    compose : (f: T1) * (g: T2) -> (body return: T3)
    x : T4
    from body being function that takes x, (return:T3) = (x:T4)->(body return: T5), for some T5
    from g being passed x, (g:T2) = (x:T4) -> (g return: T6),  for some T6 
    from f being returned result of g. (f:T1) = T6 -> (f return: T7) for some T7
    fomr f being the call of anon func, (f return: T7) = (body return: T5)
    therefore
    compose : (T6 -> T5) * (T4 -> T6) -> (T4 -> T5)
    compose : ('a -> 'b) * ('c -> 'a) -> ('c -> 'b)
*)
(*fun compose(f,g) fn x => f(g x)*)

(*
    f : T1 * T2 -> T3
    a : T1
    b : T2
    T3 = (string * T1 * T2)

    f : T1 * T2 -> string * T1 * T2
    f : ('a * 'b) -> (string * 'a * 'b)
*)

fun f (a, b) =
    ("this is", a, b)

(*
    f : T1 * T2 -> T3
    a : T1 used in + expression so int
    b : T2 used in + expression so int
    T3 : int

    f : int * int -> int
*)
fun f (a,b) = 
    a + b


(*
f : T1 -> T2 -> T3
a : T1
b : T2
a : list T4
T3 = b T2
T3 = a T1
T2 = T3 = T1 = List T4

f : 'a list -> 'a list -> a' list
*)
fun f a b =
    case a of 
        [] => b
       | _ => a


(*
f : T1 -> T2 -> T3
a : T1
c : T2
T3 : int
b : int
f : int -> int -> int
*)
fun f a c =
    let
        val b = a -1
        val a = b -1
        val b = a -1
    in
        c - b
    end

(*x : int*)
val x = 10
(*y : int list*)
val y = [x, x]
(* z : int * int list*)
val z = (x, y)

fun f () =
    (z, y)

(*
f: t1 -> t2 -> (t1 * t1)
f: 'a -> 'b -> ('a * 'a)
*)

fun f a b = (a, a)

val x = [1,2,3]
(*
h : t1 = t2 * t3 -> t4
h : (int list * int list)
*)
val h = f x (hd x)
