(*
(* currying *)
fun f a b = a + b
val p = f 2 3;

fun p st = print(st);

p "abc";

(*c is of type string -> (string -> string)*)
(* It takes in a string and
 returns a function that takes a string and
 returns a string*)
fun c x y = x ^ " " ^ y
fun c y z x = x * y + z
*)