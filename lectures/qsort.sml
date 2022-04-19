(* this wont work *)
fun qsort(lst) =
  case lst of
       [] => []
     | [_] => lst
     | x::t =>
         let
           val p1 = (lst, x)
           val p2 = (lst, x)
         in
           p1 @[x]@p2
         end
