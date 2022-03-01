(*
Your name: Jamie Sullivan-Phillips
Your student id: V00865650
*)

structure Set =
struct
local
  open Csc330
in

datatype 'a set = EmptySet of ('a * 'a -> order) | Set of 'a list * ('a * 'a -> order)

exception SetIsEmpty

infix 1 IDENTICAL
infix 3 EXCEPT
infix 4 IS_SUBSET_OF
infix 5 INTERSECT
infix 6 UNION
infix 7 IN
infix 8 CONTAINS        
infix 9 ++
infix 9 --
(* Returns true if set s is empty*)
(* fn: 'a set -> `a *)
fun is_empty_set s =
    case s of
        EmptySet(_) => true
      | Set(_,_) => false

(* Returns the minimum value in set s. Raises an exception `SetIsEmpty` if s is empty*)
(* fn : ’a set -> ’a *)
fun min_in_set s =
    case s of
        EmptySet(_) => raise SetIsEmpty
      | Set([], _) => raise SetIsEmpty
      | Set(hd::_, _) => hd


(* Returns the maximum value in set s. Raises exception SetIsEmpty if s is empty.*)
(* fn : ’a set -> ’a *)
fun max_in_set s =
    case s of
        EmptySet(_) => raise SetIsEmpty
      | Set([], _) => raise SetIsEmpty
      | Set(hd::[], _) => hd
      | Set(hd::tl, order) => max_in_set(Set(tl, order))

(* Returns s ∪ v. where V is a single value *)
(* fn : ’a set * ’a -> ’a set *)
(* Do this 1st *)
fun insert_into_set(s,v) =
    let fun aux(s, t's, v, order)=
            case t's of
                [] => v::[]
              | hd::[] => 
                    if order(v, hd) = LESS
                    then s@(v::hd::[])
                    else if order(v,hd) = EQUAL
                    then s@(hd::[])
                    else s@(hd::v::[])
              | hd::tl => 
                    if order(v,hd) = LESS
                    then s@(v::hd::tl)
                    else if order(v,hd) = EQUAL
                    then s@(hd::tl)
                    else aux(s@[hd], tl, v, order)
    in
        case s of
            EmptySet(order) => Set(v::[], order)
          | Set([], order) => Set(v::[], order)
          | Set(hd::[], order) => 
                if order (v, hd) = LESS
                then Set(v::hd::[], order)
                else if order(v, hd) = EQUAL
                then Set(hd::[], order) 
                else Set(hd::v::[], order)
          | Set(hd::tail, order) => Set(aux([], hd::tail, v, order), order)
    end

(* Returns true if v ∈ s*)
(* fn: ’a set * ’a -> bool *)
(* Do this 2nd *)
fun in_set(s, v) =
    case s of
        EmptySet(_) => false
      | Set([], order) => false
      | Set(hd::tail, order) =>   
            if order(v, hd) = EQUAL
            then true
            else in_set(Set(tail, order), v)

(* Returns the s ∪ t. Must be tail recursive. Where S & T are both sets *)
(* fn: ’a set * ’a set -> ’a set *)
(* use insert_into on all s & t *)
(* 2 *)
fun union_set(s, t) =
    let
      fun aux(s, t, c)=
        case s of 
            EmptySet(order) => EmptySet(order)
          | Set([], order) => 
                if case t of
                    EmptySet(_) => true
                  | Set([], _) => true
                  | Set(_, _) => false
                then c
                else aux(t, s, c)
          | Set(hd::tl, order) => aux(Set(tl, order), t, insert_into_set(c, hd))
    in
        case (s,t) of
            (EmptySet(order), _) => t
          | (_, EmptySet(order)) => s
          | (Set(_, order), _) => aux(s,t, EmptySet(order))
    end

(* Returns s ∩ t. Must be tail recursive.*)
(* fn: ’a set * ’a set -> ’a set *)
(* has running time of O(2n) *)
fun intersect_set(s, t) =
    let
        fun help((a, b), c)=
            case (a,b) of
                (EmptySet(_), EmptySet(order)) =>  EmptySet(order)(* set A is empty *)
              | (EmptySet(order), _) => EmptySet(order) (* set A is empty *)
              | (_, EmptySet(order)) => EmptySet(order) (* set B is empty *)
              | (Set([], _), Set([], _)) => c
              | (Set(_, _), Set([], _)) => c
              | (Set([], _), Set(_, _)) => c
              | (Set(hdA::[], _), Set(hdB::tlB, order)) =>  (* Last value in set A *)
                    if order(hdA, hdB) = EQUAL
                    then insert_into_set(c, hdA)
                    else if order(hdA, hdB) = GREATER
                    then help((Set(hdA::[], order), Set(tlB, order)), c)
                    else c
              | (Set(hdA::tlA, _), Set(hdB::[], order)) =>  (* Last value in set B *)
                    if order(hdA, hdB) = EQUAL
                    then insert_into_set(c, hdA)
                    else if order(hdA, hdB) = LESS
                    then help((Set(tlA, order), Set(hdB::[], order)), c)
                    else c
              | (Set(hdA::tlA, order), Set(hdB::tlB, _)) => (*  *)
                    if order(hdA, hdB) = LESS
                    then help((Set(tlA, order), Set(hdB::tlB, order)), c)
                    else if order(hdA, hdB) = EQUAL
                    then help((Set(tlA, order), Set(tlB, order)), insert_into_set(c,hdA))
                    else help((Set(hdA::tlA, order), Set(tlB, order)), c)     
    in
        case (s, t) of
            (EmptySet(order), _) => EmptySet(order) (* set A is empty *)
          | (_, EmptySet(order)) => EmptySet(order) (* set B is empty *)
          | (Set(_, order), _) => help((s,t), EmptySet(order))
    end
    
(* Returns s − t . Must be tail recursive.*)
(* fn : ’a set * ’a set -> ’a set *)
(* loop through s and build auxillary set c adding every value
   that is in S and NOT in t *)
(* 1 *)
fun except_set(s, t) =
    let
        fun aux(s, t, c)=
            case s of
                EmptySet(order) => EmptySet(order)
              | Set([], order) => c
              | Set(hd::tl, order) => 
                    if in_set(t, hd)
                    then aux(Set(tl, order), t, c)
                    else aux(Set(tl, order), t, insert_into_set(c, hd))
    in
        case (s,t) of
            (EmptySet(order), _) => EmptySet(order)
          | (_, EmptySet(order)) => EmptySet(order)
          | (Set(_, order), _) => aux(s,t, EmptySet(order))
    end

(* Returns s − v where v is a value*)
(*  fn : ’a set * ’a -> ’a set *)
(* [front of list] * remove value * [tail end of list] *)
fun remove_from_set(s,v) =
    let
        fun aux(s, front, v)=
            case s of
                EmptySet(order) => EmptySet(order)
              | Set([], order) => front
              | Set(hd::tail, order) =>
                    if order(hd, v) = EQUAL
                    then union_set(front, Set(tl(tail), order) )
                    else aux(Set(tail, order) , insert_into_set(front, hd), v)
    in
        case s of
            EmptySet(order) => EmptySet(order)
          | Set([], order) => Set([], order)
          | Set(lst, order) =>
                if in_set(s, v)
                then aux(s, EmptySet(order), v)
                else s
    end
    
(* Returns the number of elements in the set.*)
(*  fn : ’a set -> int *)
fun size_set(s: 'a set) =
    let 
        fun aux(lst, acc)=
            case lst of 
                [] => acc
              | hd::[] => acc+1
              | hd::tl => aux(tl, acc+1)
    in
        case s of
            EmptySet(_) => 0
          | Set(hd::[], _) => 1
          | Set(lst, _) => aux(lst, 0)
    end

(* Returns true if s = t *)
(* fn : ’a set * ’a set -> bool *)
fun equal_set(a, b) =
    if size_set(a) = size_set(b)
    then 
        let
            val c = intersect_set(a,b)
        in
            size_set(c) = size_set(a)
        end
    else
        false

(* Returns true if s ⊆ t*)
(*   fn : ’a set * ’a set -> bool *)
fun is_subset_of(s, t) =
    size_set(intersect_set(s,t)) = size_set(s)
        
(* Returns the set created from the contents of the list, using the comparision function f.*)
(*  fn : ’a list * (’a * ’a -> order) -> ’a set *)
fun list_to_set(lst, f) =
    let
        fun aux(lst, s)=
            case lst of
                [] => s
              | hd::tl => aux(tl, insert_into_set(s, hd))
    in
        aux(lst, EmptySet(f))
    end

(* Returns a list with the contents of the set, in order.*)
(*   fn : ’a set -> ’a list *)
fun set_to_list s =
    case s of
        EmptySet(_) => []
      | Set([], _) => []
      | Set(lst, _) => lst

(* Returns a string with the contents of the set. 
It will convert each element of the set with the function fstr. 
The set contents are surrounded by braces, the elements
are separated by “:” and the elements are in order.
- str_set((EmptySet Int.compare) ++ 3 ++ 2 ++ 4, Int.toString);
val it = "{2:3:4}" : string *)
(*  fn : ’a set * (’a -> string) -> string *)
fun str_set (s, fstr) =
    let
        fun aux(s, fstr : ('a -> string), acc: string)=
            case s of
                EmptySet(order) => ""
              | Set([], _) => acc
              | Set(hd::[], order) => acc ^ fstr(hd)
              | Set(hd::tl, order) => aux(Set(tl, order), fstr, acc ^ fstr(hd) ^ ":")
    in
        case s of
            EmptySet(_) => "{}"
          | Set([], _) => "{}"
          | Set(_, _) => aux(s, fstr, "{") ^ "}"
    end

(*  Return a new set (that uses the fcomp comparision function). If
we call this set r, then:
r = `the Union of all values v that exist within s` [ f(v) ]
 (look at assignment pdf) *)  
 (*  fn : ’a set * (’b * ’b -> order) * (’a -> ’b) -> ’b set *)
fun map_set (s, fcomp, f) =
    EmptySet


fun s -- v = remove_from_set(s,v)
fun s ++ v = insert_into_set(s,v)
fun s IDENTICAL t = equal_set(s,t)
fun s UNION t = union_set(s, t)
fun s INTERSECT t = intersect_set(s, t)
fun s EXCEPT t = except_set(s, t)
fun v IN s = in_set(s, v)
fun s IS_SUBSET_OF t = is_subset_of(s, t)


fun comp_list_any (a: 'a list, b: 'a list, fcomp : ('a * 'a) -> order) =
    let
        fun aux(a, b, fcomp)=
            case (a,b) of
                ([], []) => EQUAL
              | ([], _) => LESS
              | (_, []) => GREATER
              | (hdA::tlA, hdB::tlB) =>
                    if fcomp(hdA, hdB) = EQUAL
                    then aux(tlA, tlB, fcomp)
                    else if fcomp(hdA, hdB) = GREATER
                    then GREATER
                    else LESS
    in
        aux(a,b,fcomp)
    end
                          
end
end    
