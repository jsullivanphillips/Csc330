(*
Your name: Jamie Sullivan-Phillips
Your student id: V00865650
*)

structure Patterns =

struct

exception NoAnswer
exception NotFound

datatype tree = emptyTree |
                nodeTree of int * tree * tree


datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype value = Const of int
	       | Unit
	       | Tuple of value list
	       | Constructor of string * value


(* write your tree functions here *)
(* helpers *)
fun max(a, b)=
  if a > b
  then a
  else b


fun add_to_list(x, acc)=
  acc@[x]

fun is_even(x) =
  (x mod 2) = 0

fun sum(acc, x) =
  acc + x

fun is_some x =
  if x <> 0
  then SOME(x)
  else NONE
(* tree * int -> tree
   Insert in order, if the node is duplicated, insert to the left.
   This should be a recursive function.  *)
fun tree_insert_in_order(t, v)=
  case t of
      emptyTree => nodeTree(v, emptyTree, emptyTree)
    | nodeTree(curr, l, r) => 
        if v <= curr
        then nodeTree(curr, tree_insert_in_order(l, v), r)
        else nodeTree(curr, l, tree_insert_in_order(r, v)) 
    
(* First, find the node to delete (if v appears more than once, find the first instance). 
  If v does not exist, raise NotFound. Second, if the node to delete has one child only, then
  simply delete the node and reconnect the child to the parent; otherwise, find the maximum node in the
  left child, remove it from this subtree, and create a note to replace the one you are deleting (with the
  new subtree as its left child). Use tree_max and a recursive call to tree_delete *)
fun tree_delete(t, v)=
  t

fun tree_height t =
  let
    fun aux(t, acc)=
      case t of
        emptyTree => acc
      | nodeTree(x,l,r) => max(1 + tree_height(l), 1 + tree_height(r))
  in
    aux(t, 0)
  end

(* Write a “fold” function that traverses the tree in preorder
   (node, left children, right children) "folding" the tree using the function f.
   Use acc as the starting value.

   fun fold (f,acc,xs) =
    case xs of
      [] => acc
    | x::xs => fold(f, f(acc,x), xs)
    ('a * int -> 'a) -> 'a -> Patterns.tree -> 'a
*)
fun tree_fold_pre_order f acc t =
  case t of
  (*
    nodeTree(x, left, right) => tree_fold_pre_order f (f(x, tree_fold_pre_order f acc left)) right
  *)
  nodeTree(x, left, right) => tree_fold_pre_order f (tree_fold_pre_order f (f(x, acc)) left) right
  | emptyTree => acc


(*tree_max t. Find the maximum value in the tree (returns an option). Use a val expression and
tree_fold_pre_order to write this function. I know, I know, this is the most inefficient way to
find the maximum, and I agree; we are learning functional programming, not algorithms. Restriction:
define it using val and use tree_fold_pre_order*)
val tree_max = is_some o tree_fold_pre_order max 0 


(* First, find the node to delete (if v appears more than once, find the first instance). 
If v does not exist, raise NotFound. Second, if the node to delete has one child only, then
simply delete the node and reconnect the child to the parent; otherwise, find the maximum node in the
left child, remove it from this subtree, and create a note to replace the one you are deleting (with the
new subtree as its left child). Use tree_max and a recursive call to tree_delete. *)
fun tree_delete(t, v)= 
  case t of
    emptyTree => raise NotFound
  | nodeTree(x, left, right)  => 
      if (v < x) andalso (left <> emptyTree)
        then nodeTree(x, tree_delete(left, v), right)
      else if v > x andalso right <> emptyTree
        then nodeTree(x, left, tree_delete(right, v))
      else if v = x
      (* found our node, time to check subtrees*)
        then 
        if left = emptyTree andalso right = emptyTree
          then emptyTree
        else if left <> emptyTree andalso right = emptyTree
          then left
        else if left = emptyTree andalso right <> emptyTree
          then right
        else nodeTree(valOf(tree_max left), tree_delete(left, valOf(tree_max left)), right)
      else raise NotFound

   


(* Convert the tree to a list, in pre-order. Restriction: define it using val and use
tree_fold_pre_order*)
val tree_to_list = tree_fold_pre_order add_to_list [] 

(*Write a function to “filter” the tree. The function should return a tree with only
nodes for which f returns true. Use tree_delete. It will result in a very simple implementation
(though inefficient).*)
fun tree_filter f t =
  case t of
    emptyTree => emptyTree
  | nodeTree(x, left, right) =>
      if f(x)
      then nodeTree(x, tree_filter f left, tree_filter f right)
      else tree_filter f (tree_delete(t, x))

(* Using a val expression, tree_filter and tree_fold_pre_order,
and function composition to write a function that sums the nodes that are are even. 
Use the mod operator. *)
val tree_sum_even = tree_fold_pre_order sum 0 o tree_filter is_even

fun first_answer f lst =
  case lst of
    [] => raise NoAnswer
  | v::tail => 
    if isSome(f(v))
    then valOf(f(v))
    else first_answer f tail

fun all_answers f lst =
  let
    fun aux(acc, lst)=
      case lst of
        [] => acc
      | v::tail =>
          if (isSome(f(v)))
          then aux(SOME(valOf(acc)@valOf(f(v))), tail)
          else aux(NONE, [])
  in
    aux(SOME [], lst)
  end

fun check_pattern(temp)=
  false

fun match(a, b)=
  NONE

fun first_match a b=
  NONE

(* leave the following functions untouched *)

fun tree_root t =
    case t of
        emptyTree => NONE
      | nodeTree (n, _, _) => SOME n

fun tree_equal t1 t2  =
    case (t1, t2) of
        (emptyTree, emptyTree) => true
      | (emptyTree, _) =>  false
      | (_, emptyTree) => false
      | (nodeTree(n1, l1, r1),nodeTree(n2, l2, r2)) =>
        n1 = n2 andalso (tree_equal l1 l2) andalso (tree_equal r1 r2)

infix 9 ++
infix 9 --
infix 7 == 

fun t ++ v = tree_insert_in_order(t, v)
fun t -- v = tree_delete(t, v)
fun t1 == t2 = tree_equal t1 t2

end

