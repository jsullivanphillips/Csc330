use "csc330.sml";
open Csc330;
use "set.sml";
open Set;

infix 1 IDENTICAL
infix 3 EXCEPT
infix 4 IS_SUBSET_OF
infix 5 INTERSECT
infix 6 UNION
infix 7 IN
infix 8 CONTAINS        
infix 9 ++
infix 9 --






(* should return set containing [1,2]*)
val temp = insert_into_set(insert_into_set(EmptySet Int.compare, 1), 2);
val a = temp ++ 3
val b = temp ++ 4;



val a = EmptySet Int.compare;
val b = a ++ 1 ++ 2;
size_set(b);
size_set(a);
val p = b INTERSECT a IDENTICAL b;

val se = EmptySet Int.compare;
val sa = se ++ 1 ++ 2 ++ 3 ++ 5 ++ 3 ++ 2;
val sb = se ++ 9 ++ 3 ++ 2 --2;
val empty_set = sb INTERSECT se;
val result = sb INTERSECT se IDENTICAL se;

val a = EmptySet Int.compare;
val b = a ++ 1 ++ 2 --1;
size_set(b);
val c = b ++ 3 -- 2;

val se = EmptySet Int.compare;
val sb = se ++ 9 ++ 3 ++ 2 --2;
val result = sb EXCEPT sb;




val se = EmptySet Int.compare;
val a = se ++ 1 ++ 2 ++ 3;
val b = se ++ 2 ++ 3;

val union_a = se ++ 1 ++ 2;
val union_b = se ++ 3 ++ 4;
val tester = union_a UNION union_b;

val union_c = se ++ 1 ++ 2;
val union_d = se ++ 1 ++ 2;
val tester = se UNION union_d;









