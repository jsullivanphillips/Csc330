use "patterns.sml";
open Patterns;

infix 9 ++
infix 9 --
infix 7 == 
fun add(x,y)=
    x + y

fun equals_two(x)=
    x = 2

fun check_ans(t1, t2, test_name)=
    if t1 = t2
    then test_name ^ ": test passed"
    else test_name ^ ": test failed"

fun some_answer(x) =
    if(x=3)
    then SOME x
    else NONE

fun less_than_four(x)=
    if(x<4)
    then SOME [x]
    else NONE

val t01 = emptyTree ++ 5 ++ 8 ++ 2 ++ 1 ++ 3
val t01_res = tree_delete(t01, 8)
val t01_ans = emptyTree  ++ 5 ++ 2 ++ 1 ++ 3


val t02 = emptyTree ++ 5 ++ 8 ++ 2 ++ 1 ++ 3
val t02_res = tree_delete(t01, 3)
val t02_ans = emptyTree  ++ 5 ++ 8 ++ 2 ++ 1


val t03 = emptyTree ++ 5 ++ 8 ++ 2 ++ 1 ++ 3
val t03_res = tree_to_list(t03)
val t03_ans = [5,2,1,3,8]

val t04 = emptyTree ++ 5 ++ 8 ++ 2 ++ 1 ++ 3
val t04_res = tree_filter equals_two t04
val t04_ans = emptyTree ++ 2

val t05 = emptyTree ++ 5 ++ 8 ++ 2 ++ 1 ++ 3
val t05_res = tree_sum_even t05
val t05_ans = 10

val t06 = emptyTree ++ 5 ++ 10 ++ 2 ++ 1 ++ 3
val t06_res = valOf(tree_max t06)
val t06_ans = 10

val t07 = [1,2,3,4,5]
val t07_res = first_answer some_answer t07
val t07_ans = 3

val t08 = [1,2,3,4,5]
val t08_res = all_answer less_than_four t08
val t08_ans = NONE


val t09 = [1,2,3]
val t09_res = all_answer less_than_four t09
val t09_ans = SOME([1,2,3])

val t10 = emptyTree ++ 4 ++ 2 ++ 1 ++ 5 ++ 1 ++ 10
val t10_res = tree_to_list(t10)
val t10_ans = [4,2,1,1,5,10]



val t01_passed = check_ans(t01_res, t01_ans, "delete larger")
val t02_passed = check_ans(t02_res, t02_ans, "delete smaller then larger")
val t03_passed = check_ans(t03_res, t03_ans, "tree to list")
val t04_passed = check_ans(t04_res, t04_ans, "tree filter simple")
val t05_passed = check_ans(t05_res, t05_ans, "tree sum even")
val t06_passed = check_ans(t06_res, t06_ans, "tree max")
val t07_passed = check_ans(t07_res, t07_ans, "first answer")
val t08_passed = check_ans(t08_res, t08_ans, "all answer")
val t09_passed = check_ans(t09_res, t09_ans, "all answer2")
val t10_passed = check_ans(t10_res, t10_ans, "tree to list 2")