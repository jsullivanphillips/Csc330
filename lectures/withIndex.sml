(* input list returns a list of tuples with first element is index and second
* element is item *)
fun withIndex(lst)=
  case lst of
        [] => (0, [])
      | x::x's => (0, x)

val p = [1,2,3]
val r = withIndex(p);

