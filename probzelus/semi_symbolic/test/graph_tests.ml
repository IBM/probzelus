open Semi_symbolic.Semi_symbolic_impl;;

let rv1 = sample "graph_rv1" (gaussian (const 1.) (const 100.))
let rv1_inner =
  match rv1 with
  | ExRand rv -> rv
  | _ -> assert false

let rv2 : float expr = sample "graph_rv2" (gaussian (mul (const 3.) rv1) (const 1.))
let rv2_inner =
  match rv2 with
  | ExRand rv -> rv
  | _ -> assert false;;

assert (depends_on rv2 rv1_inner true);;
assert (not (depends_on rv1 rv2_inner true));;

assert (swap rv1_inner rv2_inner);;

assert (depends_on rv1 rv2_inner true);;
assert (not (depends_on rv2 rv1_inner true));;

let rv3 : float expr = sample "graph_rv3" (gaussian (add rv1 rv2) (const 1.))
let rv3_inner =
  match rv3 with
  | ExRand rv -> rv
  | _ -> assert false;;

assert (not (has_parents_except_rv rv1_inner rv3_inner));;

assert (can_swap rv1_inner rv3_inner);;
assert (not (can_swap rv2_inner rv3_inner));;

assert (has_parents_rv rv1_inner);;
assert (not (has_parents_rv rv2_inner));;
assert (has_parents_rv rv3_inner);;

hoist rv3_inner;;

assert (not (has_parents_rv rv3_inner));;
