open Semi_symbolic.Semi_symbolic_impl
open Test_lib

let test1 = ExAdd(ExConst 0., ExConst 1.)
let test1_ops : float expr = add (const 0.) (const 1.);;

assert (eq_expr test1 test1_ops);;

let test_rv1 : float expr = sample "rv1" (gaussian (const 0.) (const 1.))
let test_rv1_inner = 
  match test_rv1 with
  | ExRand rv -> rv
  | _ -> assert false

let test_rv2 : float expr = sample "rv2" (gaussian (const 0.) (const 1.))
let test_rv2_inner = 
  match test_rv2 with
  | ExRand rv -> rv
  | _ -> assert false

let test2 = add (mul test_rv1 (const 3.)) (const 2.);;

assert (depends_on test2 test_rv1_inner false);;
assert (not (depends_on test2 test_rv2_inner false));;

let test3 = ite (eq test_rv2 test_rv2) (const 0.) (const 1.);;

assert (depends_on test3 test_rv2_inner false);;
assert (not (depends_on test3 test_rv1_inner false))

