open Semi_symbolic.Semi_symbolic_impl
open Test_lib

let rv : float expr = sample "var" (gaussian (const 0.) (const 1.))
let rv_inner =
  match rv with
  | ExRand r -> r
  | _ -> assert false

let expr1 = (add (mul (const 2.) rv) (const 3.));;

match is_affine expr1 rv_inner with
| Some (a, b) ->
  assert (eq_expr (eval a) (ExConst 2.));
  assert (eq_expr (eval b) (ExConst 3.));
| None -> assert false;;

let expr2 = add rv rv;;

match is_affine expr2 rv_inner with
| Some (a, b) ->
  assert (eq_expr (eval a) (ExConst 2.));
  assert (eq_expr (eval b) (ExConst 0.));
| None -> assert false;;

let expr3 = (add (mul rv (const 2.)) (add (mul rv (const 4.)) (const 5.)));;

match is_affine expr3 rv_inner with
| Some (a, b) ->
  assert (eq_expr (eval a) (ExConst 6.0));
  assert (eq_expr (eval b) (ExConst 5.));
| None -> assert false;;

let rv1 = sample "gaussian_rv1" (gaussian (const 1.) (const 100.))
let rv1_inner =
  match rv1 with
  | ExRand rv -> rv
  | _ -> assert false

let rv2 : float expr = sample "gaussian_rv2" (gaussian (mul (const 3.) rv1) (const 1.))
let rv2_inner =
  match rv2 with
  | ExRand rv -> rv
  | _ -> assert false;;

match gaussian_marginal rv1_inner rv2_inner with
| Some (Normal (mu, sigma)) ->
  assert (eq_expr (eval mu) (ExConst 3.));
  assert (eq_expr (eval sigma) (ExConst 901.))
| _ -> assert false

let mu', var' = 1. *. 3., 100. *. (3. ** 2.)
let var'' = 1. /. ((1. /. var') +. (1. /. 1.))
let a'', b'' = var'' *. (1. /. 1.), var'' *. (mu' /. var')
let a''', b''', var''' = a'' /. 3., b'' /. 3., var'' /. (3. ** 2.);;

match gaussian_posterior rv1_inner rv2_inner with
| Some (Normal (mu, sigma)) ->
  assert (eq_expr (eval sigma) (ExConst var'''));
  begin match is_affine mu rv2_inner with
  | Some (a, b) ->
    assert (eq_expr (eval a) (ExConst a'''));
    assert (eq_expr (eval b) (ExConst b'''))
  | None -> assert false
  end
| _ -> assert false;;

Printf.printf "Gaussian posterior -- variance = %f, a = %f, b = %f\n" var''' a''' b''';;
flush stdout;;

