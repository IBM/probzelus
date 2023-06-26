open Semi_symbolic.Semi_symbolic_impl;;
(*open Test_lib;;*)

Random.self_init ();;

let rv1 = sample "operation_rv1" (gaussian (const 1.) (const 100.))
let rv1_inner =
  match rv1 with
  | ExRand rv -> rv
  | _ -> assert false

let rv2 : float expr = sample "operation_rv2" (gaussian (mul (const 3.) rv1) (const 1.))
let rv2_inner =
  match rv2 with
  | ExRand rv -> rv
  | _ -> assert false;;

let scorefn = score rv2_inner;;

let mu, var =
  match rv2_inner.distr with
  | Normal (ExConst mu', ExConst var') -> (mu', var')
  | _ -> assert false;;
Printf.printf "Marginal -- mu = %f, var = %f\n" mu var;;

Printf.printf "Score x = 20: %f\n" (scorefn 50.);;
Printf.printf "Score x = -20: %f\n" (scorefn (-. 50.))

let drawfn = draw rv2_inner ~record:true;;
for _ = 1 to 10 do
  Printf.printf "Sample from marginal: %f\n" (drawfn ())
done;;

Printf.printf "----------------\n";;
Printf.printf "Observing x = 45\n";;
Printf.printf "----------------\n";;

let score = observe_inner 0. rv2_inner 45.

let drawfn = draw rv1_inner ~record:true;;
for _ = 1 to 10 do
  Printf.printf "Sample from posterior: %f\n" (drawfn ())
done;;


let rv3 = sample "rv3" (gaussian (const 0.) (const 100.))
let rv3_inner =
  match rv3 with
  | ExRand rv -> rv
  | _ -> assert false

let rv4 = sample "rv4" (gaussian (const 0.) (const 100.))
let rv4_inner =
  match rv4 with
  | ExRand rv -> rv
  | _ -> assert false;;

observe 0. (gaussian (mul rv3 rv4) (const 1.)) 10.;;

match rv3_inner.distr, rv4_inner.distr with
| (Delta e, _) ->
  begin match eval e with
  | ExConst v -> Printf.printf "rv3 sampled to %f\n" v
  | _ -> assert false
  end
| (_ , Delta e) ->
  begin match eval e with
  | ExConst v -> Printf.printf "rv4 sampled to %f\n" v
  | _ -> assert false
  end
| _ -> assert false


