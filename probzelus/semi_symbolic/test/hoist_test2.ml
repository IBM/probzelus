open Semi_symbolic.Semi_symbolic_impl

let rv1 : float expr = sample "rv1" (gaussian (const 0.) (const 1.))
let rv1_inner =
  match rv1 with
  | ExRand r -> r
  | _ -> assert false

let rv2 : float expr = sample "rv2" (gaussian (const 0.) (const 1.))
let rv2_inner =
  match rv2 with
  | ExRand r -> r
  | _ -> assert false

let rv3 : float expr = sample "rv3" (gaussian (add rv1 rv2) (const 1.))
 let rv3_inner =
  match rv3 with
  | ExRand r -> r
  | _ -> assert false


let rv4 : float expr = sample "rv4" (gaussian (add rv1 rv2) (const 1.))
let rv4_inner =
  match rv4 with
  | ExRand r -> r
  | _ -> assert false;;


hoist_and_eval rv3_inner;;
(*intervene rv3_inner 0.;;*)
hoist_and_eval rv4_inner
