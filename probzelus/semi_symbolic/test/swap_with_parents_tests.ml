open Semi_symbolic.Semi_symbolic_impl

let rv1 = sample "parent1" (gaussian (const 1.) (const 1.))
let rv2 = sample "parent2" (gaussian (const 1.) (const 1.))
let rv3 = sample "parent3" (gaussian (add rv2 rv1) (const 1.))
let rv4 = sample "child" (gaussian (add rv1 rv3) (const 1.))

let rv1_inner =
  match rv1 with
  | ExRand rv -> rv
  | _ -> assert false
let rv2_inner =
  match rv2 with
  | ExRand rv -> rv
  | _ -> assert false
let rv3_inner =
  match rv3 with
  | ExRand rv -> rv
  | _ -> assert false

let rv4_inner =
  match rv4 with
  | ExRand rv -> rv
  | _ -> assert false

let parents = topo_sort (get_parents rv3_inner);;

(* List.iter (fun (RandomVar rv) -> Format.printf "%s " (rv.name)) parents;; *)

let true_parents = [ RandomVar rv1_inner; RandomVar rv2_inner ];;

assert(List.for_all2 (fun x y -> x = y) parents true_parents);;

hoist_and_eval rv4_inner;;

assert (not (depends_on rv4 rv1_inner true));;
assert (not (depends_on rv4 rv2_inner true));;
assert (not (depends_on rv4 rv3_inner true));;

match rv2_inner.distr with
| Normal(ExConst 1., ExConst 1.) ->
  assert false
| Normal(mu, _) -> 
  assert (depends_on mu rv3_inner false) 
| _ -> assert false


