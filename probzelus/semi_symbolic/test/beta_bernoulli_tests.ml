open Semi_symbolic.Semi_symbolic_impl

let rv1 = sample "beta_prior" (beta (const 1.) (const 1.))
let rv1_inner =
  match rv1 with
  | ExRand rv -> rv
  | _ -> assert false

let _ = observe 0. (bernoulli rv1) true;;
let _ = observe 0. (bernoulli rv1) true;;

begin match rv1_inner.distr with
  | Beta(a, b) ->
    begin match (eval a, eval b) with
      | (ExConst a_v, ExConst b_v) ->
        Printf.printf "Beta-Bernoulli posterior: (%f, %f)\n" a_v b_v;
        assert (a_v = 3.);
        assert (b_v = 1.)
      | _ -> assert false
    end
  | _ -> assert false
end
