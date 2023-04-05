open Semi_symbolic.Semi_symbolic_impl

let rv1 = sample "beta_prior" (beta (const 1.) (const 1.))
let rv1_inner =
  match rv1 with
  | ExRand rv -> rv
  | _ -> assert false

let _ = observe 0. (binomial (ExConst 5) rv1) 3;;
let _ = observe 0. (binomial (ExConst 7) rv1) 2;;

begin match rv1_inner.distr with
  | Beta(a, b) ->
    begin match (eval a, eval b) with
      | (ExConst a_v, ExConst b_v) ->
        Printf.printf "Beta-Binomial posterior: (%f, %f)\n" a_v b_v;
        assert (a_v = 6.);
        assert (b_v = 8.)
      | _ -> assert false
    end
  | _ -> assert false
end
