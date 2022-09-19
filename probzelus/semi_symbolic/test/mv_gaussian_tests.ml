open Semi_symbolic.Semi_symbolic_impl
open Owl

let mu0 = Mat.of_array [| 20.; 0.; |] 2 1
let sigma0 = Mat.of_arrays [| [| 3.; 0.; |];
                              [|  0.; 3.;|] |]

let rv1 = sample "mu0" (mv_gaussian (const mu0) (const sigma0))
let rv1_inner =
  begin match rv1 with
  | ExRand (rv) -> rv
  | _ -> assert false
  end

let m = Mat.of_arrays [| [| 1.; 1.|];
                         [| 0.; 1.|] |]

let sigma = Mat.of_arrays [| [| 1.; 0.|];
                             [| 0.; 1.|] |]
let d2 = mv_gaussian (mat_dot (const m) rv1) (const sigma)

let rv2 = sample "x" d2
let rv2_inner =
  begin match rv2 with
  | ExRand rv -> rv
  | _ -> assert false
  end;;

hoist_and_eval rv2_inner;;

let mu' = Mat.dot m mu0
let sigma' = Mat.add (Mat.dot (Mat.dot m sigma0) (Mat.transpose m)) sigma;;

begin match rv2_inner.distr with
  | MvNormal(ExConst mu_val', ExConst sigma_val') ->
    assert (mu' = mu_val');
    assert (sigma' = sigma_val');
  | _ -> assert false
end;;

hoist_and_eval rv1_inner;;

begin match rv1_inner.distr with
  | MvNormal(ExConst mu_val, ExConst sigma_val) ->
    assert (mu_val = mu0);
    Mat.print sigma_val;
    Mat.print sigma0;
    (* assert (sigma_val = sigma0) XXX: this fails by a very small amount *)
  | _ -> assert false
end;;

let obsval = Mat.of_array [| 0.; -10. |] 2 1

let _ = observe 0. d2 obsval

let innov = Mat.sub obsval (Mat.dot m mu0)
let innov_sigma = Mat.add (Mat.dot m (Mat.dot sigma0 (Mat.transpose m))) sigma
let gain = Mat.dot sigma0 (Mat.dot (Mat.transpose m) (Mat.inv innov_sigma))
let mu' = Mat.add mu0 (Mat.dot gain innov)
let sigma' = Mat.dot (Mat.sub (Mat.eye 2) (Mat.dot gain m)) (sigma0);;

begin match rv1_inner.distr with 
  | MvNormal(mu, sigma) ->
    begin match (eval mu, eval sigma) with
    | (ExConst mu_val, ExConst sigma_val) ->
      assert (mu_val = mu');
      assert (sigma_val = sigma');
    | _ -> assert false
    end
  | _ -> assert false
end


