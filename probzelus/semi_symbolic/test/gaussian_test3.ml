open Semi_symbolic.Semi_symbolic_impl
open Owl

let mu0 = Mat.of_array [| 0.; 0. |] 2 1
let sigma0 = Mat.of_arrays [| [| 2500.; 0. |];
                              [| 0.; 2500. |] |]

let rv1 = sample "mu0" (mv_gaussian (const mu0) (const sigma0))
let rv1_inner =
  begin match rv1 with
  | ExRand (rv) -> rv
  | _ -> assert false
  end

let ml = Mat.of_arrays [| [| 1.; (2. *. -1.) |] |]
let mr = Mat.of_arrays [| [| 1.; (2.) |] |]
let obs_sigmal = Mat.of_arrays [| [| 1. |] |];;
let obs_sigmar = Mat.of_arrays [| [| 0.95 |] |];;

observe 0. (mv_gaussian (mat_dot (const ml) rv1) (const obs_sigmal)) (Mat.of_arrays [| [| -50. |] |]);;
observe 0. (mv_gaussian (mat_dot (const mr) rv1) (const obs_sigmar)) (Mat.of_arrays [| [| 200. |] |]);;

begin match (rv1_inner.distr) with
| MvNormal (mu, sigma) ->
  begin match (eval mu, eval sigma) with
  | ExConst (mu_val), ExConst (sigma_val) -> 
    Mat.print mu_val;
    Mat.print sigma_val;
  | _ -> assert false
  end
| _ -> assert false
end



