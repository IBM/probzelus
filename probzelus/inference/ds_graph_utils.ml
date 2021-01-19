(*
 * Copyright 2018-2020 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

open Owl
open Types

(** {2 Distribution manipulations} *)

let cdistr_to_mdistr : type a b.
  (a, b) cdistr -> a -> b mdistr =
  fun cdistr obs ->
  begin match cdistr with
  | AffineMeanGaussian (m, b, obsvar) ->
      Dist_gaussian (m *. obs +. b, obsvar)
  | CBernoulli ->
      Dist_bernoulli obs
  | AffineMeanGaussianMV (m, b, sigma) ->
      let aux = Mat.dot m obs in
      Mat.add_ ~out:aux aux b;
      Dist_mv_gaussian (aux, sigma, None)
  | CBernBern (bfn) ->
      Dist_bernoulli (bfn obs)
  end

let make_marginal : type a b.
  a mdistr -> (a, b) cdistr -> b mdistr =
  fun mdistr cdistr ->
  begin match mdistr, cdistr with
  | Dist_gaussian (mu, var), AffineMeanGaussian(m, b, obsvar) ->
      Dist_gaussian (m *. mu +. b,
                     m ** 2. *. var +. obsvar)
  | Dist_mv_gaussian (mu0, sigma0, _),
    AffineMeanGaussianMV(m, b, sigma) ->
      let n, k = Mat.shape sigma in
      let mu' = Mat.dot m mu0 in
      Mat.add_ ~out:mu' mu' b;
      let sigma' = Mat.empty n k in
      Mat.dot_ ~c:sigma' (Mat.dot m sigma0) ~transb: true m;
      Mat.add_ ~out:sigma' sigma' sigma;
      Dist_mv_gaussian (mu', sigma', None)
  | Dist_beta (a, b),  CBernoulli ->
      Dist_bernoulli (a /. (a +. b))
  | Dist_bernoulli (p_prior), CBernBern bfn ->
      let p_marg = (p_prior *. (bfn true)) +.
                   ((1. -. p_prior) *. (bfn false)) in
      Dist_bernoulli p_marg
  | _ -> assert false
  end

let make_conditional : type a b.
  a mdistr -> (a, b) cdistr -> b -> a mdistr =
  let gaussian_conditioning mu var obs obsvar =
    let ivar = 1. /. var in
    let iobsvar = 1. /. obsvar in
    let inf = ivar +. iobsvar in
    let var' = 1. /. inf in
    let mu' = (ivar *. mu +. iobsvar *. obs) /. inf in
    (mu', var')
  in
  fun mdistr cdistr obs ->
    begin match mdistr, cdistr with
    | Dist_gaussian(mu, var), AffineMeanGaussian(m, b, obsvar) ->
        let (mu', var') =
          gaussian_conditioning mu var
            ((obs -. b) /. m) (obsvar /. m ** 2.)
        in
        Dist_gaussian (mu', var')
    | Dist_mv_gaussian(mu0, sigma0, _),
      AffineMeanGaussianMV(m, b, sigma) ->
        let innov = Mat.sub obs b in
        Mat.sub_ ~out:innov innov (Mat.dot m mu0);
        let mt = Mat.transpose m in
        let innov_sigma =
          let aux = Mat.dot m (Mat.dot sigma0 mt) in
          Mat.add_ ~out:aux aux sigma;
          aux
        in
        let gain =
          Mat.dot sigma0 (Mat.dot mt (Linalg.D.inv innov_sigma))
        in
        let mu' =
          let aux = Mat.dot gain innov in
          Mat.add_ ~out:aux mu0 aux;
          aux
        in
        let sigma' =
          let kh = Mat.dot gain m in
          let aux = Mat.eye (Mat.row_num kh) in
          Mat.sub_ ~out:aux aux kh;
          Mat.dot aux sigma0
        in
        Dist_mv_gaussian (mu', sigma', None)

    | Dist_beta (a, b),  CBernoulli ->
        if obs then Dist_beta (a +. 1., b)
        else Dist_beta (a, b +. 1.)
    | Dist_bernoulli (p_prior), CBernBern bfn ->
        let p_true, p_false =
          if obs then
            (p_prior *. (bfn true), (1. -. p_prior) *. (bfn false))
          else
            (p_prior *. (1. -. (bfn true)), (1. -. p_prior) *. (1. -. (bfn false)))
        in
        Dist_bernoulli (p_true /. (p_true +. p_false))
    | _, _ -> assert false
    end
