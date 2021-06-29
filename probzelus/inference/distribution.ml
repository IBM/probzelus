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

(** {2 Type definitions} *)

open Owl
open Zelus_owl
open Types

exception Draw_error

module rec Distribution_rec: DISTRIBUTION = struct

  module DS_graph = Ds_streaming_graph.Make(Distribution_rec)

  type 'a t = 'a distr

  (** {2 Utils}*)

  let support_remove_duplicate l =
    let tbl = Hashtbl.create 7 in
    List.iter
      (fun (x, p) ->
         let k = Marshal.to_bytes x [Marshal.Closures] in
         try
           let (_, p') = Hashtbl.find tbl k in
           Hashtbl.replace tbl k (x, p +. p')
         with Not_found -> Hashtbl.add tbl k (x, p))
      l;
    Hashtbl.fold (fun _ (x, n) acc -> (x, n)::acc) tbl []

  let pp_print_any : type a. Format.formatter -> a -> unit =
    fun ppf _ ->
    Format.fprintf ppf "_"

  let pp_print_array :
    type a. ?pp_sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> a -> unit) -> Format.formatter -> a array -> unit =
    fun ?pp_sep pp_v ppf a ->
    let l = Array.to_list a in
    begin match pp_sep with
    | None -> Format.pp_print_list pp_v ppf l
    | Some pp_s -> Format.pp_print_list ~pp_sep:pp_s pp_v ppf l
    end

  let rec pp_print :
    type a. (Format.formatter -> a -> unit) -> Format.formatter -> a t -> unit =
    fun pp_v ppf dist ->
    begin match dist with
    | Dist_sampler (_, _) ->
        Format.fprintf ppf "sample(_, _)"
    | Dist_sampler_float (_, _, _) ->
        Format.fprintf ppf "sample(_, _)"
    | Dist_support l ->
        Format.fprintf ppf "support [ @[%a@] ]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
             (fun ppf (v, p) -> Format.fprintf ppf "(%a, %f)" pp_v v p))
          (support_remove_duplicate l)
    | Dist_mixture l ->
        Format.fprintf ppf "mixture [ @[%a@] ]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
             (fun ppf (v, p) ->
                Format.fprintf ppf "(%a, %f)" (pp_print pp_v) v p))
          l
    | Dist_pair (d1, d2) ->
        Format.fprintf ppf "(%a, %a)"
          (pp_print pp_print_any) d1
          (pp_print pp_print_any) d2
    | Dist_list l ->
        Format.fprintf ppf "[ @[%a@] ]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
             (fun ppf d ->
                Format.fprintf ppf "%a" (pp_print pp_print_any) d))
          l
    | Dist_array a ->
        Format.fprintf ppf "[| @[%a@] |]"
          (pp_print_array
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
             (fun ppf d ->
                Format.fprintf ppf "%a" (pp_print pp_print_any) d))
          a
    | Dist_gaussian (mean, sigma2) ->
        Format.fprintf ppf "gaussian (%f, %f)" mean sigma2
    | Dist_lognormal (mean, sigma) ->
        Format.fprintf ppf "lognormal (%f, %f)" mean sigma
    | Dist_beta (a, b) ->
        Format.fprintf ppf "beta (%f, %f)" a b
    | Dist_bernoulli p ->
        Format.fprintf ppf "bernoulli %f" p
    | Dist_uniform_int (a, b) ->
        Format.fprintf ppf "uniform_int (%d, %d)" a b
    | Dist_uniform_float (a, b) ->
        Format.fprintf ppf "uniform_float (%f, %f)" a b
    | Dist_exponential lambda ->
        Format.fprintf ppf "exponential %f" lambda
    | Dist_poisson lambda ->
        Format.fprintf ppf "poisson %f" lambda
    | Dist_add (a, b) ->
        Format.fprintf ppf "(%a + %a)"
          (pp_print pp_v) a (pp_print pp_v) b
    | Dist_mult (a, b) ->
        Format.fprintf ppf "(%a * %a)"
          (pp_print pp_v) a (pp_print pp_v) b
    | Dist_app (f, x) ->
        Format.fprintf ppf "(%a) (%a)"
          (pp_print pp_print_any) f (pp_print pp_print_any) x
    | Dist_mv_gaussian (mu, sigma, _) ->
        (* XXX TODO XXX *)
        Format.fprintf ppf "mv_gaussian (%a, %a)"
          pp_print_any mu
          pp_print_any sigma
    | Dist_joint _ ->
        (* XXX TODO XXX *)
        Format.fprintf ppf "joint distribution"
    end

  let print_any_t : type a. a t -> unit =
    fun dist ->
    Format.printf "%a@?" (pp_print pp_print_any) dist

  let print_float_t : float t -> unit =
    fun dist ->
    Format.printf "%a@?"
      (pp_print (fun ppf v -> Format.fprintf ppf "%f" v))
      dist

  let print_int_t : int t -> unit =
    fun dist ->
    Format.printf "%a@?"
      (pp_print (fun ppf v -> Format.fprintf ppf "%d" v))
      dist

  let print_bool_t : bool t -> unit =
    fun dist ->
    Format.printf "%a@?"
      (pp_print (fun ppf v -> Format.fprintf ppf "%B" v))
      dist

  let print_t : type a. (a -> string) -> a t -> unit =
    fun to_string dist ->
    Format.printf "%a@?"
      (pp_print (fun ppf v -> Format.fprintf ppf "%s" (to_string v)))
      dist

  module Map_float = Map.Make(struct
      (* type key = float *)
      type t = float
      let compare x y = compare x y
    end)

  let pi = 4. *. atan 1.
  let two_pi = 2.0 *. pi
  let sqrt_two_pi = sqrt two_pi

  let flatten_suport op s1 s2 =
    let support =
      List.fold_left
        (fun acc (v1, p1) ->
           let m =
             List.fold_left
               (fun acc (v2, p2) -> Map_float.add (op v1 v2) (p1 +. p2) acc)
               Map_float.empty
               s2
           in
           Map_float.union (fun _ p1 p2 -> Some(p1 +. p2)) acc m)
        Map_float.empty
        s1
    in
    Map_float.fold (fun v p acc -> (v, p)::acc) support []


  let gamma =
    let g = 7. in
    let c =
      [|0.99999999999980993; 676.5203681218851; -1259.1392167224028;
        771.32342877765313; -176.61502916214059; 12.507343278686905;
        -0.13857109526572012; 9.9843695780195716e-6; 1.5056327351493116e-7|]
    in
    let rec ag z d =
      if d = 0 then c.(0) +. ag z 1
      else if d < 8 then c.(d) /. (z +. float d) +. ag z (succ d)
      else c.(d) /. (z +. float d)
    in
    fun z ->
      let z = z -. 1. in
      let p = z +. g +. 0.5 in
      sqrt_two_pi *. p ** (z +. 0.5) *. exp (-. p) *. ag z 0

  let log_gamma x =
    (* XXX TODO: better implementation XXX *)
    log (gamma x)

  (** {2 Distributions} *)

  let sampler (draw, score) =
    Dist_sampler (draw, score)

  (** [dirac x] is a dirac distribution.
      @see<https://en.wikipedia.org/wiki/Dirac_delta_function>
  *)
  let dirac x =
    Dist_support [ x, 1. ]

  (** [bernoulli theta] is a bernoulli distribution of parameter theta.
      @see<https://en.wikipedia.org/wiki/Bernoulli_distribution>
  *)
  let bernoulli_draw p =
    Random.float 1.0 < p

  let bernoulli_score p v =
    if v then log p else log (1. -. p)

  let bernoulli_mean p = p

  let bernoulli_variance p = p *. (1. -. p)

  let bernoulli p =
    assert (0. <= p && p <= 1.);
    Dist_bernoulli p


  (** [gaussian(mu, sigma2)] is a normal distribution of mean [mu] and
      variance [sigma2].
      @see<https://en.wikipedia.org/wiki/Normal_distribution>
  *)

  let gaussian_draw =
    let rec rand_pair () =
      let u1 = Random.float 1.0 in
      let u2 = Random.float 1.0 in
      if u1 < epsilon_float then rand_pair ()
      else u1, u2
    in
    fun mu sigma2 ->
      let u1, u2 = rand_pair() in
      let z = sqrt (-.2. *. log u1) *. cos (two_pi *. u2) in
      z *. sqrt sigma2 +. mu

  let gaussian_score mu sigma2 x =
    -. 0.5 *. log (two_pi *. sigma2) -.
    (x -. mu) ** 2. /. (2. *. sigma2)

  let gaussian_mean mu _sigma2 =
    mu

  let gaussian_variance _mu sigma2 =
    sigma2

  let gaussian (mu, sigma2) =
    Dist_gaussian (mu, sigma2)

  let normal (mu, sigma) = 
    Dist_gaussian (mu, sigma ** 2.)

  let lognormal_draw mu sigma = 
    let y = gaussian_draw mu (sigma ** 2.) in
    exp y

  let lognormal_score mu sigma x = 
    gaussian_score mu (sigma ** 2.) (log x)

  let lognormal_mean mu sigma = 
    exp (mu +. sigma ** 2. /. 2.)

  let lognormal_variance mu sigma = 
    (exp sigma ** 2. -. 1.) *. exp (2. *. mu +. sigma ** 2.)

  let lognormal (mu, sigma) = 
    Dist_lognormal (mu, sigma)

  (** [mv_gaussian(mu, sigma)] is a multivariate normal distribution of
      mean [mu] and standard deviation [sigma].
      @see<https://en.wikipedia.org/wiki/Multivariate_normal_distribution>
  *)

  let mv_gaussian_draw' mu _sigma a =
    (* let n, _ = Mat.shape mu in *)
    let n = (Arr.shape mu).(0) in
    let xs = Arr.init [| n; 1 |] (fun _ -> gaussian_draw 0. 1.) in
    (* V1 *)
    (* Mat.(mu + a *@ xs) *)
    (* V2 *)
    (* let res = Mat.copy mu in *)
    (* Mat.dot_ a xs ~beta:1. ~c:res; *)
    (* res *)
    (* V3 *)
    let res = Mat.(a *@ xs) in
    Mat.add_ ~out:res mu res;
    res

  let mv_gaussian_draw mu sigma =
    let sig_svd = Linalg.Generic.svd sigma in
    let u, s, _ = sig_svd in
    let a = Mat.(u *@ (sqrt (diagm s))) in
    mv_gaussian_draw' mu sigma a

  let mv_gaussian_score' mu _sigma sig_inv sig_det x =
    let d =  (Arr.shape x).(0) in
    let n, k =  Mat.shape x in
    let x_m = Mat.(x - mu) in
    (* V1 *)
  (* -. 0.5 *. log (two_pi ** float d *. Linalg.D.det sigma) *)
  (* -. 0.5 *. Mat.(get (transpose (Linalg.D.linsolve sigma x_m) *@ x_m) 0 0) *)
    (* V2 *)
  (* -. 0.5 *. log ((two_pi ** float d) *. sig_det) *)
  (* -. 0.5 *. Mat.(get (transpose x_m *@ sig_inv *@ x_m) 0 0) *)
    (* V3 *)
    let aux = Mat.empty k n in
    Mat.dot_ ~transa:true ~c:aux x_m sig_inv;
    -. 0.5 *. log ((two_pi ** float d) *. sig_det)
    -. 0.5 *. Mat.(get (aux *@ x_m) 0 0)


  let mv_gaussian_score mu sigma x =
    let sig_inv = Linalg.D.inv sigma in
    let sig_det = Linalg.D.det sigma in
    mv_gaussian_score' mu sigma sig_inv sig_det x

  let mv_gaussian (mu, sigma) =
    Dist_mv_gaussian (mu, sigma, None)

  let mv_gaussian_ext sigma =
    let sig_inv = Linalg.D.inv sigma in
    let sig_det = Linalg.D.det sigma in
    let sig_svd = Linalg.Generic.svd sigma in
    let u, s, _ = sig_svd in
    let a = Mat.(u *@ (sqrt (diagm s))) in
    { mvg_inv_sigma = sig_inv;
      mvg_det_sigma = sig_det;
      mvg_draw_cache = a; }

  let mv_gaussian_curried sigma =
    let ext = mv_gaussian_ext sigma in
    fun mu ->
      Dist_mv_gaussian (mu, sigma, Some ext)


  (** [beta(a, b)] is a beta distribution of parameters [a] and [b].
      @see<https://en.wikipedia.org/wiki/Beta_distribution>
  *)

  let beta_draw =
    let rec exp_gamma_sample shape scale =
      if (shape < 1.) then
        let r =
          exp_gamma_sample (1. +. shape) scale +. log (Random.float 1.) /. shape
        in
        if r = neg_infinity then
          (* log gamma sample underflow, rounded to nearest representable
             support value *)
          min_float
        else
          r
      else
        let d = shape -. 1. /. 3. in
        let c = 1. /. sqrt (9. *. d) in
        let rec loop () =
          let x = ref (gaussian_draw 0. 1.) in
          let v = ref (1. +. c *. !x) in
          while !v <= 0. do
            x := gaussian_draw 0. 1.;
            v := 1. +. c *. !x;
          done;
          let log_v = 3. *. log !v in
          v := !v *. !v *. !v;
          let u = Random.float 1. in
          if ((u < 1. -. 0.331 *. !x *. !x *. !x *. !x)
              || (log u < 0.5 *. !x *. !x +. d *. (1. -. !v +. log !v))) then
            log scale +. log d +. log_v
          else
            loop ()
        in
        loop ()
    in
    fun a b ->
      let log_x = exp_gamma_sample a 1. in
      let log_y = exp_gamma_sample b 1. in
      let v = 1. /. (1. +. exp (log_y -. log_x)) in
      if v = 0. then
        (* beta sample underflow, rounded to nearest representable
           support value *)
        min_float
      else if v = 1. then
        (* beta sample overflow, rounded to nearest representable
           support value *)
        1. -. epsilon_float /. 2.
      else v

  let beta_score =
    let log_beta a b =
      log_gamma a +. log_gamma b -. log_gamma (a +. b)
    in
    fun a b x ->
      if x > 0. && x < 1. then
        (a -. 1.) *. log x +. (b -. 1.) *. log (1. -. x) -. log_beta a b
      else
        neg_infinity

  let beta_mean a b =
    a /. (a +. b)

  let beta_variance a b =
    a *. b /. ((a +. b) *. (a +. b) *. (a +. b +. 1.))

  let beta (a, b) =
    assert (a > 0.);
    assert (b > 0.);
    Dist_beta (a, b)

  (** [sph_gaussian(mus, sigma2s)] is a spherical normal distribution.
      @see<https://en.wikipedia.org/wiki/Multivariate_normal_distribution>
  *)
  let sph_gaussian (mus, sigma2s) =
    Dist_list (List.map2 (fun mu sigma2 -> gaussian(mu, sigma2)) mus sigma2s)


  (** [uniform_int(low, up)] is a uniform distribution over integers
      between [low] and [up] included.
      @see<https://en.wikipedia.org/wiki/Discrete_uniform_distribution>
  *)
  let uniform_int_draw low up =
    Random.int (up - low + 1) + low

  let uniform_int_score low up n =
    if low <= n && n <= up then
      -. log (float (up - low))
    else
      neg_infinity

  let uniform_int_mean low up =
    float (low + up) /. 2.

  let uniform_int_variance low up =
    float ((up - low + 1) * (up - low + 1) - 1) /. 12.

  let uniform_int (low, up) =
    Dist_uniform_int (low, up)

  (** [uniform_float(low, up)] is a uniform distribution over floating
      points number between [low] and [up] included.
      @see<https://en.wikipedia.org/wiki/Uniform_distribution_(continuous)>
  *)

  let uniform_float_draw low up =
    Random.float (up -. low) +. low

  let uniform_float_score low up n =
    if low <= n && n <= up then
      -. log (up -. low)
    else
      neg_infinity

  let uniform_float_mean low up =
    (low +. up) /. 2.

  let uniform_float_variance low up =
    (up -. low) ** 2. /. 12.

  let uniform_float (low, up) =
    Dist_uniform_float (low, up)


  (** [uniform_list l] is a categorical distribution where each element
      is equiprobable.
      @see<https://en.wikipedia.org/wiki/Categorical_distribution>
  *)
  let uniform_list l =
    let p = 1. /. float (List.length l) in
    Dist_support (List.rev_map (fun x -> (x, p)) l)


  (** [weighted_list l] is a categorical distribution where each element
      (x_i, w_i) has the probability w_i / (sum_i w_i)
  *)
  let weighted_list l =
    let n = List.fold_left (fun n (w, _) -> n +. w) 0. l in
    Dist_support (List.rev_map (fun (w, x) -> x, w /. n) l)

  let shuffle l =
    let sample_fn _ =
      let arr = Array.of_list l in
      for n = Array.length arr - 1 downto 1 do
        let k = Random.int (n + 1) in
        let tmp = arr.(n) in
        arr.(n) <- arr.(k);
        arr.(k) <- tmp
      done;
      Array.to_list arr
    in

    let obs_fn _ = assert false in (* XXX: Implement *)

    Dist_sampler (sample_fn, obs_fn)

  (** [exponential lambda] is an exponential distribution of parameter lambda.
      @see<https://en.wikipedia.org/wiki/Exponential_distribution>
  *)

  let exponential_draw lambda =
    let u = Random.float 1. in
    -. log u /. lambda

  let exponential_score lambda x =
    if x >= 0. then log lambda -. lambda *. x
    else neg_infinity

  let exponential_mean lambda =
    1. /. lambda

  let exponential_variance lambda =
    1. /. (lambda ** 2.)

  let exponential lambda =
    assert (lambda > 0.);
    Dist_exponential lambda


  (** [poisson lambda] is an poisson distribution of parameter lambda.
      @see<https://en.wikipedia.org/wiki/Poisson_distribution>
  *)

  let poisson_draw lambda =
    let rec draw t k =
      let t = t +. exponential_draw lambda in
      if t > 1. then k
      else draw t (k+1)
    in
    draw 0. 0

  let poisson_score lambda x =
    if x < 0 then
      neg_infinity
    else
      float_of_int x *. log lambda -. lambda -. log_gamma (float_of_int (x + 1))

  let poisson_mean lambda =
    lambda

  let poisson_variance lambda =
    lambda

  let poisson lambda =
    assert (lambda > 0.);
    Dist_poisson lambda


  (** [alias_method_unsafe values probabilities] is the [alias_method]
      where the arrays [values] and [probabilities] are not copied.
  *)
  let alias_method_unsafe values probabilities =
    let size = Array.length values in
    let size_f = float size in
    let probability = Array.create_float size in
    let alias = Array.make size 0 in
    let average = 1.0 /. size_f in
    let _, small, large =
      Array.fold_left
        (fun (i, small, large) p ->
           if p >= average then (i + 1, small, i :: large)
           else (i + 1, i :: small, large))
        (0, [], []) probabilities
    in
    let rec while_ small large =
      begin match small, large with
      | [], [] -> ()
      | less :: small, more :: large ->
          probability.(less) <- probabilities.(less) *. size_f;
          alias.(less) <- more;
          probabilities.(more) <-
            (probabilities.(more) +. probabilities.(less)) -. average;
          if (probabilities.(more) >= 1.0 /. size_f) then while_ small (more :: large)
          else while_ (more :: small) large
      | less :: small, [] ->
          probability.(less) <- 1.0;
          while_ small []
      | [], more :: large ->
          probability.(more) <- 1.0;
          while_ [] large
      end
    in
    while_ small large;
    let draw () =
      let column = Random.int size in
      let coin_toss = Random.float 1. < probability.(column) in
      if coin_toss then values.(column) else values.(alias.(column))
    in
    let score x =
      let exception Idx of int in
      try
        Array.iteri (fun i v -> if x = v then raise (Idx i)) values;
        neg_infinity
      with Idx i ->
        log (probabilities.(i))
    in
    Dist_sampler (draw, score)


  (** [alias_method support] is the [alias_method] where [support] is a
      pair [(x, p)] of a value [x] of probability [p].
  *)
  let alias_method_list support =
    let size = List.length support in
    let values =
      begin match support with
      | [] -> assert false
      | (d, _) :: _ -> Array.make size d
      end
    in
    let probabilities = Array.create_float size in
    List.iteri (fun i (v, p) -> values.(i) <- v; probabilities.(i) <- p) support;
    alias_method_unsafe values probabilities

  (** [alias_method values probabilities] is a discrete distribution where
      the value [value.(i)] has the probability [probabilities.(i)].
      @see<https://en.wikipedia.org/wiki/Alias_method>
  *)
  let alias_method values probabilities =
    let values = Array.copy values in
    let probabilities = Array.copy probabilities in
    alias_method_unsafe values probabilities


  (** [add (d1, d2)] is the sum of two distributions. *)
  let add : float t * float t -> float t =
    fun (dist1, dist2) ->
    begin match dist1, dist2 with
    | Dist_support s1, Dist_support s2 ->
        Dist_support (flatten_suport (+.) s1 s2)
    | (Dist_support _, _) | (_, Dist_support _)
    | (Dist_sampler _, _) | (_, Dist_sampler _)
    | (Dist_sampler_float _, _) | (_, Dist_sampler_float _)
    | (Dist_mixture _, _) | (_, Dist_mixture _)
    | (Dist_gaussian (_, _), _) | (_, Dist_gaussian (_, _))
    | (Dist_lognormal (_, _), _) | (_, Dist_lognormal (_, _))
    | (Dist_beta (_, _), _) | (_, Dist_beta (_, _))
    | (Dist_uniform_float (_, _), _) | (_, Dist_uniform_float (_, _))
    | (Dist_exponential _, _) | (_, Dist_exponential _)
    | (Dist_add (_, _), _) | (_, Dist_add (_, _))
    | (Dist_mult (_, _), _) | (_, Dist_mult (_, _))
    | (Dist_app (_, _), _) | (_, Dist_app (_, _))
    | (Dist_joint _, _) | (_, Dist_joint _) ->
        (* XXX TODO XXX *)
        Dist_add (dist1, dist2)
    | (Dist_mv_gaussian (_, _, _), Dist_mv_gaussian (_, _, _)) ->
        assert false
    end

  (** [mult (d1, d2)] is the multiplication of two distributions. *)
  let mult : float t * float t -> float t =
    fun (dist1, dist2) ->
    begin match dist1, dist2 with
    | Dist_support s1, Dist_support s2 ->
        Dist_support (flatten_suport ( *. ) s1 s2)
    | (Dist_support _, _) | (_, Dist_support _)
    | (Dist_sampler _, _) | (_, Dist_sampler _)
    | (Dist_sampler_float _, _) | (_, Dist_sampler_float _)
    | (Dist_mixture _, _) | (_, Dist_mixture _)
    | (Dist_gaussian (_, _), _) | (_, Dist_gaussian (_, _))
    | (Dist_lognormal (_, _), _) | (_, Dist_lognormal (_, _))
    | (Dist_beta (_, _), _) | (_, Dist_beta (_, _))
    | (Dist_uniform_float (_, _), _) | (_, Dist_uniform_float (_, _))
    | (Dist_exponential _, _) | (_, Dist_exponential _)
    | (Dist_add (_, _), _) | (_, Dist_add (_, _))
    | (Dist_mult (_, _), _) | (_, Dist_mult (_, _))
    | (Dist_app (_, _), _) | (_, Dist_app (_, _))
    | (Dist_joint _, _) | (_, Dist_joint _) ->
        Dist_mult (dist1, dist2)
    | (Dist_mv_gaussian (_, _, _), Dist_mv_gaussian (_, _, _)) ->
        assert false (* XXX TODO XXX *)
    end

  (** [app (d1, d2)] is the application of two distributions. *)
  let app : ('a -> 'b) t * 'a t -> 'b t =
    fun (dist1, dist2) ->
    Dist_app (dist1, dist2)


  (** {2 Draw and score}*)

  let rec to_dist_support : type a. a t -> a t =
    fun dist ->
    begin match dist with
    | Dist_sampler (_, _) -> assert false
    | Dist_sampler_float (_, _, _) -> assert false
    | Dist_support _ -> dist
    | Dist_mixture _ -> assert false (* XXX TODO XXX *)
    | Dist_pair(_, _) -> assert false (* XXX TODO XXX *)
    | Dist_list _ -> assert false (* XXX TODO XXX *)
    | Dist_array _ -> assert false (* XXX TODO XXX *)
    | Dist_gaussian (_, _) -> assert false
    | Dist_lognormal (_, _) -> assert false
    | Dist_mv_gaussian (_, _, _) -> assert false
    | Dist_beta (_, _) -> assert false
    | Dist_bernoulli p ->
        Dist_support [ (true, p); (false, 1. -. p); ]
    | Dist_uniform_int (a, b) ->
        let p = 1. /. float (b - a) in
        let rec build n acc =
          if n < a then acc
          else
            build (n - 1) ((n, p) :: acc)
        in
        Dist_support (build b [])
    | Dist_uniform_float (_, _) -> assert false
    | Dist_exponential _ -> assert false
    | Dist_poisson _ -> assert false
    | Dist_add (d1, d2) ->
        begin match to_dist_support d1, to_dist_support d2 with
        | Dist_support s1, Dist_support s2 ->
            Dist_support (flatten_suport ( +. ) s1 s2)
        | _, _ -> assert false
        end
    | Dist_mult (d1, d2) ->
        begin match to_dist_support d1, to_dist_support d2 with
        | Dist_support s1, Dist_support s2 ->
            Dist_support (flatten_suport ( *. ) s1 s2)
        | _, _ -> assert false
        end
    | Dist_app (_, _) -> assert false (* XXX TODO XXX *)
    | Dist_joint _ -> assert false (* XXX TODO XXX *)
    end

  (** [draw dist] draws a value form the distribution [dist] *)
  let draw =
    let rec draw : type a. a t -> a =
      fun dist ->
        begin match dist with
        | Dist_sampler (sampler, _) -> sampler ()
        | Dist_sampler_float (sampler, _, _) -> sampler ()
        | Dist_support sup ->
            let sample = Random.float 1.0 in
            (* TODO data structure for more efficient sampling *)
            let rec draw sum r =
              begin match r with
              | [] -> raise Draw_error
              | (v, p) :: r ->
                  let sum = sum +. p in
                  if sample <= sum then v else draw sum r
              end
            in
            draw 0. sup
        | Dist_mixture l ->
            let d' = draw (Dist_support l) in
            draw d'
        | Dist_gaussian (mu, sigma2) -> gaussian_draw mu sigma2
        | Dist_lognormal (mu, sigma) -> lognormal_draw mu sigma
        | Dist_mv_gaussian (mu, sigma, ext) ->
            begin match ext with
            | Some { mvg_draw_cache = a; _ } ->
                mv_gaussian_draw' mu sigma a
            | None ->
                mv_gaussian_draw mu sigma
            end
        | Dist_beta (a, b) -> beta_draw a b
        | Dist_bernoulli p -> bernoulli_draw p
        | Dist_uniform_int (a, b) -> uniform_int_draw a b
        | Dist_uniform_float (a, b) -> uniform_float_draw a b
        | Dist_exponential lambda -> exponential_draw lambda
        | Dist_poisson lambda -> poisson_draw lambda
        | Dist_pair (d1, d2) ->
            (draw d1, draw d2)
        | Dist_list a ->
            List.map (fun ed -> draw ed) a
        | Dist_array a ->
            Array.map (fun ed -> draw ed) a
        | Dist_add (d1, d2) ->
            draw d1 +. draw d2
        | Dist_mult (d1, d2) ->
            draw d1 *. draw d2
        | Dist_app (d1, d2) ->
            (draw d1) (draw d2)
        | Dist_joint j -> draw_joint (Probzelus_utils.copy j)
        end
    and draw_joint : type a. a joint_distr -> a =
      fun dist ->
        begin match dist with
        | JDist_const x -> x
        | JDist_rvar (RV x) -> DS_graph.value x
        | JDist_add (d1, d2) -> draw_joint d1 +. draw_joint d2
        | JDist_mult (d1, d2) -> draw_joint d1 *. draw_joint d2
        | JDist_app (d1, d2) ->
            (draw_joint d1) (draw_joint d2)
        | JDist_pair (d1, d2) ->
            (draw_joint d1, draw_joint d2)
        | JDist_list a ->
            List.map (fun ed -> draw_joint ed) a
        | JDist_array a ->
            Array.map (fun ed -> draw_joint ed) a
        | JDist_ite (i,t,e) ->
            if draw_joint i then
              draw_joint t
            else
              draw_joint e
        | JDist_matrix m ->
            Array.map
              (fun a -> Array.map draw_joint a)
              m
        | JDist_mat_add (e1, e2) ->
            Mat.add (draw_joint e1) (draw_joint e2)
        | JDist_mat_scalar_mul (e1, e2) ->
            Mat.scalar_mul (draw_joint e1) (draw_joint e2)
        | JDist_mat_dot (e1, e2) ->
            Mat.dot (draw_joint e1) (draw_joint e2)
        | JDist_vec_get (m, i) ->
            Mat.get (draw_joint m) i 0
        end
    in
    draw

  (** [score(dist, x)] returns the log probability of the value [x] in the
      distribution [dist].
  *)
  let rec score : type a. a t * a -> log_proba =
    fun (dist, x) ->
    begin match dist with
    | Dist_sampler (_, scorer) -> scorer x
    | Dist_sampler_float (_, scorer, _) -> scorer x
    | Dist_support sup ->
        let p =
          List.fold_left
            (fun acc (y, w) -> if x = y then acc +. w else acc)
            0. sup
        in
        log p
    | Dist_mixture (l) ->
        let p =
          List.fold_left
            (fun acc (d', p) -> acc +. p *. exp(score (d', x)))
            0. l
        in
        log p
    | Dist_gaussian (mu, sigma2) -> gaussian_score mu sigma2 x
    | Dist_lognormal (mu, sigma) -> lognormal_score mu sigma x
    | Dist_mv_gaussian (mu, sigma, Some { mvg_inv_sigma = sig_inv;
                                          mvg_det_sigma = sig_det; _ }) ->
        mv_gaussian_score' mu sigma sig_inv sig_det x
    | Dist_mv_gaussian (mu, sigma, None) ->
        mv_gaussian_score mu sigma x
    | Dist_beta (a, b) -> beta_score a b x
    | Dist_bernoulli p -> bernoulli_score p x
    | Dist_uniform_int (a, b) -> uniform_int_score a b x
    | Dist_uniform_float (a, b) -> uniform_float_score a b x
    | Dist_exponential lambda -> exponential_score lambda x
    | Dist_poisson lambda -> poisson_score lambda x
    | Dist_pair (d1, d2) ->
        (* XXX TO CHECK XXX *)
        let v1, v2 = x in
        score (d1, v1) +. score (d2, v2)
    | Dist_list l ->
        (* XXX TO CHECK XXX *)
        begin try
          List.fold_left2
            (fun acc di xi -> acc +. score (di, xi))
            0. l x
        with Invalid_argument _ ->
          neg_infinity
        end
    | Dist_array a ->
        (* XXX TO CHECK XXX *)
        let len = Array.length a in
        if Array.length x = len then
          let acc = ref 0. in
          for i = 0 to len - 1 do
            acc := !acc +. score (a.(i), x.(i))
          done;
          !acc
        else
          neg_infinity
    | Dist_add (Dist_support [c, 1.], d) -> score (d, x -. c)
    | Dist_add (d, Dist_support [c, 1.]) -> score (d, x -. c)
    | Dist_add (_, _) -> score (to_dist_support dist, x)
    | Dist_mult (Dist_support [c, 1.], d) -> score (d, x /. c)
    | Dist_mult (d, Dist_support [c, 1.]) -> score (d, x /. c)
    | Dist_mult (_, _) -> score (to_dist_support dist, x)
    | Dist_app (_, _) -> assert false (* do not know how to inverse d1 *)
    | Dist_joint j -> score_joint (j, x)
    end
  and score_joint : type a. a joint_distr * a -> log_proba =
    fun (dist, x) ->
    begin match dist with
    | JDist_const c -> if x = c then 0. else neg_infinity
    | JDist_rvar (RV y) -> score (DS_graph.get_distr y, x)
    | _ -> assert false (* XXX TODO XXX *)
    end

  (** [draw dist] draws a value form the distribution [dist] and returns
      its log probability.
  *)
  let draw_and_score : type a. a t -> a * log_proba =
    fun dist ->
    begin match dist with
    | Dist_sampler (sampler, scorer) ->
        let x = sampler () in
        (x, scorer x)
    | Dist_sampler_float (sampler, scorer, _) ->
        let x = sampler () in
        (x, scorer x)
    | Dist_support _ ->
        let x = draw dist in
        (x, score (dist, x))
    (* let sample = Random.float 1.0 in *)
    (* (\* TODO data structure for more efficient sampling *\) *)
    (* let rec draw sum r = *)
    (*   begin match r with *)
    (*     | [] -> assert false *)
    (*     | (v, p) :: r -> *)
    (*         let sum = sum +. p in *)
    (*         if sample <= sum then v, (log p) else draw sum r *)
    (*   end *)
    (* in *)
    (* draw  0. sup *)
    | Dist_mixture _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_pair _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_list _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_array _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_gaussian _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_lognormal _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_mv_gaussian _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_beta _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_bernoulli _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_uniform_int _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_uniform_float _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_exponential _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_poisson _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_add _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_mult _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_app _ ->
        let x = draw dist in
        (x, score (dist, x))
    | Dist_joint _ ->
        let x = draw dist in
        (x, score (dist, x))
    end


  (** {2 Operations on distributions} *)

  (** [of_sampler (draw, score)] builds a distribution from its sampling
      and score functions.
  *)
  let of_sampler (draw, score) =
    Dist_sampler (draw, score)

  (** [of_list dists] builds a distribution of list from a
      list of distributions.
  *)
  let of_list dists =
    Dist_sampler
      ((fun () -> List.map (fun dist -> draw dist) dists),
       (fun xs ->
          List.fold_left2
            (fun acc dist x -> acc +. score(dist, x))
            1.0
            dists xs))

  (** [of_array dists] builds a distribution of array from a
      array of distributions.
  *)
  let of_array dists =
    Dist_array dists

  (** [of_pair (dist1, dist2)] builds a distribution from a pair
      of distributions.
  *)
  let of_pair (dist1, dist2) =
    Dist_pair (dist1, dist2)

  (** [split dist] turns a distribution of pairs into a pair of
      distributions.
  *)
  let rec split : type a b. (a * b) t -> a t * b t =
    fun dist ->
    begin match dist with
    | Dist_sampler (draw, _score) ->
        Dist_sampler ((fun () -> fst (draw ())), (fun _ -> assert false)),
        Dist_sampler ((fun () -> snd (draw ())), (fun _ -> assert false))
    (* | Dist_support support -> *)
    (*     let s1, s2 = *)
    (*       List.fold_right *)
    (*         (fun ((a, b), p) (acc1, acc2) -> *)
    (*            let add_p o = *)
    (*              begin match o with *)
    (*              | None -> p *)
    (*              | Some p' -> p +. p' *)
    (*              end *)
    (*            in *)
    (*            (Misc_lib.list_replace_assoc a add_p acc1, *)
    (*             Misc_lib.list_replace_assoc b add_p acc2)) *)
    (*         support *)
    (*         ([], []) *)
    (*     in *)
    (*     (Dist_support s1, Dist_support s2) *)
    | Dist_support support ->
        let s1, s2 =
          List.fold_left
            (fun (acc1, acc2) ((a, b), p) -> ((a,p)::acc1, (b,p)::acc2))
            ([], [])
            support
        in
        (Dist_support s1, Dist_support s2)
    | Dist_mixture l ->
        let s1, s2 =
          List.fold_left
            (fun (acc1, acc2) (d, p) ->
               let d1, d2 = split d in
               ((d1,p)::acc1, (d2,p)::acc2))
            ([], [])
            l
        in
        (Dist_mixture s1, Dist_mixture s2)
    | Dist_pair (d1, d2) ->
        d1, d2
    | Dist_app (d1, d2) ->
        Dist_sampler ((fun () -> fst ((draw d1) (draw d2))), (fun _ -> assert false)),
        Dist_sampler ((fun () -> snd ((draw d1) (draw d2))), (fun _ -> assert false))
    | Dist_mv_gaussian (_, _, _) -> assert false
    | Dist_joint j -> split_joint j
    end
  and split_joint : type a b. (a * b) joint_distr -> a t * b t =
    fun dist ->
    begin match dist with
    | JDist_const (a, b) ->
        Dist_support [ a, 1. ], Dist_support [ b, 1. ]
    | JDist_rvar (RV x) -> split (DS_graph.get_distr x)
    | JDist_app (d1, d2) ->
        let d1 = Dist_joint d1 in
        let d2 = Dist_joint d2 in
        Dist_sampler ((fun () -> fst ((draw d1) (draw d2))),
                      (fun _ -> assert false)),
        Dist_sampler ((fun () -> snd ((draw d1) (draw d2))),
                      (fun _ -> assert false))
    | JDist_pair (d1, d2) -> Dist_joint d1, Dist_joint d2
    | JDist_ite (d, d1, d2) ->
        let d11, d12 = split_joint d1 in
        let d21, d22 = split_joint d2 in
        (Dist_joint
           (JDist_ite (d,
                       JDist_rvar (RV (DS_graph.assume_constant d11)),
                       JDist_rvar (RV (DS_graph.assume_constant d21)))),
         Dist_joint
           (JDist_ite (d,
                       JDist_rvar (RV (DS_graph.assume_constant d12)),
                       JDist_rvar (RV (DS_graph.assume_constant d22)))))
    | JDist_mat_add _
    | JDist_mat_scalar_mul _
    | JDist_mat_dot _ -> assert false
    end

  (** [split_array dist] turns a distribution of arrays into an array of
      distributions.
  *)
  let rec split_array : type a. a array t -> a t array =
    fun dist ->
    begin match dist with
    | Dist_sampler (draw, _score) ->
        (* We assume that all arrays in the distribution have the same length. *)
        let len = Array.length (draw ()) in
        Array.init len
          (fun i ->
             let draw () = (draw ()).(i) in
             let score _ = assert false in
             Dist_sampler (draw, score))
    | Dist_support [] -> Array.make 0 (Dist_support [])
    | Dist_support (((a0, _) :: _) as support) ->
        let supports = Array.make (Array.length a0) [] in
        List.iter
          (fun (a, p) ->
             Array.iteri
               (fun i v -> supports.(i) <- (v, p) :: supports.(i))
               a)
          support;
        Array.map (fun supp -> Dist_support supp) supports
    | Dist_mixture [] -> Array.make 0 (Dist_mixture [])
    | Dist_mixture ((d0, p0) :: l) ->
        let a0 = split_array d0 in
        let accs = Array.map (fun d -> [(d,p0)]) a0 in
        List.iter
          (fun (di, pi) ->
             let ai = split_array di in
             Array.iteri (fun i d -> accs.(i) <- (d, pi) :: accs.(i)) ai)
          l;
        Array.map (fun acc -> Dist_mixture acc) accs
    | Dist_array a -> a
    | Dist_app (_, _) ->
        (* We assume that all arrays in the distribution have the same length. *)
        let len = Array.length (draw dist) in
        Array.init len
          (fun i ->
             let draw () = (draw dist).(i) in
             let score _ = assert false (* XXX TODO XXX *) in
             Dist_sampler (draw, score))
    | Dist_mv_gaussian (_, _, _) -> assert false
    | Dist_joint j -> split_joint_array j
    end
  and split_joint_array : type a. a array joint_distr -> a t array =
    fun dist ->
    begin match dist with
    | JDist_const a ->
        Array.map (fun x -> Dist_support [ x, 1. ]) a
    | JDist_rvar (RV x) ->
        split_array (DS_graph.get_distr x)
    | JDist_app (_, _) -> assert false (* XXX TODO XXX *)
    | JDist_array a -> Array.map (fun x -> Dist_joint x) a
    | JDist_ite (d, d1, d2) ->
        let a1 = split_joint_array d1 in
        let a2 = split_joint_array d2 in
        Array.map2
          (fun j1 j2 ->
             let d1 = JDist_rvar (RV (DS_graph.assume_constant j1)) in
             let d2 = JDist_rvar (RV (DS_graph.assume_constant j2)) in
             Dist_joint (JDist_ite (d, d1, d2)))
          a1 a2
    | JDist_matrix _
    | JDist_mat_add _
    | JDist_mat_scalar_mul _
    | JDist_mat_dot _ -> assert false
    end

  let rec split_matrix : type a. a array array t -> a t array array =
    fun dist ->
    begin match dist with
    | Dist_array a -> Array.map split_array a
    | Dist_support [] -> Array.make_matrix 0 0 (Dist_support [])
    | Dist_support (((m0, _) :: _) as support) ->
        let n = (Array.length m0) in
        let m = (Array.length m0.(0)) in
        let supports = Array.make_matrix n m [] in
        List.iter
          (fun (m, p) ->
             Array.iteri
               (fun i ai ->
                  Array.iteri
                    (fun j v -> supports.(i).(j) <- (v, p) :: supports.(i).(j))
                    ai)
               m)
          support;
        Array.map
          (fun ai ->
             Array.map
               (fun supp -> Dist_support supp)
               ai)
          supports
    | Dist_mixture [] -> Array.make_matrix 0 0 (Dist_mixture [])
    | Dist_mixture ((d0, p0) :: l) ->
        let m0 = split_matrix d0 in
        let accs =
          Array.map (fun ai -> (Array.map (fun d -> [(d,p0)]) ai)) m0
        in
        List.iter
          (fun (di, pi) ->
             let mi = split_matrix di in
             Array.iteri
               (fun i ai ->
                  Array.iteri
                    (fun j d -> accs.(i).(j) <- (d, pi) :: accs.(i).(j))
                    ai)
               mi)
          l;
        Array.map (fun ai -> Array.map (fun acc -> Dist_mixture acc) ai) accs
    | Dist_sampler _ -> assert false (* XXX TODO XXX *)
    | Dist_app _ -> assert false (* XXX TODO XXX *)
    | Dist_mv_gaussian _ -> assert false
    | Dist_joint j -> split_joint_matrix j
    end
  and split_joint_matrix :
    type a. a array array joint_distr -> a t array array =
    fun dist ->
    begin match dist with
    | JDist_const m ->
        Array.map
          (fun a -> Array.map (fun x -> Dist_support [ x, 1. ]) a)
          m
    | JDist_rvar (RV x) ->
        split_matrix (DS_graph.get_distr x)
    | JDist_app (_, _) -> assert false (* XXX TODO XXX *)
    | JDist_array m ->
        Array.map split_joint_array m
    | JDist_ite _ -> assert false (* XXX TODO XXX *)
    | JDist_matrix _
    | JDist_mat_add _
    | JDist_mat_scalar_mul _
    | JDist_mat_dot _ -> assert false
    end

  (** [split_list dist] turns a distribution of lists into a list of
      distributions.
  *)
  let rec split_list : type a. a list t -> a t list =
    let rec map2' f1 f2 f12 l1 l2 =
      begin match l1, l2 with
      | l1, [] -> List.map f1 l1
      | [], l2 -> List.map f2 l2
      | x1::l1, x2::l2 -> f12 x1 x2 :: (map2' f1 f2 f12 l1 l2)
      end
    in
    fun dist ->
      begin match dist with
      | Dist_sampler (_draw, _score) ->
          assert false (* XXX TODO XXX *)
      | Dist_support [] -> []
      | Dist_support sup ->
          let split =
            List.fold_left
              (fun accs (l, w) ->
                 map2'
                   (fun acc -> acc)
                   (fun v -> [(v, w)])
                   (fun acc v -> (v, w)::acc)
                   accs l)
              [] sup
          in
          List.map (fun l -> Dist_support l) split
      | Dist_mixture [] -> []
      | Dist_mixture (l) ->
          let l =
            List.fold_left
              (fun accs (d, w) ->
                 let l = split_list d in
                 map2'
                   (fun acc -> acc)
                   (fun d -> [(d, w)])
                   (fun acc d -> (d, w)::acc)
                   accs l)
              [] l
          in
          List.map (fun l -> Dist_mixture l) l
      | Dist_list l -> l
      | Dist_app (_, _) ->
          assert false (* XXX TODO XXX *)
      | Dist_mv_gaussian (_, _, _) -> assert false
      | Dist_joint j -> split_joint_list j
      end
  and split_joint_list : type a. a list joint_distr -> a t list =
    fun dist ->
    begin match dist with
    | JDist_const l ->
        List.map (fun x -> Dist_support [x, 1.]) l
    | JDist_rvar (RV x) ->
        split_list (DS_graph.get_distr x)
    | JDist_app (_, _) -> assert false (* XXX TODO XXX *)
    | JDist_list l -> List.map (fun x -> Dist_joint x) l
    | JDist_ite (d, d1, d2) ->
        let l1 = split_joint_list d1 in
        let l2 = split_joint_list d2 in
        List.map2
          (fun j1 j2 ->
             let d1 = JDist_rvar (RV (DS_graph.assume_constant j1)) in
             let d2 = JDist_rvar (RV (DS_graph.assume_constant j2)) in
             Dist_joint (JDist_ite (d, d1, d2)))
          l1 l2
    | JDist_mat_add _
    | JDist_mat_scalar_mul _
    | JDist_mat_dot _ -> assert false
    end

  (** [to_mixture d] turns a distribution of distributions into a
      mixture distribution.
      https://en.wikipedia.org/wiki/Mixture_distribution
  *)
  let rec to_mixture : type a. a t t -> a t =
    fun d ->
    begin match d with
    | Dist_sampler (_draw, _score) ->
        assert false (* XXX TODO XXX *)
    | Dist_support [ (d, _) ] ->
        d
    | Dist_support l ->
        Dist_mixture l
    | Dist_mixture [ (d, _) ] ->
        to_mixture d
    | Dist_mixture l ->
        Dist_mixture (List.map (fun (d, w) -> (to_mixture d, w)) l)
    | Dist_app _ ->
        assert false (* XXX TODO XXX *)
    | Dist_mv_gaussian (_, _, _) -> assert false
    | Dist_joint j -> to_mixture_joint j
    end
  and to_mixture_joint : type a. a t joint_distr -> a t =
    fun dist ->
    begin match dist with
    | JDist_const d -> d
    | JDist_rvar (RV x) -> to_mixture (DS_graph.get_distr x)
    | JDist_ite (d, d1, d2) ->
        let d1 = to_mixture_joint d1 in
        let d2 = to_mixture_joint d2 in
        let j1 = JDist_rvar (RV (DS_graph.assume_constant d1)) in
        let j2 = JDist_rvar (RV (DS_graph.assume_constant d2)) in
        Dist_joint (JDist_ite (d, j1, j2))
    | JDist_app (_, _) -> assert false (* XXX TODO XXX *)
    | JDist_mat_add _
    | JDist_mat_scalar_mul _
    | JDist_mat_dot _ -> assert false
    end

  (** [to_signal d] turns a distribution of signals into a signal that
      containts the distribution of present values. *)
  let rec to_signal : type a. (a * bool) t -> a t * bool =
    fun d ->
    begin match d with
    | Dist_sampler (draw, score) ->
        let rec sample () =
          begin match draw () with
          | (v, true) -> v
          | (_, false) -> sample ()
          end
        in
        let rec pres n =
          if n <= 0 then false
          else
            begin match draw () with
            | (_, true) -> true
            | (_, false) -> pres (n - 1)
            end
        in
        (Dist_sampler((fun () -> sample ()), (fun x -> score (x, true))),
         pres 10000)
    | Dist_support sup ->
        let pres, norm, sup =
          List.fold_left
            (fun (pres, sum, sup) ((v, b), w) ->
               if b then
                 (true, sum +. w, (v, w) :: sup)
               else
                 (pres, sum, sup))
            (false, 0., []) sup
        in
        (Dist_support (List.map (fun (v, w) -> (v, w /. norm)) sup), pres)
    | Dist_mixture l ->
        let pres, norm, l =
          List.fold_left
            (fun (pres, sum, l) (d, w) ->
               begin match to_signal d with
               | d', true -> true, sum +. w, (d', w) :: l
               | _, false -> pres, sum, l
               end)
            (false, 0., []) l
        in
        (Dist_mixture (List.map (fun (v, w) -> (v, w /. norm)) l), pres)
    | Dist_pair _ -> assert false
    | Dist_app _ -> assert false
    | Dist_mv_gaussian (_, _, _) -> assert false
    | Dist_joint j -> to_signal_joint j
    end
  and to_signal_joint : type a. (a * bool) joint_distr -> a t * bool =
    fun d ->
    begin match d with
    | JDist_const (x, b) -> (Dist_support [x, 1.], b)
    | JDist_rvar (RV x) -> to_signal (DS_graph.get_distr x)
    | JDist_app (_, _) -> assert false (* XXX TODO XXX *)
    | JDist_ite (_, _, _) ->
        let sample () = draw (Dist_joint d) in
        let score _x = assert false (* score (Dist_joint d) x *) in
        to_signal (Dist_sampler (sample, score))
    | JDist_pair _
    | JDist_mat_add _
    | JDist_mat_scalar_mul _
    | JDist_mat_dot _ -> assert false
    end

  (** [stats_float d] computes the mean and variance of a [float
      Distribution.t].
  *)
  let rec stats_float : float t -> float * float =
    fun dist ->
    begin match dist with
    | Dist_sampler (draw, _) ->
        let rec stats n sum sq_sum =
          begin match n with
          | 100000 ->
              let mean = sum /. (float n) in
              let var = sq_sum /. (float n) -. mean *. mean in
              mean, var
          | _ ->
              let x = draw () in
              stats (n+1) (sum +. x) (sq_sum +. x *. x)
          end
        in stats 0 0. 0.
    | Dist_sampler_float (_, _, stats) ->
        stats ()
    | Dist_gaussian (mu, sigma2) ->
        (gaussian_mean mu sigma2, gaussian_variance mu sigma2)
    | Dist_lognormal (mu, sigma) ->
        (lognormal_mean mu sigma, lognormal_variance mu sigma)
    | Dist_beta (a, b) ->
        (beta_mean a b, beta_variance a b)
    | Dist_uniform_float (a, b) ->
        (uniform_float_mean a b, uniform_float_variance a b)
    | Dist_exponential a ->
        (exponential_mean a, exponential_variance a)
    | Dist_support sup ->
        let rec stats sup sum sq_sum =
          begin match sup with
          | [] ->
              let mean = sum in
              let var = sq_sum  -. mean *. mean in
              mean, var
          | (v,w) :: t ->
              stats t (sum +. v *. w) (sq_sum +. w *. v *. v)
          end
        in stats sup 0. 0.
    | Dist_mixture l ->
        (* https://stats.stackexchange.com/questions/16608/what-is-the-variance-of-the-weighted-mixture-of-two-gaussians *)
        let rec stats l sum_mu sum_mu2 sum_sigma2 =
          begin match l with
          | [] ->
              let mean = sum_mu in
              let var = sum_sigma2 +. sum_mu2 -. sum_mu *. sum_mu in
              (mean, var)
          | (d, w) :: l ->
              let m, sigma2 = stats_float d in
              stats l
                (sum_mu +. w *. m)
                (sum_mu2 +. w *. m *. m)
                (sum_sigma2 +. w *. sigma2)
          end
        in
        stats l 0. 0. 0.
    | Dist_add (d1, d2) ->
        let m1, var1 = stats_float d1 in
        let m2, var2 = stats_float d2 in
        m1 +. m2, var1 +. var2
    | Dist_mult (d1, d2) ->
        let m1, var1 = stats_float d1 in
        let m2, var2 = stats_float d2 in
        m1 *. m2, var1 *. var2 +. var1 *. m2 ** 2. +. m2 *. m1 ** 2.
    | Dist_app (_, _) as d ->
        stats_float (Dist_sampler ((fun () -> draw d), (fun _ -> assert false)))
    | Dist_mv_gaussian (_, _, _) -> assert false
    | Dist_joint j -> stats_float_joint j
    end
  and stats_float_joint : float joint_distr -> float * float =
    fun dist ->
    begin match dist with
    | JDist_const x -> x, Float.epsilon
    | JDist_rvar (RV x) -> stats_float (DS_graph.get_distr x)
    | JDist_add (d1, d2) ->
        let m1, var1 = stats_float_joint d1 in
        let m2, var2 = stats_float_joint d2 in
        m1 +. m2, var1 +. var2
    | JDist_mult (d1, d2) ->
        let m1, var1 = stats_float_joint d1 in
        let m2, var2 = stats_float_joint d2 in
        m1 *. m2, var1 *. var2 +. var1 *. m2 ** 2. +. m2 *. m1 ** 2.
    | JDist_app _ -> assert false (* XXX TODO XXX *)
    | JDist_ite _ -> assert false (* XXX TODO XXX *)
    | JDist_vec_get _ -> assert false (* XXX TODO XXX *)
    | JDist_mat_add _
    | JDist_mat_scalar_mul _
    | JDist_mat_dot _ -> assert false
    end

  (** [mean_float d] computes the mean of a [float Distribution.t]. *)
  let rec mean_float d =
    begin match d with
    | Dist_sampler (draw, _) ->
        let n = 100000 in
        let acc = ref 0. in
        for _ = 1 to n do acc := !acc +. draw () done;
        !acc /. (float n)
    | Dist_sampler_float (_, _, stats) ->
        fst (stats())
    | Dist_support sup ->
        List.fold_left (fun acc (v, w) -> acc +. v *. w) 0. sup
    | Dist_gaussian (a, b) -> gaussian_mean a b
    | Dist_lognormal (a, b) -> lognormal_mean a b
    | Dist_beta (a, b) -> beta_mean a b
    | Dist_uniform_float (a, b) -> uniform_float_mean a b
    | Dist_exponential a -> exponential_mean a
    | Dist_mixture l ->
        List.fold_left (fun acc (d, w) -> acc +. w *. mean_float d) 0. l
    | Dist_add (d1, d2) ->
        let m1= mean_float d1 in
        let m2 = mean_float d2 in
        m1 +. m2
    | Dist_mult (d1, d2) ->
        let m1 = mean_float d1 in
        let m2 = mean_float d2 in
        m1 *. m2
    | Dist_app (_, _) ->
        mean_float (Dist_sampler ((fun () -> draw d), (fun _ -> assert false)))
    | Dist_mv_gaussian (_, _, _) -> assert false
    | Dist_joint j -> mean_float_joint j
    end
  and mean_float_joint : float joint_distr -> float =
    fun dist ->
    begin match dist with
    | JDist_const x -> x
    | JDist_rvar (RV x) -> mean_float (DS_graph.get_distr x)
    | JDist_add (d1, d2) ->
        let m1 = mean_float_joint d1 in
        let m2 = mean_float_joint d2 in
        m1 +. m2
    | JDist_mult (d1, d2) ->
        let m1 = mean_float_joint d1 in
        let m2 = mean_float_joint d2 in
        m1 *. m2
    | JDist_app _ -> assert false (* XXX TODO XXX *)
    | JDist_ite _ -> assert false (* XXX TODO XXX *)
    | JDist_vec_get _ -> assert false (* XXX TODO XXX *)
    | JDist_mat_add _
    | JDist_mat_scalar_mul _
    | JDist_mat_dot _ -> assert false
    end

  (** [stats_float_list d] computes the mean and variance of a
      [float list Distribution.t].
  *)
  let stats_float_list d =
    let ls = split_list d in
    List.map (fun l -> stats_float l) ls

  (** [mean_float_list d] computes the means of a [float list Distribution.t].
  *)
  let mean_float_list d =
    let ls = split_list d in
    List.map (fun l -> mean_float l) ls

  let rec mean : type a. (a -> float) -> a t -> float =
    let sample_mean meanfn draw =
      let n = 100000 in
      let acc = ref 0. in
      for _ = 1 to n do acc := !acc +. (meanfn (draw ())) done;
      !acc /. (float n)
    in
    begin fun meanfn dist  ->
      match dist with
      | Dist_sampler (draw, _) -> sample_mean meanfn draw
      | Dist_sampler_float (draw, _ , _) -> sample_mean meanfn draw
      | Dist_gaussian (mu, sigma2) ->
          sample_mean meanfn (fun () -> gaussian_draw mu sigma2)
      | Dist_lognormal (mu, sigma) ->
          sample_mean meanfn (fun () -> lognormal_draw mu sigma)
      | Dist_mv_gaussian (mu, sigma, _) ->
          sample_mean meanfn (fun () -> mv_gaussian_draw mu sigma)
      | Dist_beta (a, b) -> beta_draw a b
      | Dist_bernoulli p ->
          (p *. (meanfn true)) +. ((1. -. p) *. (meanfn false))
      | Dist_uniform_int (a, b) ->
          sample_mean meanfn (fun () -> uniform_int_draw a b)
      | Dist_uniform_float (a, b) ->
          sample_mean meanfn (fun () -> uniform_float_draw a b)
      | Dist_exponential lambda ->
          sample_mean meanfn (fun () -> exponential_draw lambda)
      | Dist_poisson lambda ->
          sample_mean meanfn (fun () -> poisson_draw lambda)
      | Dist_support sup ->
          List.fold_left (fun acc (v, w) -> acc +. w *. (meanfn v)) 0. sup
      | Dist_mixture l ->
          List.fold_left (fun acc (d, w) -> acc +. w *. mean meanfn d) 0. l
      | Dist_pair (_, _) ->
          assert false (* XXX TODO XXX *)
      | Dist_list _ ->
          assert false (* XXX TODO XXX *)
      | Dist_array _ ->
          assert false (* XXX TODO XXX *)
      (* Array.fold_left (fun acc d -> acc +. mean meanfn d) 0. a *)
      | Dist_add (d1, d2) ->
          let m1= mean meanfn d1 in
          let m2 = mean meanfn d2 in
          m1 +. m2
      | Dist_mult (d1, d2) ->
          let m1 = mean meanfn d1 in
          let m2 = mean meanfn d2 in
          m1 *. m2
      | Dist_app (_, _) as d ->
          mean meanfn (Dist_sampler ((fun () -> draw d), (fun _ -> assert false)))
      | Dist_joint j -> mean_joint meanfn j
    end
  and mean_joint : type a. (a -> float) -> a joint_distr -> float =
    fun meanfn dist ->
    begin match dist with
    | JDist_const x -> meanfn x
    | JDist_rvar (RV x) -> mean meanfn (DS_graph.get_distr x)
    | JDist_add (d1, d2) ->
        let m1 = mean_joint meanfn d1 in
        let m2 = mean_joint meanfn d2 in
        m1 +. m2
    | JDist_mult (d1, d2) ->
        let m1 = mean_joint meanfn d1 in
        let m2 = mean_joint meanfn d2 in
        m1 *. m2
    | JDist_pair (_, _) -> assert false (* XXX TODO XXX *)
    | JDist_list _ -> assert false (* XXX TODO XXX *)
    | JDist_array _ -> assert false (* XXX TODO XXX *)
    | JDist_matrix _ -> assert false (* XXX TODO XXX *)
    | JDist_app _ -> assert false (* XXX TODO XXX *)
    | JDist_ite _ -> assert false (* XXX TODO XXX *)
    | JDist_vec_get _ -> assert false (* XXX TODO XXX *)
    | JDist_mat_add _ -> assert false (* XXX TODO XXX *)
    | JDist_mat_scalar_mul _ -> assert false (* XXX TODO XXX *)
    | JDist_mat_dot _ -> assert false (* XXX TODO XXX *)
    end


  let mean_list (type a) : (a -> float) -> a list t -> float list =
    begin fun meanfn d ->
      let ls = split_list d in
      List.map (fun l -> mean meanfn l) ls
    end

  (** [mean_int d] computes the mean of a [int Distribution.t]. *)
  let rec mean_int (d: int t) =
    begin match d with
    | Dist_sampler (draw, _) ->
        let n = 100000 in
        let acc = ref 0 in
        for _ = 1 to n do
          acc := !acc + draw ();
        done;
        float !acc /. float n
    | Dist_support sup ->
        List.fold_left (fun acc (v, w) -> acc +. float v *. w) 0. sup
    | Dist_mixture l ->
        List.fold_left (fun acc (d, w) -> acc +. w *. mean_int d) 0. l
    | Dist_app (_, _) ->
        mean_int (Dist_sampler ((fun () -> draw d), (fun _ -> assert false)))
    | Dist_uniform_int (a, b) -> uniform_int_mean a b
    | Dist_poisson a ->
        poisson_mean a
    | Dist_mv_gaussian (_, _, _) -> assert false
    | Dist_joint j -> mean_int_joint j
    end
  and mean_int_joint (d: int joint_distr) =
    begin match d with
    | JDist_const x -> float x
    | JDist_rvar (RV x) -> mean_int (DS_graph.get_distr x)
    | JDist_app _ -> assert false (* XXX TODO XXX *)
    | JDist_ite _ -> assert false (* XXX TODO XXX *)
    | JDist_mat_add _
    | JDist_mat_scalar_mul _
    | JDist_mat_dot _ -> assert false
    end

  (** [mean_bool d] computes the mean of a [bool Distribution.t]. *)
  let rec mean_bool (d: bool t) =
    begin match d with
    | Dist_sampler (draw, _) ->
        let n = 100000 in
        let acc = ref 0 in
        for _ = 1 to n do
          if draw () then acc := !acc + 1 done;
        float !acc /. float n
    | Dist_bernoulli p -> bernoulli_mean p
    | Dist_support sup ->
        List.fold_left (fun acc (v, w) -> if v then acc +. w else acc) 0. sup
    | Dist_mixture l ->
        List.fold_left (fun acc (d, w) -> acc +. w *. mean_bool d) 0. l
    | Dist_app (_, _) ->
        mean_bool (Dist_sampler ((fun () -> draw d), (fun _ -> assert false)))
    | Dist_mv_gaussian (_, _, _) -> assert false
    | Dist_joint j -> mean_bool_joint j
    end
  and mean_bool_joint (d: bool joint_distr) =
    begin match d with
    | JDist_const x -> if x then 1. else 0.
    | JDist_rvar (RV x) -> mean_bool (DS_graph.get_distr x)
    | JDist_app _ -> assert false (* XXX TODO XXX *)
    | JDist_ite _ -> assert false (* XXX TODO XXX *)
    | JDist_mat_add _
    | JDist_mat_scalar_mul _
    | JDist_mat_dot _ -> assert false
    end

  (** [mean_signal_present d] computes the mean of the presence of ['a
      signal Distribution.t]. *)
  let rec mean_signal_present : type a. (a * bool) t -> float =
    fun d ->
    begin match d with
    | Dist_sampler (draw, _) ->
        let n = 100000 in
        let acc = ref 0 in
        for _ = 1 to n do
          if snd (draw ()) then acc := !acc + 1 done;
        float !acc /. float n
    | Dist_support sup ->
        List.fold_left (fun acc ((_, b), w) -> if b then acc +. w else acc) 0. sup
    | Dist_mixture l ->
        List.fold_left (fun acc (d, w) -> acc +. w *. mean_signal_present d) 0. l
    | Dist_pair _ -> assert false
    | Dist_app (_, _) ->
        mean_signal_present
          (Dist_sampler ((fun () -> draw d), (fun _ -> assert false)))
    | Dist_mv_gaussian (_, _, _) -> assert false
    | Dist_joint j -> mean_signal_present_joint j
    end
  and mean_signal_present_joint : type a. (a * bool) joint_distr -> float =
    fun d ->
    begin match d with
    | JDist_const (_, true) -> 1.
    | JDist_const (_, false) -> 0.
    | JDist_rvar (RV x) -> mean_signal_present (DS_graph.get_distr x)
    | JDist_app _ -> assert false (* XXX TODO XXX *)
    | JDist_ite _ -> assert false (* XXX TODO XXX *)
    | JDist_pair _
    | JDist_mat_add _
    | JDist_mat_scalar_mul _
    | JDist_mat_dot _ -> assert false
    end

  let rec mean_matrix (d: Mat.mat t) : Owl.Mat.mat =
    begin match d with
    | Dist_sampler (draw, _) ->
        let n = 100000 in
        let acc = ref (draw ()) in
        for _ = 1 to n - 1 do
          acc := Owl.Mat.(!acc @= draw ())
        done;
        Owl.Mat.mean ~axis:0 !acc
    | Dist_support ((v, w)::sup) ->
        List.fold_left
          (fun acc (v, w) -> Owl.Mat.(acc + (v *$ w)))
          Owl.Mat.(v *$ w) sup
    | Dist_support [] -> assert false
    | Dist_mixture ((v, w)::sup) ->
        List.fold_left
          (fun acc (v, w) -> Owl.Mat.(acc + (mean_matrix v *$ w)))
          Owl.Mat.(mean_matrix v *$ w) sup
    | Dist_mixture [] -> assert false
    | Dist_mv_gaussian (mu, _, _) -> mu
    | Dist_sampler_float _ -> assert false
    | Dist_pair _ -> assert false
    | Dist_list _ -> assert false
    | Dist_array _ -> assert false
    | Dist_gaussian _ -> assert false
    | Dist_lognormal _ -> assert false
    | Dist_beta _ -> assert false
    | Dist_bernoulli _ -> assert false
    | Dist_uniform_int _ -> assert false
    | Dist_uniform_float _ -> assert false
    | Dist_exponential _ -> assert false
    | Dist_poisson _ -> assert false
    | Dist_add _ -> assert false
    | Dist_mult _ -> assert false
    | Dist_app _ -> assert false
    | Dist_joint j -> mean_matrix_joint j
    end
  and mean_matrix_joint (d: Mat.mat joint_distr) =
    begin match d with
    | JDist_const x -> x
    | JDist_rvar (RV x) -> mean_matrix (DS_graph.get_distr x)
    | JDist_app _ -> assert false (* XXX TODO XXX *)
    | JDist_ite _ -> assert false (* XXX TODO XXX *)
    | JDist_vec_get _ -> assert false (* XXX TODO XXX *)
    | JDist_matrix _ -> assert false (* XXX TODO XXX *)
    | JDist_mat_add _ -> assert false (* XXX TODO XXX *)
    | JDist_mat_scalar_mul _ -> assert false (* XXX TODO XXX *)
    | JDist_mat_dot _ -> assert false (* XXX TODO XXX *)
    | JDist_add _ -> assert false
    | JDist_mult _ -> assert false
    | JDist_pair _ -> assert false
    | JDist_array _ -> assert false
    | JDist_list _ -> assert false
    end

  let rec map: type a b. (a -> b) -> a t -> b t =
    fun f dist ->
    let to_sampler d =
      Dist_sampler ((fun () -> f (draw d)),
                    (fun _ -> assert false))
    in
    begin match dist with
    | Dist_sampler (_, _) -> to_sampler dist
    | Dist_sampler_float (_, _, _) -> to_sampler dist
    | Dist_support l -> Dist_support (List. map (fun (x, p) -> (f x, p)) l)
    | Dist_mixture l ->
        Dist_mixture (List.map (fun (d, p) -> (map f d, p)) l)
    | Dist_pair(_, _) -> to_sampler dist
    | Dist_list _ -> to_sampler dist
    | Dist_array _ -> to_sampler dist
    | Dist_gaussian (_, _) -> to_sampler dist
    | Dist_lognormal (_, _) -> to_sampler dist
    | Dist_mv_gaussian (_, _, _) -> to_sampler dist
    | Dist_beta (_, _) -> to_sampler dist
    | Dist_bernoulli _ -> to_sampler dist
    | Dist_uniform_int _ -> to_sampler dist
    | Dist_uniform_float (_, _) -> to_sampler dist
    | Dist_exponential _ -> to_sampler dist
    | Dist_poisson _ -> to_sampler dist
    | Dist_add _ -> to_sampler dist
    | Dist_mult _ -> to_sampler dist
    | Dist_app (_, _) -> to_sampler dist
    | Dist_joint j -> Dist_joint (map_joint f j)
    end
  and map_joint : type a b. (a -> b) -> a joint_distr -> b joint_distr  =
    fun f dist ->
    let to_app d = JDist_app (JDist_const f, d) in
    begin match dist with
    | JDist_const x -> JDist_const (f x)
    | JDist_rvar _ -> to_app dist
    | JDist_app _ -> to_app dist
    | JDist_ite (b, d1, d2) ->
        JDist_ite (b, map_joint f d1, map_joint f d2)
    | JDist_vec_get _ -> to_app dist
    | JDist_matrix _ -> to_app dist
    | JDist_mat_add _ -> to_app dist
    | JDist_mat_scalar_mul _ -> to_app dist
    | JDist_mat_dot _ -> to_app dist
    | JDist_add _ -> to_app dist
    | JDist_mult _ -> to_app dist
    | JDist_pair _ -> to_app dist
    | JDist_array _ -> to_app dist
    | JDist_list _ -> assert false
    end

end

include Distribution_rec
