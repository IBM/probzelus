open Owl

let pi = 4. *. atan 1.
let two_pi = 2.0 *. pi
let sqrt_two_pi = sqrt two_pi

let log_combination n k =
  let rec comb acc n k =
    if k = 0 then acc
    else comb (acc +. log (float_of_int n) -. log (float_of_int k)) (n - 1) (k - 1)
  in
  comb 0. n k

let log_gamma x =
  Owl_maths.loggamma x

let gaussian_draw =
  let rec rand_pair () =
    let u1 = Random.float 1.0 in
    let u2 = Random.float 1.0 in
    if u1 < epsilon_float then rand_pair ()
    else u1, u2
  in
  fun mu sigma2 ->
    let u1, u2 = rand_pair() in
    let z = sqrt (-.2. *. log u1) *. cos (2. *. pi *. u2) in
    z *. sqrt sigma2 +. mu

let gaussian_score mu sigma2 x =
  -. 0.5 *. log (2. *. pi *. sigma2) -.
  (x -. mu) ** 2. /. (2. *. sigma2)

let mv_gaussian_draw' mu _sigma a =
  let n = (Arr.shape mu).(0) in
  let xs = Arr.init [| n; 1 |] (fun _ -> gaussian_draw 0. 1.) in
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
  let aux = Mat.empty k n in
  Mat.dot_ ~transa:true ~c:aux x_m sig_inv;
  -. 0.5 *. log ((two_pi ** float d) *. sig_det)
  -. 0.5 *. Mat.(get (aux *@ x_m) 0 0)

let mv_gaussian_score mu sigma x =
  let sig_inv = Linalg.D.inv sigma in
  let sig_det = Linalg.D.det sigma in
  mv_gaussian_score' mu sigma sig_inv sig_det x

let bernoulli_draw p =
  Random.float 1.0 < p

let bernoulli_score p v =
  if v then log p else log (1. -. p)

let bernoulli_mean p = p

let bernoulli_variance p = p *. (1. -. p)

let binomial_draw n p =
  let rec run_trials n n_success =
    if n = 0 then n_success
    else begin
      if Random.float 1.0 < p then
        run_trials (n - 1) (n_success + 1)
      else
        run_trials (n - 1) n_success
    end
  in
  run_trials n 0

let binomial_score n p k =
  log_combination n k +.
    float_of_int k *. (log p) +. float_of_int (n - k) *. log (1. -. p)

let binomial_mean n p =
  float_of_int n *. p

let binomial_variance n p =
  float_of_int n *. p *. (1. -. p)

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

let log_beta a b =
  log_gamma a +. log_gamma b -. log_gamma (a +. b)

let beta_score =
  fun a b x ->
    if x > 0. && x < 1. then
      (a -. 1.) *. log x +. (b -. 1.) *. log (1. -. x) -. log_beta a b
    else
      neg_infinity

let beta_mean a b =
  a /. (a +. b)

let beta_variance a b =
  a *. b /. ((a +. b) *. (a +. b) *. (a +. b +. 1.))

(* Beta-Binomial distribution is a compound distribution where
   the p parameter of the binomial distribution is drawn from a beta *)
let beta_binomial_draw n a b =
  let p = beta_draw a b in
  binomial_draw n p

let beta_binomial_score n a b k =
  log_combination n k +. 
    log_beta (float_of_int k +. a) (float_of_int (n - k) +. b) -. 
    log_beta a b

let beta_binomial_mean n a b =
  float_of_int n *. a /. (a +. b)

let beta_binomial_variance n a b =
  let n = float_of_int n in
  n *. a *. b *. (a +. b +. n) /. ((a +. b) ** 2. *. (a +. b +. 1.))

let negative_binomial_draw n p =
  let rec run_trials n_success n_failure =
    if n_success = 0 then n_failure
    else begin
      if Random.float 1.0 < p then
        run_trials (n_success - 1) n_failure
      else
        run_trials n_success (n_failure + 1)
    end
  in
  run_trials n 0

let negative_binomial_score n p k =
  log_combination (n + k - 1) k +.
    float_of_int k *. log (1. -. p) +. float_of_int n *. log p

let negative_binomial_mean n p =
  float_of_int n *. (1. -. p) /. p

let negative_binomial_variance n p =
  float_of_int n *. (1. -. p) /. (p *. p)  

let categorical_draw sup =
  let sample = Random.float 1.0 in
  let rec draw sum r =
    begin match r with
    | [] -> failwith "Empty support"
    | (v, p) :: r ->
        let sum = sum +. p in
        if sample <= sum then v else draw sum r
    end
  in
  draw 0. sup

let categorical_score sup x =
  let p =
    List.fold_left
      (fun acc (y, w) -> if x = y then acc +. w else acc)
      0. sup
  in
  log p

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

let rec gamma_draw a b =
  if a < 1. then
    let u = Random.float 1. in
    gamma_draw (1. +. a) b *. (u ** (1. /. a))
  else
    let d = a -. 1. /. 3. in
    let c = 1. /. sqrt (9. *. d) in
    let rec loop () =
      let x = gaussian_draw 0. 1. in
      let v = 1. +. c *. x in
      let v = v *. v *. v in
      if v <= 0. then loop ()
      else
        let u = Random.float 1. in
        if log (u) < 0.5 *. x *. x +. d -. d *. v +. d *. log v then
          d *. v /. b
        else
          loop ()
    in
    loop ()

let gamma_score a b x =
  if x >= 0. then
    (a -. 1.) *. log x -. b *. x +. a *. log b -. log_gamma a
  else
    neg_infinity

let gamma_mean a b =
  a /. b

let gamma_variance a b =
  a /. (b *. b)

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

let student_t_draw mu tau2 nu =
  (* TODO:  *)
  Owl_stats.t_rvs ~df:nu ~loc:mu ~scale:(sqrt tau2)

let student_t_score mu tau2 nu x =
  assert (nu > 1.);
  Owl_stats.t_logpdf ~df:nu ~loc:mu ~scale:(sqrt tau2) x

let student_t_mean mu _ nu =
  assert (nu > 1.);
  mu

let student_t_variance _ tau2 nu =
  assert (nu < 1.);
  if nu <= 2. then
    infinity
  else
    tau2 *. nu /. (nu -. 2.)