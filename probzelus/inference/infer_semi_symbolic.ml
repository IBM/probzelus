open Ztypes

exception EvalExn of string

type 'a expr = 'a Semi_symbolic.expr

let const = Semi_symbolic.const
let add = Semi_symbolic.add
let ( +~ ) = add
let mult (a, b) = Semi_symbolic.mul a b
let ( *~ ) = mult
let pair (a, b) = Semi_symbolic.pair a b
let array = Semi_symbolic.array
let matrix = Semi_symbolic.matrix
let ite = Semi_symbolic.ite

let eval = Semi_symbolic.eval_sample

type 'a ds_distribution = 'a Semi_symbolic.distribution

let gaussian (mu, var) = Semi_symbolic.gaussian mu (Semi_symbolic.const var)
let beta (a, b) =
  Semi_symbolic.beta (Semi_symbolic.const a) (Semi_symbolic.const b)
let bernoulli p = Semi_symbolic.bernoulli p

type pstate = Infer_pf.pstate

let factor' = Infer_pf.factor'

let factor = Infer_pf.factor

let sample' (_pstate, dist) =
  Semi_symbolic.sample "" dist

let sample =
  let alloc () = () in
  let reset _state = () in
  let copy _src _dst = () in
  let step _state input =
    sample' input
  in
  Cnode { alloc; reset; copy; step; }

let observe' (_pstate, (dist, v)) =
  let score = Semi_symbolic.observe 0. dist v in
  factor' (_pstate, score)

let observe =
  let alloc () = () in
  let reset _state = () in
  let copy _src _dst = () in
  let step _state input =
    observe' input
  in
  Cnode { alloc; reset; copy; step; }

exception NonMarginal: 'a Semi_symbolic.random_var -> exn 

module Convert_fn_distr : Semi_symbolic.Conversion_fn with type 'a t = 'a Types.mdistr = struct
  open Types
  type 'a t = 'a mdistr

  let const v = Dist_support [v, 1.]
  let add d1 d2 = Dist_add(d1, d2)
  let mul d1 d2 = Dist_mult(d1, d2)
  let eq _ _ = assert false (* TODO: what to do here? *)
  let pair d1 d2 = Dist_pair(d1, d2)
  let array d = Dist_array d
  let matrix _ = assert false (* TODO: what to do here? *)
  let ite _ _ _ = assert false (* TODO: what to do here? *)

  let gaussian mu var = Dist_gaussian (mu, var)
  let beta a b = Dist_beta(a, b)
  let bernoulli p = Dist_bernoulli p
  let delta x = Distribution.dirac x
end

module Convert_distr = Semi_symbolic.Convert(Convert_fn_distr)

let infer_marginal n (Cnode { alloc; reset; copy = _; step;}) =
  let alloc () = ref (alloc ()) in
  let reset state = reset !state in
  (* TODO: this fails if the node is not marginalizable.
   * Is that expected behavior? *)
  let step state (prob, x) =
    Convert_distr.convert (step !state (prob, x))
  in
  let copy src dst = dst := Probzelus_utils.copy !src in
  let Cnode {alloc = infer_alloc; 
             reset = infer_reset;
             copy = infer_copy; 
             step = infer_step} =
    Infer_pf.infer n (Cnode {alloc; reset; copy = copy; step; })
  in
  let infer_step state i = Distribution.to_mixture (infer_step state i) in
  Cnode {alloc = infer_alloc; reset = infer_reset; copy = infer_copy; step = infer_step;}
