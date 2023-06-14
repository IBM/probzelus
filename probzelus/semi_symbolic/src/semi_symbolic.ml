open Owl

module Semi_symbolic_impl = Semi_symbolic_impl
module Distr_operations = Distr_operations

type 'a expr = 'a Semi_symbolic_impl.expr
type 'a distribution = 'a Semi_symbolic_impl.distribution
type 'a random_var = 'a Semi_symbolic_impl.random_var

let const = Semi_symbolic_impl.const
let add = Semi_symbolic_impl.add
let mul = Semi_symbolic_impl.mul
let div = Semi_symbolic_impl.div
let exp = Semi_symbolic_impl.exp
let eq = Semi_symbolic_impl.eq
let lt = Semi_symbolic_impl.lt
let pair = Semi_symbolic_impl.pair
let array = Semi_symbolic_impl.array
let matrix = Semi_symbolic_impl.matrix
let ite = Semi_symbolic_impl.ite
let lst = Semi_symbolic_impl.lst

let int_add = Semi_symbolic_impl.int_add

let mat_add = Semi_symbolic_impl.mat_add
let mat_scalar_mult = Semi_symbolic_impl.mat_scalar_mult
let mat_dot = Semi_symbolic_impl.mat_dot
let vec_get = Semi_symbolic_impl.vec_get
let int_to_float = Semi_symbolic_impl.int_to_float

let delta = Semi_symbolic_impl.delta
let gaussian = Semi_symbolic_impl.gaussian
let beta = Semi_symbolic_impl.beta
let bernoulli = Semi_symbolic_impl.bernoulli
let binomial = Semi_symbolic_impl.binomial
let beta_binomial = Semi_symbolic_impl.beta_binomial
let negative_binomial = Semi_symbolic_impl.negative_binomial
let exponential = Semi_symbolic_impl.exponential
let gamma = Semi_symbolic_impl.gamma
let poisson = Semi_symbolic_impl.poisson
let student_t = Semi_symbolic_impl.student_t
let mv_gaussian = Semi_symbolic_impl.mv_gaussian
let mixture = Semi_symbolic_impl.mixture
let sampler = Semi_symbolic_impl.sampler
let categorical = Semi_symbolic_impl.categorical

let sample = Semi_symbolic_impl.sample
let score = Semi_symbolic_impl.score
let draw = Semi_symbolic_impl.draw
let intervene = Semi_symbolic_impl.intervene
let make_marginal = Semi_symbolic_impl.make_marginal

let value = Semi_symbolic_impl.value
let observe = Semi_symbolic_impl.observe

let eval = Semi_symbolic_impl.eval
let eval_sample = Semi_symbolic_impl.eval_sample

let pp_approx_status = Semi_symbolic_impl.pp_approx_status


module type Conversion_fn = sig
  type 'a t

  val const : 'a -> 'a t
  val add : float t -> float t -> float t
  val mul : float t -> float t -> float t
  val div : float t -> float t -> float t
  val exp : float t -> float t
  val eq : 'a t -> 'a t -> bool t
  val lt : 'a t -> 'a t -> bool t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val array : 'a t array -> 'a array t
  val matrix : 'a t array array -> 'a array array t
  val ite : bool t -> 'a t -> 'a t -> 'a t
  val lst : 'a t list -> 'a list t

  val mat_add : Mat.mat t -> Mat.mat t -> Mat.mat t
  val mat_scalar_mult : float t -> Mat.mat t -> Mat.mat t
  val mat_dot : Mat.mat t -> Mat.mat t -> Mat.mat t
  val vec_get : Mat.mat t -> int -> float t
  val mat_trans : Mat.mat t -> Mat.mat t
  val mat_inv : Mat.mat t -> Mat.mat t
  val mat_single : float t -> Mat.mat t

  val gaussian : float -> float -> float t
  val beta : float -> float -> float t
  val bernoulli : float -> bool t
  val binomial : int -> float -> int t
  val beta_binomial : int -> float -> float -> int t
  val negative_binomial : int -> float -> int t
  val exponential : float -> float t
  val gamma : float -> float -> float t
  val poisson : float -> int t
  val student_t : float -> float -> float -> float t
  val mv_gaussian : Mat.mat -> Mat.mat -> Mat.mat t
  val delta : 'a -> 'a t
  val mixture : ('a t * float) list -> 'a t
  val sampler : (unit -> 'a) -> ('a -> float) -> 'a t
  val categorical : lower:int -> upper:int -> (int -> float) -> int t
end

exception NonMarginal of unit

module Convert(Fn : Conversion_fn) = struct
  let rec convert : type a. a expr -> a Fn.t =
  fun e ->
    begin match e with
    | ExConst v -> Fn.const v
    | ExRand rv ->
      Semi_symbolic_impl.hoist_and_eval rv;
      begin match rv.distr with
      | Normal (mu, var) ->
        begin match (Semi_symbolic_impl.eval mu, Semi_symbolic_impl.eval var) with
        | (ExConst mu_v, ExConst var_v) -> Fn.gaussian mu_v var_v
        | _ -> raise (NonMarginal ())
        end
      | Beta (a, b) ->
        begin match (Semi_symbolic_impl.eval a, Semi_symbolic_impl.eval b) with
        | (ExConst a_v, ExConst b_v) -> Fn.beta a_v b_v
        | _ -> raise (NonMarginal ())
        end
      | Bernoulli (p) ->
        begin match (Semi_symbolic_impl.eval p) with
        | (ExConst p_v) -> Fn.bernoulli p_v
        | _ -> raise (NonMarginal ())
        end
      | Binomial (n, p) ->
        begin match (Semi_symbolic_impl.eval n, Semi_symbolic_impl.eval p) with
        | (ExConst n_v, ExConst p_v) -> Fn.binomial n_v p_v
        | _ -> raise (NonMarginal ())
        end
      | NegativeBinomial (n, p) ->
        begin match (Semi_symbolic_impl.eval n, Semi_symbolic_impl.eval p) with
        | (ExConst n_v, ExConst p_v) -> Fn.negative_binomial n_v p_v
        | _ -> raise (NonMarginal ())
        end
      | Gamma (a, b) ->
        begin match (Semi_symbolic_impl.eval a, Semi_symbolic_impl.eval b) with
        | (ExConst a_v, ExConst b_v) -> Fn.gamma a_v b_v
        | _ -> raise (NonMarginal ())
        end
      | Poisson (lambda) ->
        begin match (Semi_symbolic_impl.eval lambda) with
        | (ExConst lambda_v) -> Fn.poisson lambda_v
        | _ -> raise (NonMarginal ())
        end
      | StudentT (mu, tau2, nu) ->
        begin match (Semi_symbolic_impl.eval mu, Semi_symbolic_impl.eval tau2, Semi_symbolic_impl.eval nu) with
        | (ExConst mu_v, ExConst tau2_v, ExConst nu_v) -> Fn.student_t mu_v tau2_v nu_v
        | _ -> raise (NonMarginal ())
        end
      | BetaBinomial (n, a, b) ->
        begin match (Semi_symbolic_impl.eval n, Semi_symbolic_impl.eval a, Semi_symbolic_impl.eval b) with
        | (ExConst n_v, ExConst a_v, ExConst b_v) -> Fn.beta_binomial n_v a_v b_v
        | _ -> raise (NonMarginal ())
        end
      | Delta e ->
          begin match Semi_symbolic_impl.eval e with
          | ExConst v -> Fn.delta v
          | _ -> raise (NonMarginal ())
          end
      | MvNormal (mu, var) ->
        begin match (Semi_symbolic_impl.eval mu, Semi_symbolic_impl.eval var) with
        | (ExConst mu_v, ExConst var_v) -> Fn.mv_gaussian mu_v var_v
        | _ -> raise (NonMarginal ())
        end
      | Mixture l -> Fn.mixture (List.map (fun (e, p) -> (convert e, p)) l)
      | Sampler (f, g) -> Fn.sampler f g
      | Categorical ({ lower; upper }, e) ->
        begin match Semi_symbolic_impl.eval e with
        | ExConst (p, _) -> Fn.categorical ~lower ~upper (fun i -> p.(i - lower))
        | _ -> raise (NonMarginal ())
        end
      end
    | ExAdd (e1, e2) -> Fn.add (convert e1) (convert e2)
    | ExMul (e1, e2) -> Fn.mul (convert e1) (convert e2)
    | ExDiv (_, _) -> raise (NonMarginal ())
    | ExUnop (_, _) -> raise (NonMarginal ())
    | ExIntToFloat _ -> raise (NonMarginal ())
    | ExPair (e1, e2) -> Fn.pair (convert e1) (convert e2)
    | ExArray a -> Fn.array (Array.map convert a)
    | ExMatrix m -> Fn.matrix (Array.map (Array.map convert) m)
    | ExList l -> Fn.lst (List.map convert l)
    | ExCmp (Eq, e1, e2) -> Fn.eq (convert e1) (convert e2)
    | ExCmp (Lt, e1, e2) -> Fn.lt (convert e1) (convert e2)
    | ExIte (i, t, e) -> Fn.ite (convert i) (convert t) (convert e)
    | ExMatAdd (e1, e2) -> Fn.mat_add (convert e1) (convert e2)
    | ExMatSub (e1, e2) -> Fn.mat_add (convert e1) (Fn.mat_scalar_mult (Fn.delta (-1.)) (convert e2))
    | ExMatMul (e1, e2) -> Fn.mat_dot (convert e1) (convert e2)
    | ExMatTrans e -> Fn.mat_trans (convert e)
    | ExMatInv e -> Fn.mat_inv (convert e)
    | ExMatScalarMul (s, e) -> Fn.mat_scalar_mult (convert s) (convert e)
    | ExMatGet (e, i) -> Fn.vec_get (convert e) i
    | ExMatSingle (e) -> Fn.mat_single (convert e)
    | ExVar _ | ExFactor _ | ExSum _ | ExGet _ | ExLet _ | ExIntAdd _ -> failwith "Unimplemented"
    end
end
