open Owl

module Semi_symbolic_impl = Semi_symbolic_impl
module Distr_operations = Distr_operations

type 'a expr;;
type 'a distribution;;
type 'a random_var;;

val const : 'a -> 'a expr;;
val get_const : 'a expr -> 'a;;
val add : float expr -> float expr -> float expr;;
val mul : float expr -> float expr -> float expr;;
val div : float expr -> float expr -> float expr;;
val exp : float expr -> float expr;;
val eq : 'a expr -> 'a expr -> bool expr;;
val lt : 'a expr -> 'a expr -> bool expr;;
val pair : 'a expr -> 'b expr -> ('a * 'b) expr;;
val split : ('a * 'b) expr -> 'a expr * 'b expr;;
val array : 'a expr array -> 'a array expr;;
val get_array : 'a array expr -> 'a expr array;;
val matrix : 'a expr array array -> 'a array array expr
val ite : bool expr -> 'a expr -> 'a expr -> 'a expr;;
val lst : 'a expr list -> 'a list expr;;
val get_lst : 'a list expr -> 'a expr list;;

val mat_add : Mat.mat expr -> Mat.mat expr -> Mat.mat expr;;
val mat_scalar_mult : float expr -> Mat.mat expr -> Mat.mat expr;;
val mat_dot : Mat.mat expr -> Mat.mat expr -> Mat.mat expr;;
val vec_get : Mat.mat expr -> int -> float expr;;
val int_to_float : int expr -> float expr;;

val gaussian : float expr -> float expr -> float distribution;;
val beta : float expr -> float expr -> float distribution;;
val bernoulli : float expr -> bool distribution;;
val binomial : int expr -> float expr -> int distribution;;
val beta_binomial : int expr -> float expr -> float expr -> int distribution;;
val negative_binomial : int expr -> float expr -> int distribution;;
val exponential : float expr -> float distribution;;
val gamma : float expr -> float expr -> float distribution;;
val poisson : float expr -> int distribution;;
val mv_gaussian : Mat.mat expr -> Mat.mat expr -> Mat.mat distribution;;
val sampler : (unit -> 'a) -> ('a -> float) -> 'a distribution;;
val categorical : lower:int -> upper:int -> (int -> float) -> int distribution;;

val sample : string -> 'a distribution -> 'a expr;;

val score : 'a random_var -> 'a -> float;;
val draw : 'a random_var -> unit -> 'a;;
val intervene : 'a random_var -> 'a -> unit;;
val make_marginal : 'a random_var -> unit;;

val value : 'a random_var -> 'a;;
val observe : float -> 'a distribution -> 'a -> float;;

val eval_sample : 'a expr -> 'a;;

val pp_approx_status : bool -> string;;

val get_marginal_expr : 'a expr -> 'a expr;;
val pp_distribution : 'a distribution -> string;;
val mean_float : float expr -> float expr;;
val mean_int : int expr -> float expr;;
val mean_bool : bool expr -> float expr;;

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
  val mv_gaussian : Mat.mat -> Mat.mat -> Mat.mat t
  val delta : 'a -> 'a t
  val sampler : (unit -> 'a) -> ('a -> float) -> 'a t
  val categorical : lower:int -> upper:int -> (int -> float) -> int t
end

module Convert(Fn : Conversion_fn) : sig
  val convert : 'a expr -> 'a Fn.t
end

