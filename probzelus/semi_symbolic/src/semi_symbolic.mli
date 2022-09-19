open Owl

module Semi_symbolic_impl = Semi_symbolic_impl
module Distr_operations = Distr_operations

type 'a expr;;
type 'a distribution;;
type 'a random_var;;

val const : 'a -> 'a expr;;
val add : float expr -> float expr -> float expr;;
val mul : float expr -> float expr -> float expr;;
val eq : 'a expr -> 'a expr -> bool expr;;
val pair : 'a expr -> 'b expr -> ('a * 'b) expr;;
val array : 'a expr array -> 'a array expr;;
val matrix : 'a expr array array -> 'a array array expr
val ite : bool expr -> 'a expr -> 'a expr -> 'a expr;;
val lst : 'a expr list -> 'a list expr;;

val mat_add : Mat.mat expr -> Mat.mat expr -> Mat.mat expr;;
val mat_scalar_mult : float expr -> Mat.mat expr -> Mat.mat expr;;
val mat_dot : Mat.mat expr -> Mat.mat expr -> Mat.mat expr;;
val vec_get : Mat.mat expr -> int -> float expr;;


val gaussian : float expr -> float expr -> float distribution;;
val beta : float expr -> float expr -> float distribution;;
val bernoulli : float expr -> bool distribution;;
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

module type Conversion_fn = sig
  type 'a t

  val const : 'a -> 'a t
  val add : float t -> float t -> float t
  val mul : float t -> float t -> float t
  val eq : 'a t -> 'a t -> bool t
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
  val mv_gaussian : Mat.mat -> Mat.mat -> Mat.mat t
  val delta : 'a -> 'a t
  val sampler : (unit -> 'a) -> ('a -> float) -> 'a t
  val categorical : lower:int -> upper:int -> (int -> float) -> int t
end

module Convert(Fn : Conversion_fn) : sig
  val convert : 'a expr -> 'a Fn.t
end

