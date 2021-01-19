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


(** Probabilities (must be in the interval [0, 1]). *)
type proba = float

(** Logarithm of probabilities *)
type log_proba = float

(** Family of marginal distributions (used as kind) *)
type kdistr =
  | KGaussian
  | KMVGaussian
  | KBeta
  | KBernoulli
  | KValue
  | KOthers

(** Type of distributions *)
type _ distr =
  | Dist_sampler : ((unit -> 'a) * ('a -> log_proba)) -> 'a distr
  | Dist_sampler_float :
      ((unit -> float) * (float -> log_proba) * (unit -> float * float)) -> float distr
  | Dist_support : ('a * proba) list -> 'a distr
  (* | Dist_mixture of ('a distr) distr *)
  | Dist_mixture : ('a distr * proba) list -> 'a distr
  | Dist_pair : 'a distr * 'b distr -> ('a * 'b) distr
  | Dist_list : 'a distr list -> 'a list distr
  | Dist_array : 'a distr array -> 'a array distr
  | Dist_gaussian : float * float -> float distr
  | Dist_beta : float * float -> float distr
  | Dist_bernoulli : float -> bool distr
  | Dist_uniform_int : int * int -> int distr
  | Dist_uniform_float : float * float -> float distr
  | Dist_exponential : float -> float distr
  | Dist_poisson : float -> int distr
  | Dist_add : float distr * float distr -> float distr
  | Dist_mult : float distr * float distr -> float distr
  | Dist_app : ('a -> 'b) distr * 'a distr -> 'b distr
  (* Dist_mv_gaussian (mu, sigma, 1/sigma, det(sigma), svd(sigma)) *)
  | Dist_mv_gaussian :
      (Mat.mat * Mat.mat * mv_gaussian_ext option) -> Mat.mat distr

  | Dist_joint : 'a joint_distr -> 'a distr

and mv_gaussian_ext =
  { mvg_inv_sigma : Mat.mat; (* 1/sigma *)
    mvg_det_sigma : float; (* det(sigma) *)
    mvg_draw_cache : Mat.mat; (* let u,s,_ = svd(sigma) in u *@ sqrt(diagm s) *) }

and _ joint_distr =
  | JDist_const : 'a -> 'a joint_distr
  | JDist_rvar : 'a random_var -> 'a joint_distr
  | JDist_add : float joint_distr * float joint_distr -> float joint_distr
  | JDist_mult : float joint_distr * float joint_distr -> float joint_distr
  | JDist_app : ('a -> 'b) joint_distr * 'a joint_distr -> 'b joint_distr
  | JDist_pair : 'a joint_distr * 'b joint_distr -> ('a * 'b) joint_distr
  | JDist_array : 'a joint_distr array -> 'a array joint_distr
  | JDist_matrix : 'a joint_distr array array -> 'a array array joint_distr
  | JDist_list : 'a joint_distr list -> 'a list joint_distr
  | JDist_ite :
      bool joint_distr * 'a joint_distr * 'a joint_distr -> 'a joint_distr
  | JDist_mat_add :
      Mat.mat joint_distr * Mat.mat joint_distr -> Mat.mat joint_distr
  | JDist_mat_scalar_mul :
      float joint_distr * Mat.mat joint_distr -> Mat.mat joint_distr
  | JDist_mat_dot :
      Mat.mat joint_distr * Mat.mat joint_distr -> Mat.mat joint_distr
  | JDist_vec_get :
      Mat.mat joint_distr * int -> float joint_distr

(** Marginalized distribution *)
and 'a mdistr = 'a distr

(** Conditionned distribution *)
and ('m1, 'm2) cdistr =
  | AffineMeanGaussian: float * float * float -> (float, float) cdistr
  | AffineMeanGaussianMV :
      Mat.mat * Mat.mat * Mat.mat -> (Mat.mat, Mat.mat) cdistr
  | CBernoulli : (float, bool) cdistr
  | CBernBern : (bool -> float) -> (bool, bool) cdistr


(** Delayed sampling *)

(** Random variable of type ['b] and with parent of type ['a] *)
and ('p, 'a) ds_graph_node =
  { ds_graph_node_id : int;
    mutable ds_graph_node_state : ('p, 'a) ds_graph_state; }

and ('p, 'a) ds_graph_state =
  | DSgraph_Initialized:
      ('z, 'p) ds_graph_node * ('p, 'a) cdistr
      -> ('p, 'a) ds_graph_state
  | DSgraph_Marginalized:
      'a mdistr * (('a, 'z) ds_graph_node * ('a, 'z) cdistr) option
      -> ('p, 'a) ds_graph_state
  | DSgraph_Realized of 'a


(** Delayed sampling expressions *)

and 'a random_var =
  | RV : ('b, 'a) ds_graph_node -> 'a random_var


module type DS_GRAPH = sig

  val value : ('a, 'b) ds_graph_node -> 'b
  val get_distr_kind : ('a, 'b) ds_graph_node -> kdistr
  val get_distr : ('a, 'b) ds_graph_node -> 'b distr

  val assume_constant : 'a mdistr -> ('p, 'a) ds_graph_node
  val assume_conditional :
    ('a, 'b) ds_graph_node -> ('b, 'c) cdistr -> ('b, 'c) ds_graph_node

  val force_condition : ('a, 'b) ds_graph_node -> unit
  val realize : 'b -> ('a, 'b) ds_graph_node -> unit
  val graft : ('a, 'b) ds_graph_node -> unit

  val shape : ('a, Mat.mat) ds_graph_node -> int
  val is_realized : ('p, 'a) ds_graph_node -> bool
  val copy_node :
    (int, Obj.t) Hashtbl.t -> ('a, 'b) ds_graph_node -> ('a, 'b) ds_graph_node

end

module type DISTRIBUTION = sig
  module DS_graph: DS_GRAPH

  type 'a t = 'a distr

  val pp_print_any : Format.formatter -> 'a -> unit
  val pp_print_array :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a array -> unit
  val pp_print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val print_any_t : 'a t -> unit
  val print_float_t : float t -> unit
  val print_int_t : int t -> unit
  val print_bool_t : bool t -> unit
  val print_t : ('a -> string) -> 'a t -> unit

  val sampler : (unit -> 'a) * ('a -> float) -> 'a t
  val gamma : float -> float
  val log_gamma : float -> float
  val dirac : 'a -> 'a t
  val bernoulli_draw : float -> bool
  val bernoulli_score : float -> bool -> float
  val bernoulli_mean : 'a -> 'a
  val bernoulli_variance : float -> float
  val bernoulli : float -> bool t
  val gaussian_draw : float -> float -> float
  val gaussian_score : float -> float -> float -> float
  val gaussian_mean : 'a -> 'b -> 'a
  val gaussian_variance : 'a -> 'b -> 'b
  val gaussian : float * float -> float t
  val mv_gaussian_draw :
    Owl.Arr.arr ->
    (float, Bigarray.float64_elt) Owl.Linalg.Generic.t ->
    (float, Bigarray.float64_elt) Owl_dense_matrix_generic.t
  val mv_gaussian_score :
    (float, Bigarray.float64_elt) Owl_dense_matrix_generic.t ->
    Owl.Linalg.D.mat -> Owl.Arr.arr -> float
  val mv_gaussian : Owl.Mat.mat * Owl.Mat.mat -> Owl.Mat.mat t
  val mv_gaussian_ext : Owl.Mat.mat -> mv_gaussian_ext
  val mv_gaussian_curried : Mat.mat -> Mat.mat -> Mat.mat t
  val beta_draw : float -> float -> float
  val beta_score : float -> float -> float -> float
  val beta_mean : float -> float -> float
  val beta_variance : float -> float -> float
  val beta : float * float -> float t
  val sph_gaussian : float list * float list -> float list t
  val uniform_int_draw : int -> int -> int
  val uniform_int_score : int -> int -> int -> float
  val uniform_int_mean : int -> int -> float
  val uniform_int_variance : int -> int -> float
  val uniform_int : int * int -> int t
  val uniform_float_draw : float -> float -> float
  val uniform_float_score : float -> float -> float -> float
  val uniform_float_mean : float -> float -> float
  val uniform_float_variance : float -> float -> float
  val uniform_float : float * float -> float t
  val uniform_list : 'a list -> 'a t
  val weighted_list : (float * 'a) list -> 'a t
  val shuffle : 'a list -> 'a list t
  val exponential_draw : float -> float
  val exponential_score : float -> float -> float
  val exponential_mean : float -> float
  val exponential_variance : float -> float
  val exponential : float -> float t
  val poisson_draw : float -> int
  val poisson_score : float -> int -> float
  val poisson_mean : 'a -> 'a
  val poisson_variance : 'a -> 'a
  val poisson : float -> int t
  val alias_method_unsafe : 'a array -> float array -> 'a t
  val alias_method_list : ('a * float) list -> 'a t
  val alias_method : 'a array -> float array -> 'a t
  val add : float t * float t -> float t
  val mult : float t * float t -> float t
  val app : ('a -> 'b) t * 'a t -> 'b t
  val to_dist_support : 'a t -> 'a t
  val draw : 'a t -> 'a
  val score : 'a t * 'a -> log_proba
  val draw_and_score : 'a t -> 'a * log_proba
  val of_sampler : (unit -> 'a) * ('a -> log_proba) -> 'a t
  val of_list : 'a t list -> 'a list t
  val of_array : 'a t array -> 'a array t
  val of_pair : 'a t * 'b t -> ('a * 'b) t
  val split : ('a * 'b) t -> 'a t * 'b t
  val split_array : 'a array t -> 'a t array
  val split_matrix : 'a array array t -> 'a t array array
  val split_list : 'a list t -> 'a t list
  val to_mixture : 'a t t -> 'a t
  val to_signal : ('a * bool) t -> 'a t * bool
  val stats_float : float t -> float * float
  val mean_float : float t -> float
  val stats_float_list : float list t -> (float * float) list
  val mean_float_list : float list t -> float list
  val mean : ('a -> float) -> 'a t -> float
  val mean_list : ('a -> float) -> 'a list t -> float list
  val mean_int : int t -> float
  val mean_bool : bool t -> float
  val mean_signal_present : ('a * bool) t -> float
  val mean_matrix : Owl.Mat.mat t -> Owl.Mat.mat
  val map : ('a -> 'b) -> 'a t -> 'b t
end
