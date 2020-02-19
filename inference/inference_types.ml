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
  | Dist_mv_gaussian : Mat.mat * Mat.mat -> Mat.mat distr
  (* | Dist_ds : 'a expr -> 'a distr *)

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
and _ expr_tree =
  | Econst : 'a -> 'a expr_tree
  | Ervar : 'a random_var -> 'a expr_tree
  | Eadd : float expr * float expr -> float expr_tree
  | Emult : float expr * float expr -> float expr_tree
  | Eapp : ('a -> 'b) expr * 'a expr -> 'b expr_tree
  | Epair : 'a expr * 'b expr -> ('a * 'b) expr_tree
  | Earray : 'a expr array -> 'a array expr_tree
  | Ematrix : 'a expr array array -> 'a array array expr_tree
  | Elist : 'a expr list -> 'a list expr_tree
  | Eite : bool expr * 'a expr * 'a expr -> 'a expr_tree
  | Emat_add : Mat.mat expr * Mat.mat expr -> Mat.mat expr_tree
  | Emat_scalar_mul : float expr * Mat.mat expr -> Mat.mat expr_tree
  | Emat_dot : Mat.mat expr * Mat.mat expr -> Mat.mat expr_tree
  | Evec_get : Mat.mat expr * int -> float expr_tree

and 'a expr =
  { mutable value : 'a expr_tree }

and 'a random_var =
  | RV : ('b, 'a) ds_graph_node -> 'a random_var
