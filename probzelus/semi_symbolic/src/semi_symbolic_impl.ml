open Owl

(* Types *)

type cmp_op =
  | Eq
  | Lt
(*  | Neq
  | Gt
  | Le
  | Ge*)

type unary_op =
  | Squared
  | SquareRoot
  | Exp

type category = int
type domain = { lower: category; upper: category }
type 'a factor = 'a array * domain
type [@unboxed] 'a var = Var of int

type 'a expr =
  | ExConst : 'a -> 'a expr
  | ExRand : 'a random_var -> 'a expr
  | ExVar : 'a var -> 'a expr
  | ExFactor : category var * domain * 'a expr -> 'a factor expr
  | ExGet : 'a factor expr * category expr -> 'a expr
  | ExLet : 'a var * 'a expr * 'b expr -> 'b expr
  | ExSum : category var * domain * float expr -> float expr
  | ExAdd : float expr * float expr -> float expr
  | ExMul : float expr * float expr -> float expr
  | ExDiv : float expr * float expr -> float expr
  | ExIntAdd : int expr * int expr -> int expr
  | ExIntMul : int expr * int expr -> int expr
  | ExMatAdd : Mat.mat expr * Mat.mat expr -> Mat.mat expr
  | ExMatSub : Mat.mat expr * Mat.mat expr -> Mat.mat expr
  | ExMatMul : Mat.mat expr * Mat.mat expr -> Mat.mat expr
  | ExMatTrans : Mat.mat expr -> Mat.mat expr
  | ExMatInv : Mat.mat expr -> Mat.mat expr
  | ExMatScalarMul : float expr * Mat.mat expr -> Mat.mat expr
  | ExMatGet : Mat.mat expr * int -> float expr
  | ExMatSingle : float expr -> Mat.mat expr
  | ExCmp : cmp_op * 'a expr * 'a expr -> bool expr
  | ExPair : 'a expr * 'b expr -> ('a * 'b) expr
  | ExArray : 'a expr array -> 'a array expr
  | ExMatrix : 'a expr array array -> 'a array array expr
  | ExIte : bool expr * 'a expr * 'a expr -> 'a expr
  | ExList : 'a expr list -> 'a list expr
  | ExUnop : unary_op * float expr -> float expr
  | ExIntToFloat : int expr -> float expr
and 'a distribution =
  | Normal : float expr * float expr -> float distribution
  | MvNormal : Mat.mat expr * Mat.mat expr -> Mat.mat distribution
  | Categorical : domain * float factor expr -> category distribution
  | Beta : float expr * float expr -> float distribution
  | Bernoulli : float expr -> bool distribution
  | Binomial : int expr * float expr -> int distribution
  | BetaBinomial : int expr * float expr * float expr -> int distribution
  | NegativeBinomial : int expr * float expr -> int distribution
  | Gamma : float expr * float expr -> float distribution
  | Poisson : float expr -> int distribution
  | StudentT : float expr * float expr * float expr -> float distribution
  | Delta : 'a expr -> 'a distribution
  | Mixture : ('a expr * float) list -> 'a distribution (* probabilities NOT logscale *)
  | Sampler : ((unit -> 'a) * ('a -> float)) -> 'a distribution
and 'a random_var = {
    name : string; (* Used only for debugging *)
    mutable distr : 'a distribution
}

let rec ex_add e1e2 =
  match e1e2 with
  | ExConst c1, ExConst c2 -> ExConst (c1 +. c2)
  | ExConst c1, ExAdd (ExConst c2, e3) -> ex_add (ExConst (c1 +. c2), e3)
  | ExConst c1, ExAdd (e2, ExConst c3) -> ex_add (ExConst (c1 +. c3), e2)
  | ExAdd (ExConst c1, e2), e3 -> ex_add (ExConst c1, ex_add (e2, e3))
  | (e1, e2) -> ExAdd (e1, e2)

let rec ex_int_add e1e2 =
  match e1e2 with
  | ExConst c1, ExConst c2 -> ExConst (c1 + c2)
  | ExConst c1, ExIntAdd (ExConst c2, e3) -> ex_int_add (ExConst (c1 + c2), e3)
  | ExConst c1, ExIntAdd (e2, ExConst c3) -> ex_int_add (ExConst (c1 + c3), e2)
  | ExIntAdd (ExConst c1, e2), e3 -> ex_int_add (ExConst c1, ex_int_add (e2, e3))
  | (e1, e2) -> ExIntAdd (e1, e2)

let rec ex_mul e1e2 =
  match e1e2 with
  | ExConst c1, ExConst c2 -> ExConst (c1 *. c2)
  | ExConst c1, ExMul (ExConst c2, e3) -> ex_mul (ExConst (c1 *. c2), e3)
  | ExConst c1, ExMul (e2, ExConst c3) -> ex_mul (ExConst (c1 *. c3), e2)
  | ExConst c1, ExAdd (ExConst c2, e3) -> ex_add (ExConst (c1 *. c2), ex_mul (ExConst c1, e3))
  | (e1, e2) -> ExMul (e1, e2)

let rec ex_int_mul e1e2 =
  match e1e2 with
  | ExConst c1, ExConst c2 -> ExConst (c1 * c2)
  | ExConst c1, ExIntMul (ExConst c2, e3) -> ex_int_mul (ExConst (c1 * c2), e3)
  | ExConst c1, ExIntMul (e2, ExConst c3) -> ex_int_mul (ExConst (c1 * c3), e2)
  | ExConst c1, ExIntAdd (ExConst c2, e3) -> ex_int_add (ExConst (c1 * c2), ex_int_mul (ExConst c1, e3))
  | (e1, e2) -> ExIntMul (e1, e2)

let ex_div e1e2 =
  match e1e2 with
  | ExConst c1, ExConst c2 -> ExConst (c1 /. c2)
  | (e1, e2) -> ExDiv (e1, e2)

let ex_unop ope =
  match ope with
  | Squared, ExConst c -> ExConst (c ** 2.)
  | SquareRoot, ExConst c -> ExConst (Float.sqrt c)
  | Exp, ExConst c -> ExConst (Float.exp c)
  | (op, e) -> ExUnop (op, e)

let ex_cmp ope1e2 =
  match ope1e2 with
  | Eq, ExConst c1, ExConst c2 -> ExConst (c1 = c2)
  | Lt, ExConst c1, ExConst c2 -> ExConst (c1 < c2)
  | (op, e1 , e2) -> ExCmp (op, e1, e2)

let ex_ite e1e2e3 =
  match e1e2e3 with
  | ExConst true, e2, _ -> e2
  | ExConst false, _, e3 -> e3
  | (e1, e2, e3) -> ExIte (e1, e2, e3)

let ex_int_to_float e =
  match e with
  | ExConst c -> ExConst (float_of_int c)
  | e -> ExIntToFloat e

let ex_mat_add e1e2 =
  match e1e2 with
  | ExConst c1, ExConst c2 -> ExConst (Mat.add c1 c2)
  | (e1, e2) -> ExMatAdd (e1, e2)

let ex_mat_sub e1e2 =
  match e1e2 with
  | ExConst c1, ExConst c2 -> ExConst (Mat.sub c1 c2)
  | (e1, e2) -> ExMatSub (e1, e2)

let ex_mat_mul e1e2 =
  match e1e2 with
  | ExConst c1, ExConst c2 -> ExConst (Mat.dot c1 c2)
  | (e1, e2) -> ExMatMul (e1, e2)

let ex_mat_trans e =
  match e with
  | ExConst c -> ExConst (Mat.transpose c)
  | _ -> ExMatTrans e

let ex_mat_inv e =
  match e with
  | ExConst c -> ExConst (Linalg.D.inv c)
  | _ -> ExMatInv e

let ex_mat_scalar_mul e1e2 =
  match e1e2 with
  | ExConst c1, ExConst c2 -> ExConst (Mat.scalar_mul c1 c2)
  | (e1, e2) -> ExMatScalarMul (e1, e2)

let ex_mat_get ei =
  match ei with
  | ExConst c, i -> ExConst (Mat.get c i 0)
  | (e, i) -> ExMatGet (e, i)

let ex_mat_single e =
  match e with
  | ExConst c -> ExConst(Mat.of_arrays [| [| c |] |])
  | _ -> ExMatSingle e

(* Debug functions *)

let rec string_of_expr_gen : type a. (a -> string) -> a expr -> string =
fun string_of_const e ->
  let string_of_expr = string_of_expr_gen string_of_const in
  begin match e with
  | ExConst c -> string_of_const c
  | ExRand rv -> "RV: " ^ rv.name
  | ExVar _ -> "var"
  | ExAdd (e1, e2) -> "(" ^ (string_of_expr e1) ^ ") + (" ^ (string_of_expr e2) ^ ")"
  | ExMul (e1, e2) -> "(" ^ (string_of_expr e1) ^ ") * (" ^ (string_of_expr e2) ^ ")"
  | ExDiv (e1, e2) -> "(" ^ (string_of_expr e1) ^ ") / (" ^ (string_of_expr e2) ^ ")"
  | ExIte (_, _, _) -> "ite"
  | ExUnop(Squared, e_inner) -> "(" ^ string_of_expr e_inner ^ ")^2"
  | ExUnop(SquareRoot, e_inner) -> "sqrt(" ^ string_of_expr e_inner ^ ")"
  | ExUnop(Exp, e_inner) -> "expp(" ^ string_of_expr e_inner ^ ")"
  | _ -> assert false
  end

and string_of_distribution_gen : type a. (a -> string) -> a distribution -> string =
  fun string_of_const distr ->
    let string_of_expr = string_of_expr_gen string_of_const in
    match distr with
    | Normal (e1, e2) -> "Normal("^(string_of_expr e1)^","^(string_of_expr e2)^")"
    | _ -> assert false

let string_of_expr : float expr -> string =
  string_of_expr_gen string_of_float

let string_of_expr_any : type a. a expr -> string =
  fun expr ->
    string_of_expr_gen (fun _ -> "const") expr

let rec size_expr:  type a. a expr -> int =
  fun expr ->
    match expr with
    | ExConst _ -> 1
    | ExRand _ -> 1
    | ExVar _ -> 1
    | ExFactor (_, _, e) -> 1 + size_expr e
    | ExGet (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | ExLet (_, e1, e2) -> 1 + size_expr e1 + size_expr e2
    | ExSum (_, _, e) -> 1 + size_expr e
    | ExAdd (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | ExMul (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | ExDiv (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | ExIntAdd (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | ExIntMul (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | ExMatAdd (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | ExMatSub (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | ExMatMul (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | ExMatTrans e -> 1 + size_expr e
    | ExMatInv e -> 1 + size_expr e
    | ExMatScalarMul (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | ExMatGet (e, _) -> 1 + size_expr e
    | ExMatSingle e -> 1 + size_expr e
    | ExCmp (_, e1, e2) -> 1 + size_expr e1 + size_expr e2
    | ExPair (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | ExArray a -> Array.fold_left (fun acc e -> acc + size_expr e) 1 a
    | ExMatrix m ->
        Array.fold_left
          (fun acc a -> Array.fold_left (fun acc e -> acc + size_expr e) acc a)
          1 m
    | ExIte (e, e1, e2) -> size_expr e + size_expr e1 + size_expr e2
    | ExList l -> List.fold_left (fun acc e -> acc + size_expr e) 1 l
    | ExUnop (_, e) -> 1 + size_expr e
    | ExIntToFloat e -> 1 + size_expr e

and size_distr: type a. a distribution -> int =
  fun d ->
    match d with
    | Normal (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | MvNormal (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | Categorical (_, e) -> 1 + size_expr e
    | Beta (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | Bernoulli e -> 1 + size_expr e
    | Binomial (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | BetaBinomial (e1, e2, e3) -> 1 + size_expr e1 + size_expr e2 + size_expr e3
    | NegativeBinomial (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | Gamma (e1, e2) -> 1 + size_expr e1 + size_expr e2
    | Poisson e -> 1 + size_expr e
    | StudentT (e1, e2, e3) -> 1 + size_expr e1 + size_expr e2 + size_expr e3
    | Delta e -> 1 + size_expr e
    | Mixture l -> 
      1 + List.fold_left (fun acc (e, _) -> max acc (size_expr e)) 1 l
    | Sampler _ -> 1

(* Utility functions *)

let rec subst : type a b. a expr -> b var -> b expr -> a expr =
  fun e v e' -> match e with
  | ExVar v' -> if v = Obj.magic v' then Obj.magic e' else e
  | ExConst _ -> e
  | ExRand _ -> e
  | ExFactor (v', d, e) -> ExFactor (v', d, subst e v e')
  | ExGet (e1, e2) -> ExGet (subst e1 v e', subst e2 v e')
  | ExLet (v', e1, e2) -> ExLet (v', subst e1 v e', subst e2 v e')
  | ExSum (v', d, e) -> ExSum (v', d, subst e v e')
  | ExAdd (e1, e2) -> ex_add (subst e1 v e', subst e2 v e')
  | ExMul (e1, e2) -> ex_mul (subst e1 v e', subst e2 v e')
  | ExDiv (e1, e2) -> ex_div (subst e1 v e', subst e2 v e')
  | ExIntAdd (e1, e2) -> ex_int_add (subst e1 v e', subst e2 v e')
  | ExIntMul (e1, e2) -> ex_int_mul (subst e1 v e', subst e2 v e')
  | ExMatAdd (e1, e2) -> ex_mat_add (subst e1 v e', subst e2 v e')
  | ExMatSub (e1, e2) -> ex_mat_sub (subst e1 v e', subst e2 v e')
  | ExMatMul (e1, e2) -> ex_mat_mul (subst e1 v e', subst e2 v e')
  | ExMatTrans e -> ex_mat_trans (subst e v e')
  | ExMatInv e -> ex_mat_inv (subst e v e')
  | ExMatScalarMul (e1, e2) -> ex_mat_scalar_mul (subst e1 v e', subst e2 v e')
  | ExMatGet (e, i) -> ex_mat_get (subst e v e', i)
  | ExMatSingle (e) -> ex_mat_single(subst e v e')
  | ExCmp (op, e1, e2) -> ex_cmp (op, subst e1 v e', subst e2 v e')
  | ExPair (e1, e2) -> ExPair (subst e1 v e', subst e2 v e')
  | ExArray e -> ExArray (Array.map (fun e -> subst e v e') e)
  | ExMatrix e -> ExMatrix (Array.map (Array.map (fun e -> subst e v e')) e)
  | ExIte (e1, e2, e3) -> ex_ite (subst e1 v e', subst e2 v e', subst e3 v e')
  | ExList e -> ExList (List.map (fun e -> subst e v e') e)
  | ExUnop (op, e) -> ex_unop (op, subst e v e')
  | ExIntToFloat e -> ex_int_to_float (subst e v e')

let rec subst_rv : type a b. a expr -> b random_var -> b expr -> a expr =
  fun e v e' ->
    match e with
  | ExVar _ -> e
  | ExConst _ -> e
  | ExRand v' -> if v == Obj.magic v' then Obj.magic e' else e
  | ExFactor (v', d, e) -> ExFactor (v', d, subst_rv e v e')
  | ExGet (e1, e2) -> ExGet (subst_rv e1 v e', subst_rv e2 v e')
  | ExLet (v', e1, e2) -> ExLet (v', subst_rv e1 v e', subst_rv e2 v e')
  | ExSum (v', d, e) -> ExSum (v', d, subst_rv e v e')
  | ExAdd (e1, e2) -> ex_add (subst_rv e1 v e', subst_rv e2 v e')
  | ExMul (e1, e2) -> ex_mul (subst_rv e1 v e', subst_rv e2 v e')
  | ExDiv (e1, e2) -> ex_div (subst_rv e1 v e', subst_rv e2 v e')
  | ExIntAdd (e1, e2) -> ex_int_add (subst_rv e1 v e', subst_rv e2 v e')
  | ExIntMul (e1, e2) -> ex_int_mul (subst_rv e1 v e', subst_rv e2 v e')
  | ExMatAdd (e1, e2) -> ex_mat_add (subst_rv e1 v e', subst_rv e2 v e')
  | ExMatSub (e1, e2) -> ex_mat_sub (subst_rv e1 v e', subst_rv e2 v e')
  | ExMatMul (e1, e2) -> ex_mat_mul (subst_rv e1 v e', subst_rv e2 v e')
  | ExMatTrans e -> ex_mat_trans (subst_rv e v e')
  | ExMatInv e -> ex_mat_inv (subst_rv e v e')
  | ExMatScalarMul (e1, e2) -> ex_mat_scalar_mul (subst_rv e1 v e', subst_rv e2 v e')
  | ExMatGet (e, i) -> ex_mat_get (subst_rv e v e', i)
  | ExMatSingle (e) -> ex_mat_single (subst_rv e v e')
  | ExCmp (op, e1, e2) -> ex_cmp (op, subst_rv e1 v e', subst_rv e2 v e')
  | ExPair (e1, e2) -> ExPair (subst_rv e1 v e', subst_rv e2 v e')
  | ExArray e -> ExArray (Array.map (fun e -> subst_rv e v e') e)
  | ExMatrix e -> ExMatrix (Array.map (Array.map (fun e -> subst_rv e v e')) e)
  | ExIte (e1, e2, e3) -> ex_ite (subst_rv e1 v e', subst_rv e2 v e', subst_rv e3 v e')
  | ExList e -> ExList (List.map (fun e -> subst_rv e v e') e)
  | ExUnop (op, e) -> ex_unop (op, subst_rv e v e')
  | ExIntToFloat e -> ex_int_to_float (subst_rv e v e')

exception InternalError of string
exception NonConjugate : 'a random_var -> exn
exception InterfaceError of string
exception MatrixShapeError of unit

(* Helper functions *)

let rv_number = ref 0

let gensym_semi_symbolic _ =
  let tmp = !rv_number in
  rv_number := !rv_number + 1;
  Var tmp

let is_const_array a =
  Array.for_all
    (fun x -> match x with ExConst _ -> true | _ -> false)
    a

let is_const_list a =
  List.for_all (function ExConst _ -> true | _ -> false) a

type approx_status = 
| Exact of int
| Approx of int

let rv_approx_status : (string, approx_status * approx_status) Hashtbl.t= Hashtbl.create 100

let record_new_rv var =
  match Hashtbl.find_opt rv_approx_status var with
  | None -> Hashtbl.add rv_approx_status var (Exact 0, Approx 0)
  | Some _ -> ()

let record_approx_status var status =
  match Hashtbl.find_opt rv_approx_status var with
  | None -> 
    begin match status with
    | Exact _ -> Hashtbl.add rv_approx_status var (Exact 1, Approx 0)
    | Approx _ -> Hashtbl.add rv_approx_status var (Exact 0, Approx 1)
    end
  | Some (Exact(e), Approx(a)) ->
    begin match status with
    | Exact _ -> 
      Hashtbl.replace rv_approx_status var (Exact (e + 1), Approx(a))
    | Approx _ -> 
      Hashtbl.replace rv_approx_status var (Exact(e), Approx (a + 1))
    end
  | _ -> failwith "Approx status error"
    
let pp_approx_status : bool -> string =
fun obs ->
  Hashtbl.fold (fun key value acc -> (key, value)::acc) rv_approx_status []
  |> List.sort compare
  |> List.fold_left (fun acc (var, status) ->
    match status with
    | Exact(e), Approx(a) ->
      if not obs && String.starts_with ~prefix: "obs" var then acc
      else if not (e = 0) && not (a = 0) then
        Format.sprintf "%s%s: DYNAMIC (e-%d,a-%d)\n" acc var e a
      else if not (a = 0) then
        Format.sprintf "%s%s: APPROX (%d)\n" acc var a
      else
        Format.sprintf "%s%s: EXACT (%d)\n" acc var e
    | _ -> failwith "Approx status error") ""

(* Partially evaluates expressions *)
let rec eval : type a. a expr -> a expr =
fun expr ->
  match expr with
  | ExConst c -> ExConst c
  | ExRand rv ->
    begin match rv.distr with
    | Delta e -> eval e
    | _ ->
      rv.distr <- eval_distr rv.distr;
      ExRand rv
    end
  | ExVar x -> ExVar x
  | ExFactor (x, domain, e) ->
    let e = eval e in
    let factor =
      Array.init (domain.upper - domain.lower + 1)
      (fun i -> subst e x (ExConst (i + domain.lower))) in
    eval (ExPair (ExArray factor, ExConst domain))
  | ExLet (x, e, e') ->
    begin match eval e with
    | ExConst e -> eval (subst e' x (ExConst e))
    | e'' -> ExLet (x, e'', eval e')
    end
  | ExGet (e1, e2) ->
    begin match eval e1, eval e2 with
    | ExConst (e1, domain), ExConst e2 -> ExConst (e1.(e2 - domain.lower))
    | e1', e2' -> ExGet (e1', e2')
    end
  | ExSum (x, domain, e) ->
    let e = eval e in begin
      try
        let range = List.init (domain.upper - domain.lower + 1) (fun i -> i) in
        let sum = List.fold_left begin fun sum i ->
          match eval (subst e x (ExConst (i + domain.lower))) with
          | ExConst c -> c +. sum
          | _ -> raise_notrace Exit
        end 0. range in ExConst sum
      with Exit -> ExSum (x, domain, e)
    end
  | ExAdd (e1, e2) -> ex_add (eval e1, eval e2)
  | ExMul (e1, e2) -> ex_mul (eval e1, eval e2)
  | ExDiv (e1, e2) -> ex_div(eval e1, eval e2)
  | ExIntAdd (e1, e2) -> ex_int_add (eval e1, eval e2)
  | ExIntMul (e1, e2) -> ex_int_mul (eval e1, eval e2)
  | ExMatAdd (e1, e2) -> ex_mat_add (eval e1, eval e2)
  | ExMatSub (e1, e2) -> ex_mat_sub (eval e1, eval e2)
  | ExMatMul (e1, e2) -> ex_mat_mul (eval e1, eval e2)
  | ExMatTrans (e_inner) -> ex_mat_trans(eval e_inner)
  | ExMatInv (e_inner) -> ex_mat_inv(eval e_inner)
  | ExMatScalarMul (s, e_inner) -> ex_mat_scalar_mul (s, eval e_inner)
  | ExMatGet (e_inner, i) -> ex_mat_get (eval e_inner, i)
  | ExMatSingle(e_inner) -> ex_mat_single(eval e_inner)
  | ExCmp (op, e1, e2) -> ex_cmp(op, eval e1, eval e2)
  | ExPair (e1, e2) ->
    begin match (eval e1, eval e2) with
    | (ExConst c1, ExConst c2) -> ExConst (c1, c2)
    | (e1', e2') -> ExPair(e1', e2')
    end
  | ExArray a ->
    let a = Array.map eval a in
    if is_const_array a then
      ExConst (Array.map (function ExConst c -> c | _ -> assert false) a)
    else
      ExArray a
  | ExMatrix m ->
    let m = Array.map (fun a -> Array.map eval a) m in
    if Array.for_all is_const_array m then
      ExConst
        (Array.map
           (fun a -> Array.map (function ExConst c -> c | _ -> assert false) a)
           m)
    else
      ExMatrix m
  | ExList l ->
    let l = List.map eval l in
    if is_const_list l then
      ExConst (List.map (function ExConst c -> c | _ -> assert false) l)
    else
      ExList l
  | ExIte (i, t, e) ->
    begin match (eval i, eval t, eval e) with
    | (ExConst ci, t', e') -> if ci then t' else e'
    | (i', t', e') -> ex_ite(i', t', e')
    end
  | ExUnop(Squared, e_inner) -> ex_unop(Squared, eval e_inner)
  | ExUnop(SquareRoot, e_inner) -> ex_unop(SquareRoot, eval e_inner)
  | ExUnop(Exp, e_inner) -> ex_unop(Exp, eval e_inner)
  | ExIntToFloat(e_inner) -> ex_int_to_float(eval e_inner)

and eval_distr : type a. a distribution -> a distribution =
fun distr ->
  (* (Printf.printf "eval_distr\n"); *)
  match distr with
  | Normal (mu, var) -> Normal (eval mu, eval var)
  | MvNormal (mu, var) -> MvNormal (eval mu, eval var)
  | Categorical (d, e) -> Categorical (d, eval e)
  | Beta (a, b) -> Beta (eval a, eval b)
  | Bernoulli(p) -> Bernoulli(eval p)
  | Binomial(n, p) -> Binomial(eval n, eval p)
  | BetaBinomial(n, a, b) -> BetaBinomial(eval n, eval a, eval b)
  | NegativeBinomial(n, p) -> NegativeBinomial(eval n, eval p)
  | Gamma(a, b) -> Gamma(eval a, eval b)
  | Poisson(l) -> Poisson(eval l)
  | StudentT (mu, tau2, nu) -> StudentT (eval mu, eval tau2, eval nu)
  | Delta e -> Delta (eval e)
  | Mixture l -> Mixture (List.map (fun (e, p) -> (eval e, p)) l)
  | Sampler e -> Sampler e

(* Returns Some(a,b) if e can be written as an affine function of
 * rv_in (e = a * rv_in + b) *)
let rec is_affine : float expr -> float random_var -> (float expr * float expr) option =
fun e rv_in ->
  match e with
  | ExConst c -> Some (ExConst 0., ExConst c)
  | ExRand rv ->
    if rv == rv_in then
      Some(ExConst 1., ExConst 0.)
    else
      Some (ExConst 0., ExRand rv)
  | ExAdd (e1, e2) ->
    begin match (is_affine e1 rv_in, is_affine e2 rv_in) with
    | (Some (a1, b1), Some (a2, b2)) -> Some (ex_add(a1, a2), ex_add(b1, b2))
    | _ -> None
    end
  | ExMul (e1, e2) ->
    begin match (is_affine e1 rv_in, is_affine e2 rv_in) with
    | (Some(a1, b1), Some(a2, b2)) ->
      begin match eval a1, eval a2 with
      | ExConst 0., ExConst 0. -> Some(ExConst 0., ex_mul(b1, b2))
      | a1, ExConst 0. -> Some(ex_mul(a1, b2), ex_mul(b1, b2))
      | ExConst 0., a2 -> Some(ex_mul(b1, a2), ex_mul(b1, b2))
      | _ -> None
      end
    | _ -> None
    end
  | ExDiv (e1, e2) ->
    begin match (is_affine e1 rv_in, is_affine e2 rv_in) with
    | (Some (a1, b1), Some(a2, b2)) ->
      begin match eval a2 with
      | ExConst 0. -> Some (ex_div (a1, b2), ex_div(b1, b2))
      | _ -> None
      end
    | _ -> None
    end
  (* TODO: what if this is a constant? *)
  | ExIte (_, _, _) -> None
  (* For now, all unary operations are non-linear, but may be constant *)
  | ExUnop(op, e_inner) ->
    begin match is_affine e_inner rv_in with
    | Some(a_inner, b_inner) ->
      begin match eval a_inner with
      | ExConst 0. -> Some(ExConst 0., ex_unop (op, b_inner))
      | _ -> None
      end
    | _ -> None
    end
  (* TODO: What to do if not constant? If a RV? *)
  | ExIntToFloat _ -> None
  | ExMatAdd _ | ExMatSub _ | ExMatMul _
  | ExMatTrans _ | ExMatInv _ | ExMatScalarMul _ | ExMatGet _
  | ExMatSingle _
  | ExLet _ | ExVar _ | ExSum _ | ExGet _ -> None

let rec mat_shape : Mat.mat expr -> (int * int) =
fun e ->
  begin match e with
  | ExConst (m) -> Mat.shape m
  | ExRand rv ->
    begin match rv.distr with
    | MvNormal(mu, _) ->
      mat_shape mu
    | _ -> raise (MatrixShapeError ())
    end
  | ExMatMul(m1, m2) ->
    begin match (mat_shape m1, mat_shape m2) with
    | ((a, b), (b', c)) when b = b' -> (a, c)
    | _ -> raise (MatrixShapeError ())
    end
  | ExMatAdd (m1, m2) | ExMatSub (m1, m2) | ExIte (_, m1, m2) ->
    begin match (mat_shape m1, mat_shape m2) with
    | ((a, b), (a', b')) when a = a' && b = b' -> (a, b)
    | _ -> raise (MatrixShapeError ())
    end
  | ExMatTrans(m_inner) ->
    let (a, b) = mat_shape m_inner in
    (b, a)
  | ExMatInv(m_inner) ->
    begin match mat_shape m_inner with
    | (n, n') when n = n' -> (n, n)
    | _ -> raise (MatrixShapeError ())
    end
  | ExMatSingle (_) -> (1, 1)
  | ExMatScalarMul (_, m_inner) -> mat_shape m_inner
  | ExAdd _  | ExMul _ | ExDiv _ | ExIntAdd _ | ExIntMul _ | ExArray _ | ExMatrix _ | ExList _
  | ExPair _ | ExUnop _ | ExCmp _ | ExMatGet _ | ExIntToFloat _
  | ExFactor _ | ExLet _ | ExVar _ | ExSum _ | ExGet _ -> raise (MatrixShapeError ())
  end

(* Fold over expressions *)
(*
let rec fold_expr : type a b ctx ret. (a, ctx) expr -> ret -> (ret -> b random_var -> ret) -> ret =
fun e init foldfn ->
  match e with
  | ExConst _ -> init
  | ExRand rv -> foldfn init rv
  | ExRef -> init
  | ExAdd(e1, e2) ->
    let tmp = fold_expr e1 init foldfn in
    fold_expr e2 tmp foldfn
  | ExMul(e1, e2) ->
    let tmp = fold_expr e1 init foldfn in
    fold_expr e2 tmp foldfn
  | ExDiv(e1, e2) ->
    let tmp = fold_expr e1 init foldfn in
    fold_expr e2 tmp foldfn
  | ExCmp(_, e1, e2) ->
    let tmp = fold_expr e1 init foldfn in
    fold_expr e2 tmp foldfn
  | ExIte(i, t, e) ->
    let tmp1 = fold_expr i init foldfn in
    let tmp2 = fold_expr t tmp1 foldfn in
    fold_expr e tmp2 foldfn
  | ExUnop (_, e_inner) ->
    fold_expr e_inner init foldfn
*)

(* Returns true iff rv is in the free variables of e *)
let rec depends_on : type a b. a expr -> b random_var -> bool -> bool =
fun e rv_in transitive ->
  match e with
  | ExConst _ | ExVar _ -> false
  | ExRand rv ->
    let rv_in' : a random_var = Obj.magic rv_in in
    if rv_in' == rv then
      true
    else
      if transitive then
        begin match rv.distr with
        | Normal (mu, var) -> (depends_on mu rv_in transitive) || (depends_on var rv_in transitive)
        | Beta (a, b) -> (depends_on a rv_in transitive) || (depends_on b rv_in transitive)
        | Bernoulli (p) -> (depends_on p rv_in transitive)
        | Binomial (n, p) -> (depends_on n rv_in transitive) || (depends_on p rv_in transitive)
        | BetaBinomial (n, a, b) -> (depends_on n rv_in transitive) || 
                                    (depends_on a rv_in transitive) || 
                                    (depends_on b rv_in transitive)
        | NegativeBinomial(n, p) -> (depends_on n rv_in transitive) || 
                                    (depends_on p rv_in transitive)
        | Gamma (a, b) -> (depends_on a rv_in transitive) || (depends_on b rv_in transitive)
        | Poisson (lambda) -> (depends_on lambda rv_in transitive)
        | StudentT (mu, tau2, nu) -> (depends_on mu rv_in transitive) || 
                                    (depends_on tau2 rv_in transitive) || 
                                    (depends_on nu rv_in transitive)
        | Categorical (_, e) -> depends_on e rv_in transitive
        | Delta e -> (depends_on e rv_in transitive)
        | MvNormal (mu, var) -> (depends_on mu rv_in transitive) || (depends_on var rv_in transitive)
        | Mixture l -> List.exists (fun (e, _) -> depends_on e rv_in transitive) l
        | Sampler _ -> false
        end
      else
        false
  (* | ExRef ->  false *)
  | ExAdd (e1, e2) -> (depends_on e1 rv_in transitive) || (depends_on e2 rv_in transitive)
  | ExMul (e1, e2) -> (depends_on e1 rv_in transitive) || (depends_on e2 rv_in transitive)
  | ExDiv (e1, e2) -> (depends_on e1 rv_in transitive) || (depends_on e2 rv_in transitive)
  | ExIntAdd (e1, e2) -> (depends_on e1 rv_in transitive) || (depends_on e2 rv_in transitive)
  | ExIntMul (e1, e2) -> (depends_on e1 rv_in transitive) || (depends_on e2 rv_in transitive)
  | ExCmp (_, e1, e2) -> (depends_on e1 rv_in transitive) || (depends_on e2 rv_in transitive)
  | ExPair (e1, e2) -> (depends_on e1 rv_in transitive) || (depends_on e2 rv_in transitive)
  | ExArray a -> Array.exists (fun e -> depends_on e rv_in transitive) a
  | ExMatrix m ->
    Array.exists
      (fun a -> Array.exists (fun e -> depends_on e rv_in transitive) a)
      m
  | ExList l -> List.exists (fun e -> depends_on e rv_in transitive) l
  | ExIte (i, t, e) -> (depends_on i rv_in transitive) || (depends_on t rv_in transitive) || (depends_on e rv_in transitive)
  | ExUnop (_, e_inner) -> depends_on e_inner rv_in transitive
  | ExIntToFloat e -> depends_on e rv_in transitive
  | ExMatAdd (e1, e2) | ExMatSub (e1, e2) | ExMatMul (e1, e2) ->
      (depends_on e1 rv_in transitive) || (depends_on e2 rv_in transitive)
  | ExMatScalarMul (s, e) -> (depends_on s rv_in transitive) || (depends_on e rv_in transitive)
  | ExMatTrans e | ExMatInv e | ExMatGet (e, _)  -> depends_on e rv_in transitive
  | ExMatSingle e -> depends_on e rv_in transitive
  | ExFactor (_, _, e) -> depends_on e rv_in transitive
  | ExSum (_, _, e) -> depends_on e rv_in transitive
  | ExGet (e1, e2) -> depends_on e1 rv_in transitive || depends_on e2 rv_in transitive
  | ExLet (_, e1, e2) -> depends_on e1 rv_in transitive || depends_on e2 rv_in transitive

type 'a affine_expr = AEExp of 'a * 'a random_var * 'a | AEConst of 'a

(* Returns Some e' if e can be written as an affine function of
 * rv_in (e = m *@ rv_in + b) *)
let rec is_affine_mat : Mat.mat expr -> Mat.mat random_var -> Mat.mat affine_expr option =
  fun e rv_in ->
    begin match e with
    | ExConst m -> Some (AEConst m)
    | ExRand rv ->
      if rv == rv_in
      then
        let (sz, _) = mat_shape (ExRand rv_in) in
        Some (AEExp (Mat.eye sz, rv, Mat.zeros sz 1))
      else None
    | ExMatMul (e1, e2) ->
      begin match is_affine_mat e1 rv_in, is_affine_mat e2 rv_in with
      | Some (AEConst m1), Some (AEExp (m, x, b)) -> Some (AEExp (Mat.dot m1 m, x, Mat.dot m1 b))
      | Some (AEConst m1), Some (AEConst m2) -> Some (AEConst (Mat.dot m1 m2))
      | _ -> None
      end
    | ExMatAdd (e1, e2) ->
      begin match (is_affine_mat e1 rv_in, is_affine_mat e2 rv_in) with
      | Some (AEExp (m, x, b)), Some (AEConst c)
      | Some (AEConst c), Some (AEExp (m, x, b)) -> Some (AEExp (m, x, Mat.add b c))
      | Some (AEConst c1), Some (AEConst c2) -> Some (AEConst (Mat.add c1 c2))
      | _ -> None
      end
    | ExMatSub (e1, e2) ->
      begin match (is_affine_mat e1 rv_in, is_affine_mat e2 rv_in) with
      | Some (AEExp (m, x, b)), Some (AEConst c)
      | Some (AEConst c), Some (AEExp (m, x, b)) -> Some (AEExp (Mat.scalar_mul (-1.) m, x, Mat.sub b c))
      | Some (AEConst c1), Some (AEConst c2) -> Some (AEConst (Mat.sub c1 c2))
      | _ -> None
      end
    | ExMatScalarMul (s, e) ->
      begin match eval s, is_affine_mat e rv_in with
      | ExConst c, Some (AEExp (m, x, b)) -> Some (AEExp (Mat.scalar_mul c m, x, Mat.scalar_mul c b))
      | ExConst c1, Some (AEConst c2) -> Some (AEConst (Mat.scalar_mul c1 c2))
      | _ -> None
      end
    | _ -> None
    end

let rec has_parents : type a. a expr -> bool =
fun e ->
  match e with
  | ExConst _ | ExVar _ -> false
  | ExRand _ -> true
  | ExAdd (e1, e2) -> (has_parents e1) || (has_parents e2)
  | ExMul (e1, e2) -> (has_parents e1) || (has_parents e2)
  | ExDiv (e1, e2) -> (has_parents e1) || (has_parents e2)
  | ExIntAdd (e1, e2) -> (has_parents e1) || (has_parents e2)
  | ExIntMul (e1, e2) -> (has_parents e1) || (has_parents e2)
  | ExPair (e1, e2) -> (has_parents e1) || (has_parents e2)
  | ExArray a -> Array.exists has_parents a
  | ExMatrix m -> Array.exists (Array.exists has_parents) m
  | ExList l -> List.exists has_parents l
  | ExCmp (_, e1, e2) -> (has_parents e1) || (has_parents e2)
  | ExIte (i, t, e) -> (has_parents i) || (has_parents t) || (has_parents e)
  | ExUnop (_, e_inner) -> has_parents e_inner
  | ExIntToFloat e -> has_parents e
  | ExMatAdd (e1, e2) | ExMatSub (e1, e2) | ExMatMul (e1, e2) ->
      (has_parents e1) || (has_parents e2)
  | ExMatScalarMul (s, e) -> (has_parents s) || (has_parents e)
  | ExMatTrans e | ExMatInv e | ExMatGet (e, _) -> has_parents e
  | ExMatSingle e -> has_parents e
  | ExFactor (_, _, e) -> has_parents e
  | ExSum (_, _, e) -> has_parents e
  | ExGet (e1, e2) -> has_parents e1 || has_parents e2
  | ExLet (_, e1, e2) -> has_parents e1 || has_parents e2

let has_parents_rv : type a. a random_var -> bool = fun rv' ->
  match rv'.distr with
  | Normal (mu, var) -> (has_parents mu) || (has_parents var)
  | Beta (a, b) -> (has_parents a) || (has_parents b)
  | Categorical (_, e) -> has_parents e
  | Bernoulli (p) -> has_parents p
  | Binomial (n, p) -> (has_parents n) || (has_parents p)
  | BetaBinomial (n, a, b) -> (has_parents n) || (has_parents a) || (has_parents b)
  | NegativeBinomial (n, p) -> (has_parents n) || (has_parents p)
  | Gamma (a, b) -> (has_parents a) || (has_parents b)
  | Poisson (lambda) -> has_parents lambda
  | StudentT (mu, tau2, nu) -> (has_parents mu) || (has_parents tau2) || (has_parents nu)
  | Delta e -> has_parents e
  | MvNormal (mu, var) -> (has_parents mu) || (has_parents var)
  | Mixture l -> List.exists (fun (e, _) -> has_parents e) l
  | Sampler _ -> false

(* Returns whether the expression has any parents that are _not_ _direct_
 * parents of rv_child *)
let rec has_parents_except : type a b. a expr -> b random_var -> bool =
fun e rv_child ->
  match e with
  | ExConst _ | ExVar _ -> false
  | ExRand rv ->
    let is_parent_rv_child =
      match rv_child.distr with
      | Normal (mu, var) -> (depends_on mu rv false) || (depends_on var rv false)
      | Beta (a, b) -> (depends_on a rv false) || (depends_on b rv false)
      | Categorical (_, e) -> (depends_on e rv false)
      | Bernoulli (p) -> depends_on p rv false
      | Binomial (n, p) -> (depends_on n rv false) || (depends_on p rv false)
      | BetaBinomial (n, a, b) -> (depends_on n rv false) || 
                                  (depends_on a rv false) || 
                                  (depends_on b rv false)
      | NegativeBinomial (n, p) -> (depends_on n rv false) || (depends_on p rv false)
      | Gamma (a, b) -> (depends_on a rv false) || (depends_on b rv false)
      | Poisson (lambda) -> depends_on lambda rv false
      | StudentT (mu, tau, nu) -> (depends_on mu rv false) || 
                                  (depends_on tau rv false) || 
                                  (depends_on nu rv false)
      | Delta e_inner -> depends_on e_inner rv false
      | MvNormal (mu, var) -> (depends_on mu rv false) || (depends_on var rv false)
      | Mixture l -> List.exists (fun (e, _) -> depends_on e rv false) l
      | Sampler _ -> false
    in
    not is_parent_rv_child
  | ExAdd (e1, e2) -> (has_parents_except e1 rv_child) || (has_parents_except e2 rv_child)
  | ExMul (e1, e2) -> (has_parents_except e1 rv_child) || (has_parents_except e2 rv_child)
  | ExDiv (e1, e2) -> (has_parents_except e1 rv_child) || (has_parents_except e2 rv_child)
  | ExIntAdd (e1, e2) -> (has_parents_except e1 rv_child) || (has_parents_except e2 rv_child)
  | ExIntMul (e1, e2) -> (has_parents_except e1 rv_child) || (has_parents_except e2 rv_child)
  | ExCmp (_, e1, e2) -> (has_parents_except e1 rv_child) || (has_parents_except e2 rv_child)
  | ExPair (e1, e2) -> (has_parents_except e1 rv_child) || (has_parents_except e2 rv_child)
  | ExArray a ->
    Array.exists (fun e -> has_parents_except e rv_child) a
  | ExMatrix m ->
    Array.exists
      (Array.exists (fun e -> has_parents_except e rv_child))
      m
  | ExList l -> List.exists (fun e -> has_parents_except e rv_child) l
  | ExIte (i, t, e) -> (has_parents_except i rv_child) ||
                       (has_parents_except t rv_child) ||
                       (has_parents_except e rv_child)
  | ExUnop (_, e_inner) -> has_parents_except e_inner rv_child
  | ExIntToFloat e -> has_parents_except e rv_child
  | ExMatAdd (e1, e2) | ExMatSub (e1, e2) | ExMatMul (e1, e2) ->
      (has_parents_except e1 rv_child) || (has_parents_except e2 rv_child)
  | ExMatScalarMul (s, e) -> (has_parents_except s rv_child) || (has_parents_except e rv_child)
  | ExMatTrans e | ExMatInv e | ExMatGet (e, _) -> has_parents_except e rv_child
  | ExMatSingle e -> has_parents_except e rv_child
  | ExFactor (_, _, e) -> has_parents_except e rv_child
  | ExSum (_, _, e) -> has_parents_except e rv_child
  | ExGet (e1, e2) -> has_parents_except e1 rv_child || has_parents_except e2 rv_child
  | ExLet (_, e1, e2) -> has_parents_except e1 rv_child || has_parents_except e2 rv_child

(* Returns whether the expression has any parents that are _not_ _direct_
 * parents of rv_child *)
let has_parents_except_rv : type a b. a random_var -> b random_var -> bool =
fun rv_parent rv_child ->
  match rv_parent.distr with
  | Normal (mu, var) -> (has_parents_except mu rv_child) || (has_parents_except var rv_child)
  | Beta (a, b) -> (has_parents_except a rv_child) || (has_parents_except b rv_child)
  | Categorical (_, e) -> has_parents_except e rv_child
  | Bernoulli (p) -> has_parents_except p rv_child
  | Binomial (n, p) -> (has_parents_except n rv_child) || (has_parents_except p rv_child)
  | BetaBinomial (n, a, b) -> (has_parents_except n rv_child) || 
                              (has_parents_except a rv_child) || 
                              (has_parents_except b rv_child)
  | NegativeBinomial (n, p) -> (has_parents_except n rv_child) || (has_parents_except p rv_child)
  | Gamma (a, b) -> (has_parents_except a rv_child) || (has_parents_except b rv_child)
  | Poisson (lambda) -> has_parents_except lambda rv_child
  | StudentT (mu, tau2, nu) -> (has_parents_except mu rv_child) || 
                              (has_parents_except tau2 rv_child) || 
                              (has_parents_except nu rv_child)
  | Delta e_inner -> has_parents_except e_inner rv_child
  | MvNormal (mu, var) -> (has_parents_except mu rv_child) || (has_parents_except var rv_child)
  | Mixture l -> List.exists (fun (e, _) -> has_parents_except e rv_child) l
  | Sampler _ -> false

(* Assumes rv1 and rv2 have Gaussian distributions and
 * rv1 and rv2 have a linear relationship.
 * Returns the marginal distribution of rv2  *)
let gaussian_marginal : float random_var -> float random_var -> float distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  match prior, likelihood with
  | (Normal(mu_0, var_0), Normal(mu, var)) ->
    begin match is_affine mu rv1 with
    | Some(a, b) ->
        if (not (depends_on mu_0 rv2 true)) &&
           (not (depends_on var_0 rv2 true)) &&
           (not (depends_on var rv1 true)) then
          let mu' = ex_add ((ex_mul (a, mu_0)), b) in
          let var' = ex_add ((ex_mul(ex_unop (Squared, a), var_0)), var) in
          Some(Normal(mu', var'))
        else
          None
    | None -> None
    end
  | _ -> None

(* Assumes rv1 and rv2 have Gaussian distributions and
 * rv1 and rv2 have a linear relationship.
 * Returns the posterior distribution of rv1 conditioned on rv2 *)
let gaussian_posterior : float random_var -> float random_var -> float distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  match prior, likelihood with
  | (Normal(mu_0, var_0), Normal(mu, var)) ->
    begin match is_affine mu rv1 with
    | Some(a, b) ->
        if (not (depends_on mu_0 rv2 true)) &&
           (not (depends_on var_0 rv2 true)) &&
           (not (depends_on var rv1 true)) then

          (* Apply the linear transformation *)
          let mu_0' = ex_add (ex_mul(a, mu_0), b) in
          let var_0' = ex_mul(ex_unop (Squared, a), var_0) in

          (* Perform the conjugate update *)
          let inv_var'' = ex_add (ex_div (ExConst 1., var_0'), ex_div(ExConst 1., var)) in
          let mu'' = ex_mul(ex_div(ExConst 1., inv_var''), ex_add(ex_div(mu_0', var_0'), ex_div (ExRand rv2, var))) in
          let var'' = ex_div(ExConst 1., inv_var'') in

          (* Apply the inverse linear transformation *)
          let mu''' = ex_div (ex_add (mu'', (ex_mul (ExConst (-1.), b))), a) in
          let var''' = ex_div (var'', (ex_unop (Squared, a))) in
          Some(Normal(mu''', var'''))
        else
          None
    | None -> None
    end
  | _ -> None

(* Assumes rv1 and rv2 have Gaussian distributions and
 * rv1 and rv2 have a linear relationship.
 * Returns the marginal distribution of rv2  *)
let mv_gaussian_marginal : Mat.mat random_var -> Mat.mat random_var -> Mat.mat distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  match prior, likelihood with
  | (MvNormal(mu_0, sigma_0), MvNormal(mu, sigma)) ->
    begin match is_affine_mat mu rv1 with
    | Some (AEExp (m, _, b)) ->
      if (not (depends_on mu_0 rv2 true)) &&
         (not (depends_on sigma_0 rv2 true)) &&
         (not (depends_on sigma rv1 true)) then
        let mu' = ex_mat_add (ex_mat_mul (ExConst m, mu_0), ExConst b) in
        let sigma' = ex_mat_add( ex_mat_mul (ex_mat_mul (ExConst m, sigma_0), ex_mat_trans (ExConst m)), sigma) in
        Some (MvNormal (mu', sigma'))
      else None
    | _ -> None
    end
  | _ -> None

(* Assumes rv1 and rv2 have Gaussian distributions and
 * rv1 and rv2 have a linear relationship.
 * Returns the posterior distribution of rv1 conditioned on rv2 *)
let mv_gaussian_posterior : Mat.mat random_var -> Mat.mat random_var -> Mat.mat distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  begin match prior, likelihood with
  | (MvNormal(mu_0, sigma_0), MvNormal(mu, sigma)) ->
    begin match is_affine_mat mu rv1 with
    | Some (AEExp (m, _, b)) ->
      if (not (depends_on mu_0 rv2 true)) &&
         (not (depends_on sigma_0 rv2 true)) &&
         (not (depends_on sigma rv1 true)) then
        let innov = ex_mat_sub (ex_mat_sub (ExRand rv2, ExConst b), ex_mat_mul (ExConst m, mu_0)) in
        let innov_cov = ex_mat_add (ex_mat_mul (ExConst m, ex_mat_mul (sigma_0, ex_mat_trans (ExConst m))), sigma) in
        let gain = ex_mat_mul(sigma_0, ex_mat_mul (ex_mat_trans (ExConst m), ex_mat_inv innov_cov)) in
        let mu' = ex_mat_add(mu_0, ex_mat_mul (gain, innov)) in
        let kh = ex_mat_mul(gain, ExConst m) in
        let (_, n) = mat_shape kh in
        let id = ExConst (Mat.eye n) in
        let sigma' = ex_mat_mul (ex_mat_sub (id, kh), sigma_0) in
        Some (MvNormal(mu', sigma'))
      else None
    | _ -> None
    end
  | _ -> None
  end


(* Assumes rv1 and rv2 have a Beta and Bernoulli distribution, resp.
 * Returns the marginal distribution of rv2*)
let beta_bernoulli_marginal : float random_var -> bool random_var -> bool distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  match (prior, likelihood) with
  | (Beta(a, b), Bernoulli(ExRand(rv))) ->
    if rv == rv1 &&
       (not (depends_on a rv2 true)) &&
       (not (depends_on b rv2 true)) then
      Some(Bernoulli(ex_div(a, ex_add(a, b))))
    else
      None
  | _ -> None

(* Assumes rv1 and rv2 have a Beta and Bernoulli distribution, resp.
 * Returns the conditional distribution of rv1 conditioned on rv2*)
let beta_bernoulli_posterior : float random_var -> bool random_var -> float distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  match (prior, likelihood) with
  | (Beta(a, b), Bernoulli(ExRand(rv))) ->
    if rv == rv1 &&
       (not (depends_on a rv2 true)) &&
       (not (depends_on b rv2 true)) then
      Some(Beta(ex_add(a, ex_ite(ExRand(rv2), ExConst(1.), ExConst(0.))),
       ex_add(b, ex_ite(ExRand(rv2), ExConst(0.), ExConst(1.)))))
    else
      None
  | _ -> None

(* Assumes rv1 and rv2 have a Beta and Binomial distribution, resp.
 * Returns the marginal distribution of rv2*)
 let beta_binomial_marginal : float random_var -> int random_var -> int distribution option =
  fun rv1 rv2 ->
    let prior, likelihood = rv1.distr, rv2.distr in
    match (prior, likelihood) with
    | (Beta(a, b), Binomial(ExConst(n), ExRand(rv))) ->
      if rv == rv1 &&
         (not (depends_on a rv2 true)) &&
         (not (depends_on b rv2 true)) then
        (* let beta_binomial_p = { name = (rv2.name ^ "_beta"); distr = Beta(a, b) } in *)
        (* Some(Binomial(ExConst(n), ExRand(beta_binomial_p))) *)
        Some(BetaBinomial(ExConst(n), a, b))
      else
        None
    | _ -> None

(* Assumes rv1 and rv2 have a Beta and Binomial distribution, resp.
 * Returns the conditional distribution of rv1 conditioned on rv2*)
let beta_binomial_posterior : float random_var -> int random_var -> float distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  match (prior, likelihood) with
  | (Beta(a, b), Binomial(ExConst(n), ExRand(rv))) ->
    if rv == rv1 &&
        (not (depends_on a rv2 true)) &&
        (not (depends_on b rv2 true)) then
      Some(Beta(ex_add(a, ex_int_to_float(ExRand(rv2))),
        ex_add(b, ex_add(ex_int_to_float(ExConst(n)), ex_mul(ExConst(-1.), ex_int_to_float(ExRand(rv2)))))))
    else
      None
  | _ -> None

(* Assumes rv1 and rv2 have a Gamma and Poisson distribution, resp.
 * Returns the marginal distribution of rv2*)
let gamma_poisson_marginal : float random_var -> int random_var -> int distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  match (prior, likelihood) with
  | (Gamma(ExConst(a), b), Poisson(ExRand(rv))) ->
    if rv == rv1 && 
      classify_float (fst(modf a)) = FP_zero && (* a is an int *)
      (not (depends_on b rv2 true)) then
     Some(NegativeBinomial(ExConst(int_of_float(a)), ex_div(b, ex_add(ExConst(1.), b))))
   else
     None
 | _ -> None

(* Assumes rv1 and rv2 have a Gamma and Poisson distribution, resp.
 * Returns the conditional distribution of rv1 conditioned on rv2*)
let gamma_poisson_posterior : float random_var -> int random_var -> float distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  match (prior, likelihood) with
  | (Gamma(ExConst(a), b), Poisson(ExRand(rv))) ->
    if rv == rv1 && 
      classify_float (fst(modf a)) = FP_zero && (* a is an int *)
      (not (depends_on b rv2 true)) then
      (* Assume n = 1 (i.e. time unit is 1). Is this always the case? *)
      (* Some(Delta(ExConst(a))) *)
     Some(Gamma(ex_add(ExConst(a), ExIntToFloat(ExRand(rv2))), ex_add(b, ExConst(1.))))
   else
     None
  | _ -> None

(* Assumes rv1 and rv2 have a inverse Gamma and Normal distribution, resp.
  * Returns the marginal distribution of rv2 *)
let gamma_normal_marginal : float random_var -> float random_var -> float distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  match (prior, likelihood) with
  | (Gamma(a, b), Normal(ExConst(mu), ExDiv(ExConst(1.), ExRand(rv)))) ->
    if rv == rv1 &&
        (not (depends_on a rv2 true)) &&
        (not (depends_on b rv2 true)) then
      Some(StudentT(ExConst(mu), ex_div(b, a), ex_mul(ExConst(2.), a)))
    else 
      None
  | _ -> None

let gamma_normal_posterior : float random_var -> float random_var -> float distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  match (prior, likelihood) with
  | (Gamma(a, b), Normal(ExConst(mu), ExDiv(ExConst(1.), ExRand(rv)))) ->
    if rv == rv1 &&
        (not (depends_on a rv2 true)) &&
        (not (depends_on b rv2 true)) then
      let a' = ex_add(a, ExConst(0.5)) in
      let b' = ex_add(b, ex_mul(ExConst(0.5), 
        ex_unop(Squared, ex_add(ExRand(rv2), ExConst(-. mu))))) in
      Some(Gamma(a', b'))
    else 
      None
  | _ -> None

(* Assumes rv1 and rv2 both have a Bernoulli distribution. 
  * Returns the marginal distribution of rv2 *)
let bernoulli_marginal : bool random_var -> bool random_var -> bool distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  match (prior, likelihood) with
  | Bernoulli(p1), Bernoulli(p2) ->
    if depends_on p2 rv1 false &&
        (not (depends_on p1 rv2 true)) then

      let p2' = ex_add(ex_mul(p1, subst_rv p2 rv1 (ExConst true)), 
        ex_mul(ex_add(ExConst 1., ex_mul(ExConst(-1.), p1)), subst_rv p2 rv1 (ExConst false))) in
      Some(Bernoulli(p2'))
    else
      None
  | _ -> None

let bernoulli_posterior : bool random_var -> bool random_var -> bool distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  match (prior, likelihood) with
  | Bernoulli(p1), Bernoulli(p2) ->
    if depends_on p2 rv1 false &&
        (not (depends_on p1 rv2 true)) then

      let p2' = ex_add(ex_mul(p1, subst_rv p2 rv1 (ExConst true)), 
        ex_mul(ex_add(ExConst 1., ex_mul(ExConst(-1.), p1)), subst_rv p2 rv1 (ExConst false))) in

      let p1'_num_sub = ex_ite (ExRand rv2, p2, ex_add(ExConst 1., ex_mul(ExConst(-1.), p2))) in
      let p1'_num = ex_mul(p1, subst_rv p1'_num_sub rv1 (ExConst true)) in
      let p1'_denom = ex_ite(ExRand rv2, p2', ex_add(ExConst 1., ex_mul(ExConst(-1.), p2'))) in
      Some(Bernoulli(ex_div(p1'_num, p1'_denom)))
    else
      None
  | _ -> None

(* Assumes rv1 and rv2 have Categorical distributions.
 * Returns the marginal distribution of rv2 *)
let categorical_marginal : category random_var -> category random_var -> category distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  match (prior, likelihood) with
  | Categorical (d1, e1), Categorical (d2, e2) ->
    let d1v = gensym_semi_symbolic () in
    let d2v = gensym_semi_symbolic () in
    let x1v = gensym_semi_symbolic () in
    let x2v = gensym_semi_symbolic () in
    let e = ExLet (d1v, e1,
      ExLet (d2v, ExFactor (x1v, d2, subst_rv e2 rv1 (ExVar x1v)),
        ExFactor (x2v, d2,
          ExSum (x1v, d1,
            ex_mul (
              ExGet (ExVar d1v, ExVar x1v),
              ExGet (ExGet (ExVar d2v, ExVar x1v), ExVar x2v)
            ))))) in
    Some (Categorical (d2, e))
  | _ -> None

(* Assumes rv1 and rv2 have Categorical distributions.
 * Returns the conditional distribution of rv1 conditioned on rv2 *)
let categorical_posterior : category random_var -> category random_var -> category distribution option =
fun rv1 rv2 ->
  let prior, likelihood = rv1.distr, rv2.distr in
  match (prior, likelihood) with
  | Categorical (d1, e1), Categorical (d2, e2) ->
    let d1v = gensym_semi_symbolic () in
    let d2v = gensym_semi_symbolic () in
    let x1v = gensym_semi_symbolic () in
    let e = ExLet (d1v, e1,
      ExLet (d2v, ExFactor (x1v, d2, subst_rv e2 rv1 (ExVar x1v)),
        ExFactor (x1v, d1,
          ex_div (
            ex_mul (
              ExGet (ExVar d1v, ExVar x1v), ExGet (ExGet (ExVar d2v, ExVar x1v), ExRand rv2)
            ),
            ExSum (x1v, d1,
              ex_mul (
                ExGet (ExVar d1v, ExVar x1v),
                ExGet (ExGet (ExVar d2v, ExVar x1v), ExRand rv2)
              )))))) in
    Some (Categorical (d1, e))
  | _ -> None

(* Swaps rv1 and rv2
 * Precondition: swapping rv1 and rv2 must not create a cyclic dependency
 * Returns: true if successful, false if swap failed due to no conjugacy *)
let swap : type a b. a random_var -> b random_var -> bool =
fun rv1 rv2 ->
  (* (Printf.printf "Swapping: parent %s, child %s\n" (rv1.name) (rv2.name)); *)
  match (rv1.distr, rv2.distr) with
  | (Normal(_, _), Normal(_, _)) ->
    (* Try to swap as gaussians *)
    begin match (gaussian_marginal rv1 rv2, gaussian_posterior rv1 rv2) with
    | Some(dist_marg), Some(dist_post) ->
      rv2.distr <- dist_marg;
      rv1.distr <- dist_post;
      true
    | _ -> false
    end
  | (Beta (_, _), Bernoulli (_)) ->
    begin match (beta_bernoulli_marginal rv1 rv2, beta_bernoulli_posterior rv1 rv2) with
    | Some(dist_marg), Some(dist_post) ->
      rv2.distr <- dist_marg;
      rv1.distr <- dist_post;
      true
    | _ -> false
    end
  | (Beta (_, _), Binomial (_)) ->
    begin match (beta_binomial_marginal rv1 rv2, beta_binomial_posterior rv1 rv2) with
    | Some(dist_marg), Some(dist_post) ->
      rv2.distr <- dist_marg;
      rv1.distr <- dist_post;
      true
    | _ -> false
    end
  | (Gamma (_), Poisson (_)) ->
    begin match (gamma_poisson_marginal rv1 rv2, gamma_poisson_posterior rv1 rv2) with
    | Some(dist_marg), Some(dist_post) ->
      rv2.distr <- dist_marg;
      rv1.distr <- dist_post;
      true
    | _ -> false
    end
  | (Gamma (_), Normal (_)) ->
    begin match (gamma_normal_marginal rv1 rv2, gamma_normal_posterior rv1 rv2) with
    | Some(dist_marg), Some(dist_post) ->
      rv2.distr <- dist_marg;
      rv1.distr <- dist_post;
      true
    | _ -> false
    end
  | (MvNormal _, MvNormal _) ->
    begin match (mv_gaussian_marginal rv1 rv2, mv_gaussian_posterior rv1 rv2) with
    | Some dist_marg, Some dist_post ->
      rv2.distr <- dist_marg;
      rv1.distr <- dist_post;
      true
    | _ -> false
    end
  | (MvNormal _, Normal (ExMatGet(mu, i), var)) ->
    begin match (eval var) with
    | ExConst (var_val) ->
      let (m, _) = mat_shape mu in
      let proj_mat =
        let ret = Mat.zeros 1 m in
        Mat.set ret 0 i 1.;
        ret
      in
      let rv2' =
       {
         name = "tmp";
         distr = MvNormal(ex_mat_mul(ExConst(proj_mat), mu), ExConst (Mat.of_arrays [| [| var_val |] |]))
       }
      in
      begin match (mv_gaussian_marginal rv1 rv2', mv_gaussian_posterior rv1 rv2') with
      | Some dist_marg, Some dist_post ->
        begin match (dist_marg, dist_post) with
        | MvNormal(mu_marg, sigma_marg), MvNormal(mu_post, sigma_post) ->
          rv2.distr <- Normal(ex_mat_get(mu_marg, 0), ex_mat_get(sigma_marg, 0));
          rv1.distr <- MvNormal(subst_rv mu_post rv2' (ex_mat_single (ExRand rv2)), subst_rv sigma_post rv2' (ex_mat_single (ExRand rv2)));
          true
        | _ -> assert false
        end
      | _ -> false
      end
    | _ -> false
    end
  | (Categorical _, Categorical _) ->
    begin match (categorical_marginal rv1 rv2, categorical_posterior rv1 rv2) with
    | Some dist_marg, Some dist_post ->
      rv2.distr <- dist_marg;
      rv1.distr <- dist_post;
      true
    | _ -> false
    end
  | (Bernoulli _, Bernoulli _) ->
    begin match (bernoulli_marginal rv1 rv2, bernoulli_posterior rv1 rv2) with
    | Some dist_marg, Some dist_post ->
      rv2.distr <- dist_marg;
      rv1.distr <- dist_post;
      true
    | _ -> false
    end
  | _ -> false (* TODO: other distributions *)

(* Returns whether rv_parent and rv_child can be swapped without creating a cycle *)
let can_swap : type a b. a random_var -> b random_var -> bool =
fun rv_parent rv_child ->
  (* Returns whether e, a subexpression of rv_child indirectly depends on rv_parent *)
  let rec has_other_deps : type c. c expr -> bool = fun e ->
    match e with
    | ExConst _ | ExVar _ -> false
    | ExRand rv ->
      let rv' : a random_var = Obj.magic rv in
      if rv' != rv_parent then
        match rv.distr with
        | Normal(mu, var) -> (depends_on mu rv_parent true) ||
                             (depends_on var rv_parent true)
        | Beta (a, b) -> (depends_on a rv_parent true) ||
                         (depends_on b rv_parent true)
        | Categorical(_, e) -> depends_on e rv_parent true
        | Bernoulli (p) -> depends_on p rv_parent true
        | Binomial (n, p) -> (depends_on n rv_parent true) || 
                             (depends_on p rv_parent true)
        | BetaBinomial (n, a, b) -> (depends_on n rv_parent true) ||
                                    (depends_on a rv_parent true) ||
                                    (depends_on b rv_parent true)
        | NegativeBinomial (n, p) -> (depends_on n rv_parent true) ||
                                     (depends_on p rv_parent true)
        | Gamma (a, b) -> (depends_on a rv_parent true) ||
                          (depends_on b rv_parent true)
        | Poisson (lambda) -> depends_on lambda rv_parent true
        | StudentT (mu, tau2, nu) -> (depends_on mu rv_parent true) ||
                                    (depends_on tau2 rv_parent true) ||
                                    (depends_on nu rv_parent true)
        | Delta e_inner -> depends_on e_inner rv_parent true
        | MvNormal (mu, var) ->
            (depends_on mu rv_parent true) || (depends_on var rv_parent true)
        | Mixture l -> List.exists (fun (e, _) -> depends_on e rv_parent true) l
        | Sampler _ -> false
      else
        false
    | ExAdd (e1, e2) -> (has_other_deps e1) || (has_other_deps e2)
    | ExMul (e1, e2) -> (has_other_deps e1) || (has_other_deps e2)
    | ExDiv (e1, e2) -> (has_other_deps e1) || (has_other_deps e2)
    | ExIntAdd (e1, e2) -> (has_other_deps e1) || (has_other_deps e2)
    | ExIntMul (e1, e2) -> (has_other_deps e1) || (has_other_deps e2)
    | ExCmp (_, e1, e2) -> (has_other_deps e1) || (has_other_deps e2)
    | ExPair (e1, e2) -> (has_other_deps e1) || (has_other_deps e2)
    | ExArray a -> Array.exists has_other_deps a
    | ExMatrix m -> Array.exists (Array.exists has_other_deps) m
    | ExList l -> List.exists has_other_deps l
    | ExIte (i, t, e) -> (has_other_deps i) || (has_other_deps t) || (has_other_deps e)
    | ExUnop(_, e_inner) -> has_other_deps e_inner
    | ExIntToFloat e_inner -> has_other_deps e_inner
    | ExMatAdd (e1, e2) | ExMatSub (e1, e2) | ExMatMul (e1, e2) ->
        (has_other_deps e1) || (has_other_deps e2)
    | ExMatScalarMul (s, e) -> (has_other_deps s) || (has_other_deps e)
    | ExMatTrans e | ExMatInv e | ExMatGet (e, _) -> has_other_deps e
    | ExMatSingle(e)  -> has_other_deps e
    | ExFactor (_, _, e) -> has_other_deps e
    | ExSum (_, _, e) -> has_other_deps e
    | ExGet (e1, e2) -> has_other_deps e1 || has_other_deps e2
    | ExLet (_, e1, e2) -> has_other_deps e1 || has_other_deps e2
  in

  match rv_child.distr with
  | Normal(mu, var) -> ((depends_on mu rv_parent false) ||
                        (depends_on var rv_parent false)) &&
                       (not (has_other_deps mu)) &&
                       (not (has_other_deps var))
  | Beta(a, b) -> ((depends_on a rv_parent false) ||
                   (depends_on b rv_parent false)) &&
                  (not (has_other_deps a)) &&
                  (not (has_other_deps b))
  | Categorical(_, e) -> (depends_on e rv_parent false) && (not (has_other_deps e))
  | Bernoulli (p) -> (depends_on p rv_parent false) &&
                     (not (has_other_deps p))
  | Binomial (n, p) -> ((depends_on n rv_parent false) ||
                        (depends_on p rv_parent false)) &&
                      (not (has_other_deps n)) &&
                      (not (has_other_deps p))
  | BetaBinomial (n, a, b) -> ((depends_on n rv_parent false) ||
                               (depends_on a rv_parent false) ||
                               (depends_on b rv_parent false)) &&
                             (not (has_other_deps n)) &&
                             (not (has_other_deps a)) &&
                             (not (has_other_deps b))
  | NegativeBinomial (n, p) -> ((depends_on n rv_parent false) ||
                                (depends_on p rv_parent false)) &&
                              (not (has_other_deps n)) &&
                              (not (has_other_deps p))
  | Gamma (a, b) -> ((depends_on a rv_parent false) ||
                     (depends_on b rv_parent false)) &&
                    (not (has_other_deps a)) &&
                    (not (has_other_deps b))
  | Poisson (lambda) -> (depends_on lambda rv_parent false) &&
                        (not (has_other_deps lambda))
  | StudentT (mu, tau2, nu) -> ((depends_on mu rv_parent false) ||
                                (depends_on tau2 rv_parent false) ||
                                (depends_on nu rv_parent false)) &&
                               (not (has_other_deps mu)) &&
                               (not (has_other_deps tau2)) &&
                               (not (has_other_deps nu))
  | Delta e_inner -> (depends_on e_inner rv_parent false) &&
                     (not (has_other_deps e_inner))
  | MvNormal (mu, var) ->
      ((depends_on mu rv_parent false) ||
      (depends_on var rv_parent false)) &&
    (not (has_other_deps mu)) &&
    (not (has_other_deps var))
  | Mixture l -> List.exists (fun (e, _) -> depends_on e rv_parent false) l
  | Sampler _ -> false

type rvwrapper = 
  | RandomVar : 'a random_var -> rvwrapper 
type rvset = rvwrapper list 
let rec member : type a. rvset -> a random_var -> bool = 
  fun rvs rv ->
    begin match rvs with
    | [] -> false
    | (RandomVar rv') :: rest -> (Obj.magic rv) == rv' || (member rest rv)
    end

let get_parents : type a. a random_var -> rvset =
  fun rv ->
    let rec get_parents_expr : type a. a expr -> rvset =
      fun e ->
        begin match e with
        | ExConst _ -> []
        | ExRand rv -> [ RandomVar rv ]
        | ExVar _ -> []
        | ExFactor (_, _, e_inner) -> get_parents_expr e_inner
        | ExGet (e1, e2) -> List.append (get_parents_expr e1) (get_parents_expr e2)
        | ExLet (_, e1, e2) -> List.append (get_parents_expr e1) (get_parents_expr e2)
        | ExSum (_, _, e_inner) -> get_parents_expr e_inner
        | ExAdd (e1, e2) -> List.append (get_parents_expr e1) (get_parents_expr e2)
        | ExMul (e1, e2) -> List.append (get_parents_expr e1) (get_parents_expr e2)
        | ExDiv (e1, e2) -> List.append (get_parents_expr e1) (get_parents_expr e2)
        | ExIntAdd (e1, e2) -> List.append (get_parents_expr e1) (get_parents_expr e2)
        | ExIntMul (e1, e2) -> List.append (get_parents_expr e1) (get_parents_expr e2)
        | ExMatAdd (e1, e2) -> List.append (get_parents_expr e1) (get_parents_expr e2)
        | ExMatSub (e1, e2) -> List.append (get_parents_expr e1) (get_parents_expr e2)
        | ExMatMul (e1, e2) -> List.append (get_parents_expr e1) (get_parents_expr e2)
        | ExMatTrans (e_inner) -> get_parents_expr e_inner
        | ExMatInv (e_inner) -> get_parents_expr e_inner
        | ExMatScalarMul (e1, e2) -> List.append (get_parents_expr e1) (get_parents_expr e2)
        | ExMatGet (e_inner, _) -> get_parents_expr e_inner
        | ExMatSingle (e_inner) -> get_parents_expr e_inner
        | ExCmp (_, e1, e2) -> List.append (get_parents_expr e1) (get_parents_expr e2)
        | ExPair (e1, e2) -> List.append (get_parents_expr e1) (get_parents_expr e2)
        | ExArray (e_inner) -> Array.fold_left (fun lst e -> List.append lst (get_parents_expr e)) [] e_inner
        | ExMatrix (e_inner) -> 
          Array.fold_left (fun lst e -> 
            Array.fold_left (fun lst e -> List.append lst (get_parents_expr e)) lst e
          ) [] e_inner

        | ExIte(i, t, e) -> List.append (List.append (get_parents_expr i) (get_parents_expr t)) (get_parents_expr e)
        | ExList (e_inner) -> List.fold_left (fun lst e -> List.append lst (get_parents_expr e)) [] e_inner
        | ExUnop (_, e_inner) -> get_parents_expr e_inner
        | ExIntToFloat e_inner -> get_parents_expr e_inner
        end
    in

    let get_parents_distr : type a. a distribution -> rvset =
      fun distr ->
        begin match distr with
        | Normal (mu, var) -> List.append (get_parents_expr mu) (get_parents_expr var)
        | MvNormal (mu, sigma) -> List.append (get_parents_expr mu) (get_parents_expr sigma)
        | Categorical(_, expr) -> get_parents_expr expr
        | Beta(a, b) -> List.append (get_parents_expr a) (get_parents_expr b)
        | Bernoulli(p) -> get_parents_expr p
        | Binomial(n, p) -> List.append (get_parents_expr n) (get_parents_expr p)
        | BetaBinomial(n, a, b) -> 
          List.append (List.append (get_parents_expr n) (get_parents_expr a)) (get_parents_expr b)
        | NegativeBinomial(n, p) -> List.append (get_parents_expr n) (get_parents_expr p)
        | Gamma(a, b) -> List.append (get_parents_expr a) (get_parents_expr b)
        | Poisson(lambda) -> get_parents_expr lambda
        | StudentT (mu, tau2, nu) -> List.append (List.append (get_parents_expr mu) (get_parents_expr tau2)) (get_parents_expr nu)
        | Delta(e) -> get_parents_expr e
        | Mixture(l) -> List.fold_left (fun lst (e, _) -> List.append lst (get_parents_expr e)) [] l
        | Sampler(_, _) -> []
        end
    in

    get_parents_distr rv.distr

let rec string_of_rvset : rvset -> string = fun rvs ->
  begin match rvs with
  | [] -> ""
  | RandomVar rv :: rest -> rv.name ^ ", " ^ (string_of_rvset rest)
  end

(* Sorts the random variables in topological order. Also deduplicates the list *)
let topo_sort : rvset -> rvset =
fun rvs ->
  let sorted_nodes = ref [] in
  let rec visit : type a. a random_var -> unit =
  fun rv ->
    let parents = get_parents rv in
    let rec visit_parents p =
      begin match p with
      | [] -> ()
      | RandomVar rv :: rst ->
        visit rv;
        visit_parents rst
      end
    in
    visit_parents parents;
    if (not (member !sorted_nodes rv)) then
      sorted_nodes := (RandomVar rv) :: !sorted_nodes
    else ();
  in

  let rec do_visits : rvset -> unit =
  fun rvs ->
    begin match rvs with
    | [] -> ()
    | RandomVar rv :: rst ->
      visit rv;
      do_visits rst
    end
  in
  do_visits rvs;

  List.filter (fun sorted_rv ->
    begin match sorted_rv with
    | RandomVar srv -> member rvs srv
    end
  ) !sorted_nodes


let hoist : type a. a random_var -> unit =
fun rv ->
  (* Hoists rv_current so it is a root, except that it may have parents
   * that are in the ghost_roots set *)
  let rec hoist_inner : type a. a random_var -> rvset -> unit =
    fun rv_current ghost_roots ->
      let parents = List.rev (topo_sort (get_parents rv_current)) in
      (* (Printf.printf "Hoisting %s with ghost roots [%s] and parents [%s]\n" rv_current.name (string_of_rvset ghost_roots) (string_of_rvset parents)); *)
      let rec hoist_parents parents ghost_roots = 
        begin match parents with
        | [] -> ()
        | (RandomVar rv_par) :: rest ->
          (* (Printf.printf "Got rv_par: %s\n" rv_par.name); *)
          ((if (not (member ghost_roots rv_par)) then
            ((*(Printf.printf "Recursing into %s\n" rv_par.name);*)
            hoist_inner rv_par ghost_roots)
          else
            ()));
          hoist_parents rest ((RandomVar rv_par) :: ghost_roots)
        end
      in
      hoist_parents parents ghost_roots;

      (* Printf.printf "Done hoisting parents for %s\n" rv_current.name;
      Printf.printf "Begin swapping child %s with parents [%s]\n" rv_current.name (string_of_rvset (List.rev parents)); *)
      
      let rec swap_with_parents parents =
        begin match parents with
        | [] -> ()
        | (RandomVar rv_par) :: rest ->
          (if (not (member ghost_roots rv_par)) then (
            (* (Printf.printf "Swapping %s with %s\n" rv_par.name rv_current.name); *)
            (if (not (can_swap rv_par rv_current)) then 
              ((Printf.printf "Cannot swap!\n");
              (*(Printf.printf "rv_par: %s, rv_par's parents: %s\n" rv_par.name (string_of_rvset (get_parents rv_par)));*)
              (*(Printf.printf "rv_current: %s, rv_current's parents: %s\n" rv_current.name (string_of_rvset (get_parents rv_current)));*)
              assert (0 = 1))
            );
            (if swap rv_par rv_current then record_approx_status rv_par.name (Exact 1)
            else raise (NonConjugate rv_par));
            swap_with_parents rest
          ) else swap_with_parents rest)
        end
      in
      swap_with_parents (List.rev parents);
      (* (Printf.printf "Done hoisting %s\n" rv_current.name) *)
  in
  hoist_inner rv []

let hoist_and_eval : type a. a random_var -> unit =
fun rv ->
  (* (Printf.printf "hoist_and_eval %s\n" rv.name); *)
  rv.distr <- (eval_distr rv.distr);
  hoist rv;
  rv.distr <- (eval_distr rv.distr)

(* Operations *)

let const v = ExConst v
let add a b = ex_add(a, b)
let mul a b = ex_mul(a, b)
let div a b = ex_div(a, b)
let exp a = ex_unop(Exp, a)
let eq a b = ex_cmp (Eq, a, b)
let lt a b = ex_cmp(Lt, a, b)
let pair a b = ExPair(a, b)
let array a = ExArray a
let matrix m = ExMatrix m
let ite i t e = ex_ite(i, t, e)
let lst l = ExList l

let int_add a b = ex_int_add(a, b)
let int_mul a b = ex_int_mul(a, b)

let mat_add a b = ex_mat_add(a, b)
let mat_scalar_mult s e = ex_mat_scalar_mul (s, e)
let mat_dot a b = ex_mat_mul(a, b)
let vec_get e i = ex_mat_get (e, i)
let int_to_float i = ex_int_to_float i

let delta v = Delta v
let gaussian mu var = Normal(mu, var)
let beta a b = Beta(a, b)
let categorical ~lower ~upper f = Categorical ({ lower; upper }, ExConst (Array.init (upper - lower + 1) (fun i -> f (i + lower)), { lower; upper }))
let bernoulli p = Bernoulli(p)
let binomial n p = Binomial(n, p)
let beta_binomial n a b = BetaBinomial(n, a, b)
let negative_binomial n p = NegativeBinomial(n, p)
let exponential l = Gamma(ExConst(1.), l)
let gamma a b = Gamma(a, b)
let poisson l = Poisson(l)
let student_t mu tau2 nu = StudentT(mu, tau2, nu)
let mv_gaussian mu var = MvNormal(mu, var)
let mixture l = Mixture(l)
let sampler f g = Sampler (f, g)

let sample n d =
    record_new_rv n;
    
    let rv = {
      name = n;
      distr = d
    } in
    (*(Printf.printf "Sampling %s from %s\n" n (string_of_rvset (get_parents rv)));*)
    ExRand rv

let intervene : type a. a random_var -> a -> unit =
fun rv x ->
  (*(Printf.printf "intervening %s\n" rv.name);*)
  rv.distr <- Delta (ExConst x)

let rec draw : type a. a random_var -> record:bool -> unit -> a =
fun rv ~record ->
  if record then record_approx_status rv.name (Approx 1);
  hoist_and_eval rv;
  fun _ ->
    match rv.distr with
    | Normal (ExConst(mu), ExConst(var)) -> Distr_operations.gaussian_draw mu var
    | Beta (ExConst(a), ExConst(b)) -> Distr_operations.beta_draw a b
    | Categorical (d, ExConst a) ->
      Distr_operations.categorical_draw
      (List.combine (List.init (d.upper - d.lower + 1) (fun i -> i + d.lower))
      (Array.to_list (fst a)))
    | Bernoulli(ExConst(p)) -> Distr_operations.bernoulli_draw p
    | Binomial(ExConst(n), ExConst(p)) -> Distr_operations.binomial_draw n p
    | BetaBinomial(ExConst(n), ExConst(a), ExConst(b)) -> Distr_operations.beta_binomial_draw n a b
    | NegativeBinomial(ExConst(n), ExConst(p)) -> Distr_operations.negative_binomial_draw n p
    | Gamma(ExConst(a), ExConst(b)) -> 
      if a = 1. then Distr_operations.exponential_draw b
      else Distr_operations.gamma_draw a b
    | Poisson(ExConst(l)) -> Distr_operations.poisson_draw l
    | StudentT(ExConst(mu), ExConst(tau2), ExConst(nu)) -> Distr_operations.student_t_draw mu tau2 nu
    | Delta (ExConst v) -> v
    | Sampler (f, _) -> f ()
    | MvNormal (ExConst mu, ExConst var) -> Distr_operations.mv_gaussian_draw mu var
    | Mixture l ->
      (* Check for cyclic dependency *)
      let cyclic = List.fold_left (fun cyclic (e, _) -> cyclic || (depends_on e rv true)) false l in
      if cyclic then raise (InternalError ("Cyclic dependency in mixture distribution"));

      let sample = Random.float 1.0 in
      let rec draw' sum l =
        match l with
        | [] -> raise (InternalError ("Invalid mixture distribution"))
        | (e, p) :: rest ->
          let sum = sum +. p in
          if sample <= sum then eval_sample e else draw' sum rest
      in
      draw' 0. l
    | _ -> raise (InternalError ("Draw did not properly hoist and evaluate random variable"))

and value : type a. a random_var -> record:bool -> a =
fun rv ~record ->
  (* (Printf.printf "approxing %s\n" rv.name); *)
  let rec do_value _ =
    try draw rv ~record:record ()
    with NonConjugate rv_nc ->
      (* (Printf.printf "NonConjugate %s\n" rv_nc.name); *)
      let _ = value rv_nc ~record in
      do_value ()
  in
  let new_val = do_value () in
  intervene rv new_val;
  new_val

and eval_sample : type a. a expr -> a =
fun e ->
  begin match e with
  | ExConst c -> c
  | ExVar _ -> assert false
  | ExRand rv -> value rv ~record:true
  | ExAdd (e1, e2) -> eval_sample e1 +. eval_sample e2
  | ExMul (e1, e2) -> eval_sample e1 *. eval_sample e2
  | ExDiv (e1, e2) -> eval_sample e1 /. eval_sample e2
  | ExIntAdd (e1, e2) -> eval_sample e1 + eval_sample e2
  | ExIntMul (e1, e2) -> eval_sample e1 * eval_sample e2
  | ExCmp (Eq, e1, e2) -> eval_sample e1 = eval_sample e2
  | ExCmp (Lt, e1, e2) -> eval_sample e1 < eval_sample e2
  | ExPair (e1, e2) -> (eval_sample e1, eval_sample e2)
  | ExArray a -> Array.map eval_sample a
  | ExMatrix m -> Array.map (Array.map eval_sample) m
  | ExList l -> List.map eval_sample l
  | ExIte (i, t, e) -> if (eval_sample i) then (eval_sample t) else (eval_sample e)
  | ExUnop (Squared, e_inner) -> (eval_sample e_inner) ** 2.
  | ExUnop (SquareRoot, e_inner) -> Float.sqrt (eval_sample e_inner)
  | ExUnop (Exp, e_inner) -> Float.exp (eval_sample e_inner)
  | ExIntToFloat e_inner -> float_of_int (eval_sample e_inner)
  | ExMatAdd (e1, e2) -> Mat.add (eval_sample e1) (eval_sample e2)
  | ExMatSub (e1, e2) -> Mat.sub (eval_sample e1) (eval_sample e2)
  | ExMatMul (e1, e2) -> Mat.mul (eval_sample e1) (eval_sample e2)
  | ExMatTrans e -> Mat.transpose (eval_sample e)
  | ExMatInv e -> Linalg.D.inv (eval_sample e)
  | ExMatScalarMul (s, e) -> Mat.scalar_mul (eval_sample s) (eval_sample e)
  | ExMatGet (e, i) -> Mat.get (eval_sample e) i 0
  | ExMatSingle(e) -> Mat.of_arrays [| [| (eval_sample e) |] |]
  | ExFactor (v, d, e) ->
    let range = Array.init (d.upper - d.lower + 1) (fun i -> i) in
    (Array.map (fun i -> eval_sample (subst e v (ExConst (i + d.lower)))) range, d)
  | ExSum (v, d, e) ->
    let range = List.init (d.upper - d.lower + 1) (fun i -> i) in
    List.fold_left (fun sum i -> sum +. eval_sample (subst e v (ExConst (i + d.lower)))) 0. range
  | ExGet (e1, e2) -> let e, d = eval_sample e1 in e.(eval_sample e2 - d.lower)
  | ExLet (v, e1, e2) -> let c = eval_sample e1 in eval_sample (subst e2 v (ExConst c))
  end

let rec score : type a. a random_var -> a -> float =
fun rv ->
  hoist_and_eval rv;
  fun x ->
    match rv.distr with
    | Normal (ExConst(mu), ExConst(var)) -> Distr_operations.gaussian_score mu var x
    | Beta (ExConst(a), ExConst(b)) -> Distr_operations.beta_score a b x
    | Categorical (d, ExConst a) ->
      Distr_operations.categorical_score
      (List.combine (List.init (d.upper - d.lower + 1) (fun i -> i + d.lower))
      (Array.to_list (fst a))) x
    | Bernoulli(ExConst(p)) -> Distr_operations.bernoulli_score p x
    | Binomial(ExConst(n), ExConst(p)) -> Distr_operations.binomial_score n p x
    | BetaBinomial(ExConst(n), ExConst(a), ExConst(b)) -> Distr_operations.beta_binomial_score n a b x
    | NegativeBinomial(ExConst(n), ExConst(p)) -> Distr_operations.negative_binomial_score n p x
    | Gamma(ExConst(a), ExConst(b)) -> 
      if a = 1. then Distr_operations.exponential_score b x
      else Distr_operations.gamma_score a b x
    | Poisson(ExConst(l)) -> Distr_operations.poisson_score l x
    | StudentT (ExConst(mu), ExConst(tau2), ExConst(nu)) -> Distr_operations.student_t_score mu tau2 nu x
    | Delta _ -> raise (InternalError ("Not implemented"))
    | Sampler (_, g) -> g x
    | MvNormal (ExConst mu, ExConst var) -> Distr_operations.mv_gaussian_score mu var x
    | Mixture l -> 
      let score' e =
        match e with
        | ExRand rv' -> Float.exp (score rv' x)
        | _ -> if eval_sample e = x then 1. else 0.
      in
      let p = List.fold_left (fun acc (e, p) -> acc +. p *. (score' e)) 0. l in
      log p
    | _ -> raise (InternalError ("Score did not properly hoist and evaluate random variable"))

let make_marginal : type a. a random_var -> unit =
fun rv -> hoist_and_eval rv

let observe_inner : type a. float -> a random_var -> a -> float =
fun w rv x ->
  (* (Printf.printf "observing %s\n" rv.name); *)
  let rec do_observe _ =
    try score rv x
    with NonConjugate rv_nc ->
      (* (Printf.printf "NonConjugate %s\n" rv_nc.name); *)
      let _ = value rv_nc ~record:true in
      do_observe ()
  in
  let s = do_observe () in
  intervene rv x;
  w +. s

let obsnum = ref 0
let get_obsnum _ =
  obsnum := !obsnum + 1;
  !obsnum

let observe : type a. float -> a distribution -> a -> float =
fun w distr x ->
  let new_rv =
    match sample ("obs" ^ (string_of_int (get_obsnum ()))) distr with
    | ExRand rv -> rv
    | _ -> raise (InternalError("Sample always returns a random variable"))
  in
  observe_inner w new_rv x