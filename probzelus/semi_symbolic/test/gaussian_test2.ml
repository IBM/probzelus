open Semi_symbolic.Semi_symbolic_impl
(* open Test_lib *)

let infer rv =
  begin match rv with
  | ExRand rv_inner ->
    hoist_and_eval rv_inner;
    begin match rv_inner.distr with
    | Normal(ExConst mu, ExConst var) ->
      Printf.printf "mu: %f, var: %f\n" mu var
    | _ -> assert false
    end
  | _ -> assert false
  end

let print_affine_const e_par e_child =
  begin match (e_par, e_child) with
  | (ExRand rv_par, ExRand rv_child) ->
    begin match rv_child.distr with
    | Normal (mu, var) ->
      begin match is_affine mu rv_par with
      | Some (a, b) ->
        begin match (eval a, eval b, eval var) with
        | (ExConst a_v, ExConst b_v, ExConst var_v) -> Printf.printf "affine a = %f, b = %f -- variance = %f\n" a_v b_v var_v
        | _ -> assert false
        end
      | _ -> assert false
      end;
      begin match is_affine (eval mu) rv_par with
      | Some (a, b) ->
        begin match (eval a, eval b) with
        | (ExConst a_v, ExConst b_v) ->
          Printf.printf "after eval affine a = %f, b = %f\n" a_v b_v
        | _ -> assert false
        end
      | _ -> assert false
      end
    | _ -> assert false
    end
  | _ -> assert false
  end

let x0 = sample "x0" (gaussian (const 0.) (const 2500.))
let y0 = gaussian x0 (const 1.)

let x1 = sample "x1" (gaussian x0 (const 1.))
let y1 = (gaussian x1 (const 1.))

let x2 = sample "x2" (gaussian x1 (const 1.))
let y2 = gaussian x2 (const 1.);;

Printf.printf "y0 -- ";;
infer (sample "y0" y0);;

observe 0. y0 (-. 45.);;

Printf.printf "x1: ";;
print_affine_const x0 x1;;

Printf.printf "x0 -- ";;
infer x0;;

Printf.printf "x1: ";;
print_affine_const x0 x1;;

Printf.printf "x1 -- ";;
infer x1;;

Printf.printf "x0: ";;
print_affine_const x1 x0;;

Printf.printf "x2 -- ";;
infer x2;;

Printf.printf "x1 -- ";;
infer x1;;

Printf.printf "x0 -- ";;
infer x0;;

Printf.printf "x1: ";;
print_affine_const x0 x1;;

Printf.printf "x1 -- ";;
infer x1;;

Printf.printf "x0: ";;
print_affine_const x1 x0;;

Printf.printf "x0 -- ";;
infer x0;;

Printf.printf "x1: ";;
print_affine_const x0 x1;;

Printf.printf "y1 -- ";;
infer (sample "y1" y1);;

let x3 = sample "x3" (gaussian x2 (const 1.))
let y3 = gaussian x3 (const 1.);;

Printf.printf "y1 -- ";;
infer (sample "y1" y1);;

observe 0. y1 (-. 44.);;
Printf.printf "x3 -- ";;
infer x3;;


