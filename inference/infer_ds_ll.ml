(** Inference with delayed sampling low level interface *)

type pstate = Infer.pstate

type mgaussiant = float
type mbetat = float
type mbernoullit = bool
type 'a mtype = 'a


type marginal_t =
  | MGaussianT
  | MBetaT
  | MBernoulliT


(** Marginalized distribution *)
type 'a mdistr =
  | MGaussian : float * float -> mgaussiant mdistr
  | MBeta : float * float -> mbetat mdistr
  | MBernoulli : float -> bool mdistr

(** Conditionned distribution *)
type ('m1, 'm2) cdistr =
  | AffineMeanGaussian: float * float * float -> (mgaussiant, mgaussiant) cdistr
  | CBernoulli : (mbetat, mbernoullit) cdistr

(** Random variable *)
type ('a, 'b) rv =
  { name : string;
    mutable children : 'b rv_from list;
    mutable state : 'b rv_state;
    mutable distr : ('a, 'b) dsdistr;
  }

and 'a rv_state =
  | Initialized
  | Marginalized of 'a mdistr
  | Realized of 'a mtype

and ('a, 'b) dsdistr =
  | UDistr :  'b mdistr -> ('a, 'b) dsdistr
  | CDistr : ('z, 'm1) rv * ('m1, 'm2) cdistr -> ('m1, 'm2) dsdistr


and 'b rv_from =
  RV_from : ('b, 'c) rv -> 'b rv_from


let factor = Infer.factor


(* typeOfMDistr :: MDistr b -> SMarginalT b *)
let type_of_mdistr (type a): a mdistr -> marginal_t =
  fun mdistr ->
  begin match mdistr with
    | MGaussian _ -> MGaussianT
    | MBeta _ -> MBetaT
    | MBernoulli _ -> MBernoulliT
  end

let type_of_cdistr (type a b): (a, b) cdistr -> marginal_t =
  fun  cdist ->
  begin match cdist with
    | AffineMeanGaussian _ -> MGaussianT
    | CBernoulli -> MBernoulliT
  end

;;

let type_of_dsdistr (type a) (type b): (a, b) dsdistr -> marginal_t =
  fun dsdistr ->
  begin match dsdistr with
  | UDistr d -> type_of_mdistr d
  | CDistr (_, d) -> type_of_cdistr d
  end



let mdistr_to_distr (type a): a mdistr -> a Distribution.t = fun mdistr ->
  begin match mdistr with
    | MGaussian (mu, var) -> Distribution.gaussian mu (sqrt var)
    | MBeta (alpha, beta) -> Distribution.beta alpha beta
    | MBernoulli p -> Distribution.bernoulli p
  end

let cdistr_to_mdistr (type m) (type m'):
  (m, m') cdistr -> m mtype -> m' mdistr =
  fun cdistr obs ->
  begin match cdistr with
    | AffineMeanGaussian (m, b, obsvar) ->
        MGaussian (m *. obs +. b, obsvar)
    | CBernoulli ->
        MBernoulli obs
  end


let mgaussian mu var = MGaussian (mu, var)
let mbeta alpha beta = MBeta (alpha, beta)
let mbernoulli theta = MBernoulli theta

let affine_mean_gaussian m b var = AffineMeanGaussian (m, b, var)

let gaussian_mean_gaussian: float -> (mgaussiant, mgaussiant) cdistr =
  fun x ->
  AffineMeanGaussian (1., 0., x)

let make_marginal (type a) (type b): a mdistr -> (a, b) cdistr -> b mdistr =
  fun mdistr cdistr ->
  begin match mdistr, cdistr with
    | MGaussian (mu, var), AffineMeanGaussian(m, b, obsvar) ->
        MGaussian (m *. mu +. b, m ** 2. *. var +. obsvar)
    | MBeta (a, b),  CBernoulli ->
        MBernoulli (a /. (a +. b))
    | _ -> assert false (* error "impossible" *)
  end

let make_conditional (type a) (type b):
  a mdistr -> (a, b) cdistr -> b mtype -> a mdistr =
  let gaussian_conditioning mu var obs obsvar =
    let ivar = 1. /. var in
    let iobsvar = 1. /. obsvar in
    let inf = ivar +. iobsvar in
    let var' = 1. /. inf in
    let mu' = (ivar *. mu +. iobsvar *. obs) /. inf in
    (mu', var')
  in
  fun mdistr cdistr obs ->
  begin match mdistr, cdistr with
    | MGaussian(mu, var), AffineMeanGaussian(m, b, obsvar) ->
        let (mu', var') =
          gaussian_conditioning mu var ((obs -. b) /. m) (obsvar /. m ** 2.)
        in
        MGaussian(mu', var')
    | MBeta (a, b),  CBernoulli ->
        if obs then MBeta (a +. 1., b) else MBeta (a, b +. 1.)
    | _, _ -> assert false (* error "impossible" *)
  end


(* initialize without parent node *)
let assume_constant (type a) (type z): string -> a mdistr -> (z, a) rv =
  fun n d ->
  { name = n;
    children = [];
    state = Marginalized d;
    distr = UDistr d; }

(* initialize with parent node *)
let assume_conditional (type a) (type b) (type c):
  string -> (a,b) rv -> (b, c) cdistr -> (b, c) rv =
  fun str par cdistr ->
  let child =
    { name = str;
      children = [];
      state = Initialized;
      distr = CDistr (par, cdistr); }
  in
  par.children <- RV_from child :: par.children;
  child


let marginalize (type a) (type b): (a, b) rv -> unit =
  fun n ->
  (* Format.eprintf "marginalize: %s@." n.name; *)
  begin match n.state, n.distr with
    | Initialized, CDistr (par, cdistr) ->
        let marg =
          begin match par.state with
            | Realized x ->
                cdistr_to_mdistr cdistr x
            | Marginalized par_marginal ->
                make_marginal par_marginal cdistr
            | Initialized -> assert false (* error "marginalize'" *)
          end
        in
        n.state <- Marginalized marg
    | state, _ ->
        Format.eprintf "Error: marginalize %s@." n.name;
        assert false
  end

let rec delete :
  'a 'b. ('a, 'b) rv -> 'a rv_from list -> 'a rv_from list =
  fun n l ->
  begin match l with
    | [] -> assert false
    | RV_from x :: l ->
        if Obj.repr x == Obj.repr n then l
        else RV_from x :: (delete n l)
  end

let realize (type a) (type b): b mtype -> (a, b) rv -> unit =
  fun val_ n ->
  (* Format.eprintf "realize: %s@." n.name; *)
  (* ioAssert (isTerminal n) *)
  begin match n.distr with
    | UDistr _ -> ()
    | CDistr (p, cdistr) ->
        begin match p.state with
        | Marginalized marg ->
            p.state <- Marginalized (make_conditional marg cdistr val_);
            p.children <- delete n p.children
        | _ -> assert false
        end
  end;
  List.iter (fun (RV_from c) -> marginalize c) n.children;
  n.state <- Realized val_;
  n.children <- []


let sample (type a) (type b) : (a, b) rv -> unit =
  fun n ->
  (* Format.eprintf "sample: %s@." n.name; *)
  (* ioAssert (isTerminal n) *)
  begin match n.state with
    | Marginalized m ->
        let x = Distribution.draw (mdistr_to_distr m) in
        realize x n
    | _ -> assert false (* error "sample" *)
  end

let observe (type a) (type b): pstate -> b mtype -> (a, b) rv -> unit =
  fun prob x n ->
  (* io $ ioAssert (isTerminal n) *)
  begin match n.state with
    | Marginalized marg ->
        factor (prob, Distribution.score (mdistr_to_distr marg) x);
        realize x n
    | _ -> assert false (* error "observe'" *)
  end

let is_marginalized state =
  begin match state with
  | Marginalized _ -> true
  | _ -> false
  end

(* Invariant 2: A node always has at most one marginal Child *)
let marginal_child (type a) (type b): (a, b) rv -> b rv_from option =
  fun n ->
  List.find_opt
    (fun (RV_from x) -> is_marginalized x.state)
    n.children

let rec prune : 'a 'b. ('a, 'b) rv -> unit = function n ->
  (* Format.eprintf "prune: %s@." n.name; *)
  (* assert (isMarginalized (state n)) $ do *)
  begin match marginal_child n with
    | Some (RV_from child) ->
        prune child
    | None ->
        ()
  end;
  sample n

let rec graft : 'a 'b. ('a, 'b) rv -> unit = function n ->
  (* Format.eprintf "graft %s@." n.name; *)
  begin match n.state with
  | Marginalized _ ->
      begin match marginal_child n with
        | Some (RV_from child) -> prune child
        | None -> ()
      end
  | _ ->
      begin match n.distr with
        | UDistr _ -> assert false (* error "graft" *)
        | CDistr (par, cdistr) ->
            graft par;
            marginalize n
      end
  end

let obs (type a) (type b): pstate -> b mtype -> (a, b) rv -> unit =
  fun prob x n ->
  graft n;
  observe prob x n

let rec get_value: 'a 'b. ('a, 'b) rv -> 'b mtype =
  fun n ->
  begin match n.state with
    | Realized x -> x
    | _ ->
        graft n;
        sample n;
        get_value n
  end

(* forget' :: IORef (Node a b) -> IO () *)
let forget (type a) (type b): (a, b) rv -> unit =
  fun n ->
  (* Format.eprintf "forget: %s@." n.name; *)
  begin match n.state with
    | Realized _ -> ()
    | Initialized -> assert false (* error "forget" *)
    | Marginalized marg ->
        List.iter
          (fun (RV_from c) ->
             begin match c.distr with
               | UDistr d -> ()
               | CDistr (cdistr, par) ->
                   begin match c.state with
                     | Marginalized marg -> c.distr <- UDistr marg
                     | _ -> assert false (* error "forget" *)
                   end
             end)
          n.children;
        begin match n.distr with
          | UDistr _ -> ()
          | CDistr (cdistr, par) ->
              assert false (* error "forget: Shouldn't have parents?" *)
        end;
        n.distr <- UDistr marg
  end;
  n.children <- []


let print_state n =   (* XXX TODO XXX *)
  Format.printf "%s: " n.name;
  begin match n.state with
  | Initialized -> Format.printf "Initialized"
  | Marginalized (MGaussian (mu, var)) ->
      Format.printf "Marginalized (MGaussian (%f, %f))" mu var
  | Marginalized _ -> Format.printf "Marginalized"
  | Realized x -> Format.printf "Realized %f" x
  end;
  Format.printf "@."

let observe_conditional (type a) (type b) (type c):
  pstate -> string -> (a, b) rv -> (b, c) cdistr -> c mtype -> unit =
  fun prob str n cdistr observation ->
  let y = assume_conditional str n cdistr in
  obs prob observation y

let infer = Infer.infer


(* ----------------------------------------------------------------------- *)
(* Examples *)

(* let delay_triplet zobs = *)
(*   let prob = { score = 0. } in *)
(*   let x = assume_constant "x" (MGaussian (0., 1.)) in *)
(*   Format.printf "x created@."; *)
(*   let y = assume_conditional "y"  x (gaussian_mean_gaussian 1.) in *)
(*   Format.printf "y created@."; *)
(*   let z = assume_conditional "z" y (gaussian_mean_gaussian 1.) in *)
(*   Format.printf "z created@."; *)
(*   obs prob zobs z; *)
(*   Format.printf "z observed@."; *)
(*   Format.printf "%f@." (get_value z); *)
(*   Format.printf "%f@." (get_value x); *)
(*   Format.printf "%f@." (get_value y) *)

(* let () = *)
(*   Random.self_init (); *)
(*   delay_triplet 42. *)

(* let obs_sample xobs = *)
(*   let prob = { Infer.scores = [| 0. |]; idx = 0 } in *)
(*   let x = assume_constant "x" (MGaussian (0., 1.)) in *)
(*   Format.printf "x created@."; *)
(*   obs prob xobs x; *)
(*   Format.printf "x observed@."; *)
(*   sample x; *)
(*   Format.printf "x sampled@."; *)
(*   Format.printf "%f@." (get_value x) *)

(* let () = *)
(*   Random.self_init (); *)
(*   obs_sample 42. *)