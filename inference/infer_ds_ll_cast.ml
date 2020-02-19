module Naive = Infer_ds_ll
open Infer_ds_ll_gc
open Ds_distribution

let rec copy_node : type a b.
  (int, Obj.t) Hashtbl.t -> (a, b) ds_node -> (a, b) ds_node =
  fun tbl n  ->
  begin match Hashtbl.find_opt tbl n.ds_node_id with
  | None ->
      let state =
        begin match n.ds_node_state with
        | Realized x -> Realized x
        | Marginalized (mdistr, None) -> Marginalized (mdistr, None)
        | Marginalized (mdistr, Some (c, cdistr)) ->
            assert begin match c.ds_node_state with
              | Initialized _ -> false
              | Marginalized _ | Realized _ -> true
            end;
            let c_copy = copy_node tbl c in
            Marginalized (mdistr, Some (c_copy, cdistr))
        | Initialized (p, cdistr) ->
            assert begin match p.ds_node_state with
              | Marginalized (_, Some (c, _)) ->
                  c.ds_node_id <> n.ds_node_id
              | Initialized _ | Marginalized (_, None) | Realized _ ->
                  true
            end;
            let p_copy = copy_node tbl p in
            Initialized (p_copy, cdistr)
        end
      in
      let n =
        { ds_node_id = n.ds_node_id;
          ds_node_state = state }
      in
      Hashtbl.add tbl n.ds_node_id (Obj.repr n);
      n
  | Some o -> (Obj.obj o: (a, b) ds_node)
  end

let rec copy_cast_node : type p a.
  (int, Obj.t) Hashtbl.t -> (p, a) Naive.ds_node ->  (p, a) ds_node = 
  fun tbl n -> 
  begin match Hashtbl.find_opt tbl n.Naive.ds_node_id with
  | None -> 
      let state =
        begin match n.Naive.ds_node_state with
        | Naive.Realized x -> Realized x
        | Naive.Marginalized(mdistr, None) -> Marginalized(mdistr, None) 
        | Naive.Marginalized(mdistr, Some _) ->
            begin match Naive.marginal_child n with
            | None -> 
                Marginalized(mdistr, None)
            | Some(Child(c)) ->
                begin match c.ds_node_state with
                | Naive.Marginalized(_, Some(p, cdistr)) ->
                    assert (p.ds_node_id = n.ds_node_id);
                    let c = 
                      copy_cast_node tbl (Obj.magic c: (a, _) Naive.ds_node) 
                    in
                    Marginalized(mdistr, Some(c, cdistr))
                | Naive.Marginalized(_, None) 
                | Realized _
                | Initialized _ -> assert false
                end
            end
        | Naive.Initialized(p, cdistr) -> 
            let p = copy_cast_node tbl p in
            Initialized(p, cdistr)
        end 
      in
      let n = 
        { ds_node_id = n.ds_node_id; 
          ds_node_state = state } 
      in
      Hashtbl.add tbl n.ds_node_id (Obj.repr n); 
      n
  | Some o -> (Obj.obj o: (p, a) ds_node)
  end