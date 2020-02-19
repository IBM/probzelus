module Naive = Infer_ds_ll
open Infer_ds_ll_gc
open Ds_distribution

let rec copy_cast : type p a.
  (int, Obj.t) Hashtbl.t ->
  (p, a) Naive.ds_node -> 
  (p, a) ds_node = 
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
                    let c = copy_cast tbl (Obj.magic c: (a, _) Naive.ds_node) in
                    Marginalized(mdistr, Some(c, cdistr))
                | Naive.Marginalized(_, None) 
                | Realized _
                | Initialized _ -> assert false
                end
            end
        | Naive.Initialized(p, cdistr) -> 
            let p = copy_cast tbl p in
            Initialized(p, cdistr)
        end 
      in
      let n = {ds_node_id = n.ds_node_id; ds_node_state = state} in
      Hashtbl.add tbl n.ds_node_id (Obj.repr n); 
      n
  | Some o -> (Obj.obj o: (p, a) ds_node)
  end