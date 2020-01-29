 (* XXXXX XXXX X    XXXX  *)
 (*   X   X    X    X  X  *)
 (*   X   XXX  X    X  X  *)
 (*   X   X    X    XXXX  *)
 (*   X   X    X    X  X  *)
 (* XXX   X    XXX  X  X  *)

let m =
  [
    [ false; true;  true;  true;  true;  true;  false; true;  true;  true;  true;  false; true;  false; false; false; false; true;  true;  true;  true;  false; ];
    [ false; false; false; true;  false; false; false; true;  false; false; false; false; true;  false; false; false; false; true;  false; false; true;  false; ];
    [ false; false; false; true;  false; false; false; true;  true;  true;  false; false; true;  false; false; false; false; true;  false; false; true;  false; ];
    [ false; false; false; true;  false; false; false; true;  false; false; false; false; true;  false; false; false; false; true;  true;  true;  true;  false; ];
    [ false; false; false; true;  false; false; false; true;  false; false; false; false; true;  false; false; false; false; true;  false; false; true;  false; ];
    [ false; true;  true;  true;  false; false; false; true;  false; false; false; false; true;  true;  true;  false; false; true;  false; false; true;  false; ];
  ]

let rec t m =
  begin match m with
  | [] -> []
  | [ l ] -> List.map (fun x -> [x]) l
  | l1 :: m ->
      begin match t m with
      | [] -> assert false
      | l2 -> (List.map2 (fun x y -> y @ [x]) l1 l2)
      end
  end

let tm =
  t m

let _ = List.length m;;
