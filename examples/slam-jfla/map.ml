 (* XXXXX XXXX X    XXXX  *)
 (*   X   X    X    X  X  *)
 (*   X   XXX  X    X  X  *)
 (*   X   X    X    XXXX  *)
 (*   X   X    X    X  X  *)
 (* XXX   X    XXX  X  X  *)

let jfla =
  [
    [ false; true;  true;  true;  true;  true;  false; true;  true;  true;  true;  false; true;  false; false; false; false; true;  true;  true;  true;  false; ];
    [ false; false; false; true;  false; false; false; true;  false; false; false; false; true;  false; false; false; false; true;  false; false; true;  false; ];
    [ false; false; false; true;  false; false; false; true;  true;  true;  false; false; true;  false; false; false; false; true;  false; false; true;  false; ];
    [ false; false; false; true;  false; false; false; true;  false; false; false; false; true;  false; false; false; false; true;  true;  true;  true;  false; ];
    [ false; false; false; true;  false; false; false; true;  false; false; false; false; true;  false; false; false; false; true;  false; false; true;  false; ];
    [ false; true;  true;  true;  false; false; false; true;  false; false; false; false; true;  true;  true;  false; false; true;  false; false; true;  false; ];
  ]


 (* XXXX X   XXX  XXX *)
 (* X  X X   X  X  X  *)
 (* XXXX X   X  X  X  *)
 (* X    X   X  X  X  *)
 (* X    X   X  X  X  *)
 (* X    XXX XXX  XXX *)

let pldi =
  [
    [ false; true; true; true; true; false; true; false; false; false; true; true; true; false; false; true; true; true; false; ];
    [ false; true; false; false; true; false; true; false; false; false; true; false; false; true; false; false; true; false; false; ];
    [ false; true; true; true; true; false; true; false; false; false; true; false; false; true; false; false; true; false; false; ];
    [ false; true; false; false; false; false; true; false; false; false; true; false; false; true; false; false; true; false; false; ];
    [ false; true; false; false; false; false; true; false; false; false; true; false; false; true; false; false; true; false; false; ];
    [ false; true; false; false; false; false; true; true; true; false; true; true; true; false; false; true; true; true; false; ];
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
  t pldi

let _ = List.length tm;;
