(** This type is used to define the render mode in gymnasium. 
    Human render does not allow multi-windows. Use Human_multi instead (less efficient) *)
type render = Human | Human_multi | Ascii | Rgb_array

let () =
  Py.add_python_path
    "/home/victor/Documents/Info_et_maths/probzelus/examples/gymnasium"

let () = Py.initialize ()
let gym = Py.import "gymnasium"
let render_multi = Py.import "rendermulti"

let render_to_str (render : render) : string =
  match render with
  | Human -> "human"
  | Ascii -> "ascii"
  | Rgb_array | Human_multi -> "rgb_array"

(** Create a fresh windows name *)
let fresh_name : string -> string =
  let j = ref 0 in
  function
  | name ->
      j := !j + 1;
      String.concat "" [ name; string_of_int !j ]

(** Create a new gym environment, using by default a human render. *)
let make ~render game =
  print_string @@ render_to_str render;
  let env =
    Py.Module.get_function_with_keywords gym "make"
      [| Py.String.of_string game |]
      [ ("render_mode", Py.String.of_string @@ render_to_str render) ]
  in
  match render with
  | Human | Ascii | Rgb_array -> env
  | Human_multi ->
      Py.Module.get_function_with_keywords render_multi "RenderMultiHuman"
        [| env; Py.String.of_string @@ fresh_name game |]
        []

let reset env = Py.Object.call_method env "reset" [||]

let step env action =
  let next = Py.Object.call_method env "step" [| action |] in
  let observation, reward, terminated, truncated, info =
    Py.Tuple.to_tuple5 next
  in
  ( observation,
    reward,
    Py.Bool.to_bool terminated,
    Py.Bool.to_bool truncated,
    info )

let close env = Py.Object.call_method env "close" [||]

let sample_action env =
  let action_space = Py.Object.find_attr_string env "action_space" in
  Py.Object.call_method action_space "sample" [||]
