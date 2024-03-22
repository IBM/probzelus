Py.initialize ()

let gym = Py.import "gymnasium"

let make ?(render = "human") game =
  Py.Module.get_function_with_keywords gym "make"
    [| Py.String.of_string game |]
    [ ("render_mode", Py.String.of_string render) ]

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
