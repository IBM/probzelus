module T = Tqdm.Tqdm

let _ = 
  T.with_bar 100 
  ~f:(fun tqdm -> 
        while true do
          () 
          |> read_line 
          |> float_of_string 
          |> ( *. ) 10. 
          |> int_of_float 
          |> T.update tqdm
        done);