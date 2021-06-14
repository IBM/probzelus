module T = Tqdm.Tqdm

let _ = 
  T.with_bar 100 
  ~f:(fun tqdm -> 
        while true do
          let v = int_of_string (read_line ()) in
          T.update tqdm v
        done);