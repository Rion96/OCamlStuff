let mat_vec dim mat vec = (
  let out = Array.create_float dim in
  let rec fori i = (
    if i < dim then (
      out.(i) <- 0.;
      let rec forj j = (
        if j < dim then (
          out.(i) <- out.(i) +. (mat.(i).(j) *. vec.(j));
          forj (j + 1)
        )
      ) in
      forj 0;
      Printf.printf "%f\n" out.(i);
      fori (i + 1)
    )
  ) in
  fori 0;
  out
)

let _ = (
  let bound = 10. in
  let dim = 3 in
  let mat = Array.make_matrix dim dim 0. in
  let vec = Array.create_float dim in
  Random.self_init ();
  let rec fori i = (
    if i < dim then (
      let rec forj j = (
        if j < dim then (
          mat.(i).(j) <- Random.float bound;
          Printf.printf "%f " mat.(i).(j);
          forj (j + 1)
        )
        else (
          Printf.printf "| %f\n" vec.(i)
        )
      ) in
      vec.(i) <- Random.float bound;
      forj 0;
      fori (i + 1)
    )
  ) in
  fori 0;
  print_endline "=======================================";
  mat_vec dim mat vec
)