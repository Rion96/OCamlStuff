let rev s = (
  let rec inner input output = (
    if (input < 0) then (
      output
    ) else (
      inner (input - 1) (output ^ ((String.make 1 s.[input])))
    )
  ) in
  inner ((String.length s) - 1) ""
)

let _ = (
  let rec loop () = (
    let line = read_line () in
    if (String.length line) > 0 then (
      Printf.printf "%s\n" (rev line);
      loop ();
    )
  ) in
  loop ()
)