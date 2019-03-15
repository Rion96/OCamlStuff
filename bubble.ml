let print_list l = (
  Printf.printf "[";
  let rec inner input = (
    match input with
    | [hd] -> (
      Printf.printf "%d]\n" hd
    )
    | hd :: tl -> (
      Printf.printf "%d, " hd;
      inner tl
    )
    | [] -> (
      Printf.printf "]\n"
    )
  ) in
  inner l
)

let bubble_sort l = (
  let rec valid input = (
    match input with
    | a :: b :: tl -> (
      if a > b then
        false
      else
        valid (b :: tl)
    )
    | _ -> true
  ) in
  let rec bubble input output = (
    match input with
    | a :: b :: tl when a > b -> (
      bubble (a :: tl) (b :: output)
    )
    | a :: b :: tl -> (
      bubble (b :: tl) (a :: output)
    )
    | [a] -> (
      List.rev (a :: output)
    )
    | _ -> List.rev output
  ) in
  let rec loop input = (
    let output = bubble input [] in
      print_list output;
      if valid output then
        output
      else
        loop output
  ) in
  if List.length l > 0 then (
    loop l
  )
  else (
    l
  )
)

let parse_input s = (
  let rec inner input output = (
    if input < 0 then (
      output
    )
    else (
      let c = s.[input] in
      if c >= '0' && c <= '9' then (
        ((Char.code c) - (Char.code '0')) :: output
      )
      else (
        inner (input - 1) output
      )
    )
  ) in
  inner ((String.length s) - 1) []
)

let () = (
  print_endline "Please enter a sequence of numbers to be sorted (enter a non-number character to stop input)";
  print_endline "Example: 1 2 3 x";
  let rec parse output = (
    try (
      Scanf.scanf " %d " (fun d -> parse (d :: output))
    ) with exn -> output
  ) in
  parse []
  |> List.rev
  |> bubble_sort
  |> print_list
)