#require "yojson"

open Yojson.Basic

let get_station (j : json) (s : string) = (
  Util.member s j
  |> Util.to_string_option
)

let add_station (j : json) (name : string) (url : string) = (
  let value = "{\"" ^ name ^ "\": \"" ^ url ^ "\"}" in
  Util.combine j (from_string value)
)

let print_stations (j : json) = (
  let rec inner l index = (
    match l with
    | hd :: tl when index < 4 -> (
      Printf.printf "%s\t" hd;
      inner tl (index + 1)
    )
    | hd :: tl -> (
      Printf.printf "%s\n" hd;
      inner tl 0
    )
    | [] -> (
      Printf.printf "\n"
    )
  ) in
  inner (Util.keys j) 0
)

let _ = (
  let j = from_file "stations.json" in
  let cmd = "mpv --vid=no --shuffle " in
  let rec inner () = (
    print_stations j;
    print_string "Please enter a station: ";
    let station_opt = get_station j (read_line ()) in
    match station_opt with
    | Some s -> (
      Sys.chdir (Sys.getenv "HOME");
      Sys.command (cmd ^ "\"" ^ s ^ "\"") |> (fun x -> ());
      inner ()
    )
    | None -> (
      print_endline "Goodbye!"
    )
  ) in
  inner ()
)