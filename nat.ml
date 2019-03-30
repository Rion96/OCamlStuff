module type Nat =
  sig
    type nat
    val compare : nat -> nat -> int
    val of_int : int -> nat
    val to_int : nat -> int
    val to_string : nat -> string
    val add : nat -> nat -> nat
    val sub : nat -> nat -> nat
  end

module Nat =
  struct
    type nat = Null | Suc of nat

    let rec compare a b =
      match a, b with
      | Null , Null  -> 0
      | Null , Suc _ -> -1
      | Suc _, Null  -> 1
      | Suc a, Suc b -> compare a b

    let to_int a =
      let rec iter buffer =
        function
        | Null  -> buffer
        | Suc a -> iter (buffer + 1) a in
      iter 0 a
    
    let to_string a =
      to_int a |> string_of_int

    let of_int i =
      let rec iter buffer i =
        if i > 0 
        then iter (Suc buffer) (i - 1)
        else buffer in
      iter Null i

    let rec add a b =
      match a, b with
      | Null , b -> b
      | Suc a, b -> Suc (add a b)

    let rec sub a b =
      match a, b with
      | Null , _     -> Null
      | a    , Null  -> a
      | Suc a, Suc b -> sub a b
  end


open Nat

let () =
  of_int 123
  |> to_string
  |> print_endline;

  compare (of_int 123) (of_int 456)
  |> print_int |> print_newline;

  add (of_int 123) (of_int 456)
  |> to_string |> print_endline;

  sub (of_int 123) (of_int 456)
  |> to_string |> print_endline;

  sub (of_int 456) (of_int 123)
  |> to_string |> print_endline


