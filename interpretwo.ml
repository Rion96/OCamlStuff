(* A nicer interpreter. In my opinion. *)

type token =
  | NAME of string
  | INT of int
  | STR of string
  | LET
  | PRINT
  | NEWLINE
  | ERROR
  | PLUS

exception InvalidToken

let interpreter (tokens : token list) = (
  let rec get_stack (stack : token list) = (
    function
    | NEWLINE :: tl -> (
      stack, tl
    )
    | ERROR :: tl -> (
      print_endline "Error at get_stack!";
      raise InvalidToken
    )
    | hd :: tl -> (
      get_stack (hd :: stack) tl
    )
    | [] -> (
      stack, []
    )
  ) in
  let rec get_token (vars : (string * token) list) = (
    function
    | a :: PLUS :: b :: tl -> (
      let a, b = get_token vars [a], get_token vars [b] in
      let hd = (
        match a, b with
        | INT a, INT b -> (
          INT (a + b)
        )
        | _ -> (
          ERROR
        )
      ) in
      get_token vars (hd :: tl)
    )
    | [INT i] -> (
      INT i
    )
    | [STR s] -> (
      STR s
    )
    | [NAME n] -> (
      List.assoc n vars
    )
    | _ -> (
      ERROR
    )
  ) in
  let printer = (
    function
    | INT i -> (
      print_int i;
      print_newline ()
    )
    | STR s -> (
      print_endline s
    )
    | _ -> (
      print_endline "Error at printer!";
      raise InvalidToken
    )
  ) in
  let rec iter (vars : (string * token) list) = (
    function
    | PRINT :: tl -> (
      let stack, tl = get_stack [] tl in
      printer (get_token vars stack);
      iter vars tl
    )
    | LET :: NAME n :: tl -> (
      let stack, tl = get_stack [] tl in
      iter ((n, (get_token vars stack)) :: (List.remove_assoc n vars)) tl
    )
    | NEWLINE :: tl -> (
      iter vars tl
    )
    | [] -> (

    )
    | _ -> (
      print_endline "Error at iter!";
      raise InvalidToken
    )
  ) in
  iter [] tokens
)

let tokenizer (stack : char list) = (
  let rec ignore_token = (
    function
    | NEWLINE :: stack -> (
      NEWLINE :: stack
    )
    | hd :: tl -> (
      ignore_token tl
    )
    | [] -> (
      []
    )
  ) in
  let rec int_token (buffer : string) = (
    function
    | hd :: tl when hd >= '0' && hd <= '9' -> (
      int_token ((String.make 1 hd) ^ buffer) tl
    )
    | stack -> (
      INT (int_of_string buffer), stack
    )
  ) in
  let rec str_token (buffer : string) = (
    function
    | '"' :: stack -> (
      STR buffer, stack
    )
    | hd :: tl -> (
      str_token ((String.make 1 hd) ^ buffer) tl
    )
    | [] -> (
      ERROR, []
    )
  ) in
  let rec name_token (buffer : string) = (
    function
    | ' ' :: stack -> (
      NAME buffer, stack
    )
    | hd :: tl when hd >= 'A' && hd <= 'Z' -> (
      name_token ((String.make 1 hd) ^ buffer) tl
    )
    | _ :: stack -> (
      ERROR, stack
    )
    | [] -> (
      ERROR, []
    )
  ) in
  let rec main_parser (buffer : token list) = (
    function
    | '\n' :: stack -> (
      main_parser (NEWLINE :: buffer) stack
    )
    | '+' :: stack -> (
      main_parser (PLUS :: buffer) stack
    )
    | '#' :: stack -> (
      main_parser (ignore_token buffer) stack
    )
    | ' ' :: stack -> (
      main_parser buffer stack
    )
    | '"' :: stack -> (
      let tok, stack = str_token "" stack in
      main_parser (tok :: buffer) stack
    )
    | 'P' :: tl -> (
      main_parser (PRINT :: buffer) tl
    )
    | 'T' :: 'E' :: 'L' :: tl -> (
      main_parser (LET :: buffer) tl
    )
    | c :: stack when c >= '0' && c <= '9' -> (
      let tok, stack = int_token "" (c :: stack) in
      main_parser (tok :: buffer) stack
    )
    | c :: stack -> (
      let tok, stack = name_token "" (c :: stack) in
      main_parser (tok :: buffer) stack
    )
    | [] -> (
      buffer
    )
  ) in
  main_parser [] stack
)

let char_stack (f : in_channel) = (
  let rec loop stack = (
    try (
      let c = input_char f in
      loop (c :: stack)
    ) with End_of_file -> (
      stack
    )
  ) in
  loop []
)

let _ = (
  open_in "test"
  |> char_stack
  |> tokenizer
  |> interpreter
)