(* Attempt at an interpreter. Kinda works. *)
module Interpreter = struct
  open Printf
  type expr = 
    | String of string
    | Int of int
    | Var of string
    | Add of (expr * expr)

  type stmt =
    | Print of expr
    | Decl of (string * expr)

  type decls = {
    mutable functions : (string * stmt) list;
    mutable vars : (string * expr) list;
  }
  
  exception InvalidInt
  exception InvalidStr
  exception InvalidExpr
  exception InvalidStmt
  exception InvalidDecl
  exception InvalidAdd

  let char_list_to_string l = (
    let rec iter input output = (
      match input with
      | c :: tl -> (
        iter tl (output ^ (String.make 1 c))
      )
      | [] -> (
        output
      )
    ) in
    iter l ""
  )
  
  let clear_whitespace l = (
    let rec iter input = (
      match input with
      | ' ' :: tl -> (
        iter tl
      )
      | _ -> (
        input
      )
    ) in
    iter l
  )

  let parse_string l line_nr = (
    let rec find_end input output = (
      match input with
      | '\"' :: tl -> (
        (String output)
      )
      | c :: tl -> (
        find_end tl (output ^ (String.make 1 c))
      )
      | [] -> (
        (fprintf stderr "Error at line %d: String literal not terminated!\n" line_nr);
        raise InvalidStr
      )
    ) in
    find_end l ""
  )

  let parse_int (l : char list) (line_nr : int) = (
    let rec iter input integer = (
      match input with
      | c :: tl when c >= '0' && c <= '9' -> (
        iter tl (integer * 10 + (Char.code c - Char.code '0'))
      )
      | tl -> (
        Int integer, tl
      )
    ) in
    iter (clear_whitespace l) 0
  )

  let parse_var (l : char list) (line_nr : int) = (
    let rec find_expr input = (
      let rec iter input = (
        match input with
        | ' ' :: tl -> (
          iter tl
        )
        | _ -> (
          input
        )
      ) in
      match input with
      | ' ' :: tl -> (
        find_expr tl
      )
      | '=' :: tl -> (
        iter tl
      )
      | _ -> (
        fprintf stderr "Error at line %d: Variable declaration missing Expression (post =)!\n" line_nr;
        raise InvalidDecl
      )
    ) in
    let rec iter input output = (
      match input with
      | ' ' :: tl -> (
        output, (find_expr tl)
      )
      | c :: tl -> (
        iter tl (output ^ (String.make 1 c))
      )
      | [] -> (
        fprintf stderr "Error at line %d: Decl of variable '%s' is missing expression!\n" line_nr output;
        raise InvalidDecl
      )
    ) in 
    let rec find_beg input = (
      match input with
      | ' ' :: tl -> (
        find_beg tl
      )
      | _ :: _ -> (
        iter input ""
      )
      | [] -> (
        fprintf stderr "Error at line %d: Invalid variable name!\n" line_nr;
        raise InvalidDecl
      )
    ) in
    find_beg l
  )

  let arithmetic (l : char list) (line_nr : int) = (
    let rec parse input output = (
      match input with
      | c :: tl when c >= '0' && c <= '9' -> (
        let i, tl = parse_int input line_nr in
        parse tl (i :: output)
      )
      | ' ' :: tl -> (
        parse tl output
      )
      | '+' :: tl -> (
        match output with
        | i2 :: rest -> (
          let i1, tl = parse_int tl line_nr in
          parse tl ((Add (i1, i2)) :: rest)
        )
        | [] -> (
          fprintf stderr "Arithmetic Error: Invalid Addition!\n";
          raise InvalidAdd
        )
      )
      | _ -> (
        output
      )
    ) in
    match parse l [] with
    | [e] -> (
      e
    )
    | [] -> (
      fprintf stderr "Arithmetic Error: No output!\n";
      raise InvalidExpr
    )
    | _ -> (
      fprintf stderr "Arithmetic Error: Invalid Arithmetic!\n";
      raise InvalidExpr
    )
  )

  let parse_expr (l : char list) (line_nr : int) = (
    let rec iter input = (
      match input with
      | ' ' :: tl -> (
        iter tl
      )
      | '\"' :: tl -> (
        parse_string tl line_nr
      )
      | c :: tl when c >= '0' && c <= '9' -> (
        arithmetic input line_nr
      )
      | c :: tl -> (
        Var (char_list_to_string input)
      )
      | _ -> (
        fprintf stderr "Error at line %d: Not a valid expression!\n" line_nr;
        raise InvalidExpr
      )
    ) in
    iter l
  )

  let line_parser (line : string) (stmtlist : stmt list) (line_nr : int) = (
    let rec explode index l = (
      if index < 0 then (
        l
      ) else (
        explode (index - 1) (line.[index] :: l)
      )
    ) in
    let rec tokenizer input output = (
      match input with
      | ' ' :: tl -> (
        tokenizer tl output
      )
      | 'p' :: 'r' :: 'i' :: 'n' :: 't' :: tl -> (
        (Print (parse_expr tl line_nr)) :: output
      )
      | 'l' :: 'e' :: 't' :: tl -> (
        let v, t = parse_var tl line_nr in
        (Decl (v, parse_expr t line_nr)) :: output
      )
      | '#' :: _
      | [] -> (
        output
      )
      | _ -> (
        fprintf stderr "Error at line %d: Invalid Statement!\n" line_nr;
        raise InvalidStmt
      )
    ) in
    tokenizer (explode ((String.length line) - 1) []) stmtlist      
  )

  let rec expr_parser e decl = (
    match e with
    | Int _ -> e
    | String _ -> e
    | Var v -> (
      expr_parser (List.assoc v decl.vars) decl
    )
    | Add (e1, e2) -> (
      let i1, i2 = expr_parser e1 decl, expr_parser e2 decl in
      match i1, i2 with
      | Int a, Int b -> (
        Int (a + b)
      )
      | _ -> (
        fprintf stderr "Add Error: Expressions not integers!\n";
        raise InvalidAdd
      )
    )
  )

  let rec print e = (
    match e with
    | String s -> (
      print_endline s
    )
    | Int i -> (
      print_int i;
      print_newline ()
    )
    | _-> (
      fprintf stderr "Print Error: Not a valid expression!\n";
      raise InvalidExpr
    )
  )

  let stmt_parser (stmtlist : stmt list) = (
    let decl = {
      functions = [];
      vars = [];
    } in
    let parse_stmt (s : stmt) = (
      match s with
      | Print e -> (
        print (expr_parser e decl)
      )
      | Decl v -> (
        let name, e = v in
        decl.vars <- (v :: (List.remove_assoc name decl.vars))
      )
    ) in
    List.iter parse_stmt stmtlist
  )

  let file_parser file = (
    let rec loop stmtlist counter = (
      try (
        loop (line_parser (input_line file) stmtlist counter) (counter + 1)
      ) with exn -> (
        stmtlist
      )
    ) in
    loop [] 1
    |> List.rev
    |> stmt_parser
  )
end

open Interpreter

let _ = (
  let debug = true in
  if debug then (
    file_parser (open_in "test.re")
  ) else (
    if Array.length Sys.argv > 1 then (
      try (
        let file = open_in Sys.argv.(1) in
        file_parser file
      ) with exn -> (
        print_endline "Could not open file!";
      )
    ) else (
      print_endline "Please enter a file name!";
    )
  )
)