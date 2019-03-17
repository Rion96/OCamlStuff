module Interpreter = struct

  type token =
    | NAME of string
    | INT of int
    | BOOL of bool
    | STR of string
    | LET | PRINT | IF | ELSE | ENDIF | WHILE | ENDWHILE
    | NEWLINE | ERROR
    | UNOP_MINUS | NOT
    | PLUS | MINUS | MULT | DIV | MOD
    | START | END
    | AND | OR | GREATER | LESS | GEQ | LEQ | NEQ | EQ

  exception InvalidToken

  let interpreter (tokens : token list) = (
    let rec rpn (input : token list) (stack : token list) (output : token list) = (
      let rec precedence (op : token) (stack : token list) (output : token list) = (
        let higher_order (op : token) (stack_op : token) = (
          let order = [UNOP_MINUS; NOT; NEWLINE;
                       MULT; DIV; MOD; NEWLINE;
                       PLUS; MINUS; NEWLINE;
                       LESS; LEQ; GREATER; GEQ; NEWLINE;
                       NEQ; EQ; NEWLINE;
                       AND; NEWLINE;
                       OR; NEWLINE;
                       LET; PRINT] in
          let rec loop (input : token list) (level : int) (op : token) = (
            match input with
            | NEWLINE :: input -> loop input (level + 1) op
            | hd :: tl when hd = op -> level
            | _ :: input -> loop input level op
            | [] -> print_endline "precedence failure!"; raise InvalidToken
          ) in
          let op, stack_op = loop order 0 op, loop order 0 stack_op in
          if op < stack_op then true else false
        ) in
        match stack with
        | [] ->
          [op], output
        | stack_op :: tl -> (
          match op, stack_op with
          (* Special cases with parentheses *)
          | END, START ->
            tl, output
          | END, _ ->
            precedence op tl (stack_op :: output)
          | START, _ 
          | _, START ->
            (op :: stack), output
          | op, stack_op -> (
            if higher_order op stack_op then (
              (op :: stack), output
            ) else (
              precedence op tl (stack_op :: output)
            )
          )
        )
      ) in
      match input with
      | NEWLINE :: _
      | [] -> (
        match stack with
        | [] -> (List.rev output), input
        | hd :: tl -> rpn input tl (hd :: output)
      )
      | NAME _ :: tl
      | STR _ :: tl
      | INT _ :: tl 
      | BOOL _ :: tl ->
        let elem = List.hd input in
        rpn tl stack (elem :: output)
      | op :: tl ->
        let stack, output = precedence op stack output in
        rpn tl stack output
    ) in
    let rec eval_rpn (input : token list) (vars : (string * token) list) (stack : token list) = (
      let dref (stack : token list) (n : int) = (
        let rec loop (stack : token list) (buffer : token list) (index : int) = (
          if index < n then (
            match stack with
            | NAME a :: stack -> (
              let elem = List.assoc a vars in
              loop stack (elem :: buffer) (index + 1)
            )
            | _ :: tl -> (
              let elem = List.hd stack in
              loop tl (elem :: buffer) (index + 1)
            )
            | _ -> raise InvalidToken
          ) else (
            match buffer with
            | hd :: tl ->
              loop (hd :: stack) tl index
            | [] -> stack
          )
        ) in
        loop stack [] 0
      ) in
      match input with
      | NAME _ :: tl
      | STR _ :: tl
      | INT _ :: tl 
      | BOOL _ :: tl ->
        eval_rpn tl vars ((List.hd input) :: stack)
      | op :: input -> (
        match op with
        | LET -> (
          let stack = dref stack 1 in
          match stack with
          | v :: NAME n :: stack -> (
            eval_rpn input ((n, v) :: (List.remove_assoc n vars)) stack
          )
          | _ -> raise InvalidToken
        )
        | PRINT -> (
          let stack = dref stack 1 in
          match stack with
          | STR a :: stack -> (
            print_endline a;
            eval_rpn input vars stack
          )
          | INT a :: stack -> (
            print_int a;
            print_newline ();
            eval_rpn input vars stack
          )
          | BOOL a :: stack -> (
            if a then (
              print_endline "TRUE"
            ) else (
              print_endline "FALSE"
            );
            eval_rpn input vars stack
          )
          | _ -> raise InvalidToken
        )
        | PLUS -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack -> (
            eval_rpn input vars ((INT (a + b)) :: stack)
          )
          | STR b :: STR a :: stack -> (
            eval_rpn input vars ((STR (a ^ b)) :: stack)
          )
          | _ -> raise InvalidToken
        )
        | MINUS -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack -> (
            eval_rpn input vars ((INT (a - b)) :: stack)
          )
          | _ -> raise InvalidToken
        )
        | UNOP_MINUS -> (
          let stack = dref stack 1 in
          match stack with
          | INT a :: stack -> (
            eval_rpn input vars ((INT (-a)) :: stack)
          )
          | _ -> raise InvalidToken
        )
        | MULT -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack -> (
            eval_rpn input vars ((INT (a * b)) :: stack)
          )
          | _ -> (
            raise InvalidToken
          )
        )
        | DIV -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack -> (
            eval_rpn input vars ((INT (a / b)) :: stack)
          )
          | _ -> raise InvalidToken
        )
        | MOD -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars ((INT (a mod b)) :: stack)
          | _ -> raise InvalidToken
        )
        | LESS -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            let v = BOOL (a < b) in
            eval_rpn input vars (v :: stack)
          | _ -> raise InvalidToken
        )
        | GREATER -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            let v = BOOL (a > b) in
            eval_rpn input vars (v :: stack)
          | _ -> raise InvalidToken
        )
        | LEQ -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            let v = BOOL (a <= b) in
            eval_rpn input vars (v :: stack)
          | _ -> raise InvalidToken
        )
        | GEQ -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            let v = BOOL (a >= b) in
            eval_rpn input vars (v :: stack)
          | _ -> raise InvalidToken
        )
        | NEQ -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            let v = BOOL (a <> b) in
            eval_rpn input vars (v :: stack)
          | STR b :: STR a :: stack ->
            let v = BOOL (a <> b) in
            eval_rpn input vars (v :: stack)
          | _ -> raise InvalidToken
        )
        | EQ -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            let v = BOOL (a = b) in
            eval_rpn input vars (v :: stack)
          | STR b :: STR a :: stack ->
            let v = BOOL (a = b) in
            eval_rpn input vars (v :: stack)
          | _ -> raise InvalidToken
        )
        | AND -> (
          let stack = dref stack 2 in
          match stack with
          | BOOL b :: BOOL a :: stack ->
            let v = BOOL (a && b) in
            eval_rpn input vars (v :: stack)
          | _ -> raise InvalidToken
        )
        | OR -> (
          let stack = dref stack 2 in
          match stack with
          | BOOL b :: BOOL a :: stack ->
            let v = BOOL (a || b) in
            eval_rpn input vars (v :: stack)
          | _ -> raise InvalidToken
        )
        | NOT -> (
          let stack = dref stack 1 in
          match stack with
          | BOOL a :: stack ->
            let v = BOOL (not a) in
            eval_rpn input vars (v :: stack)
          | _ -> raise InvalidToken
        )
        | _ -> raise InvalidToken
      )
      | [] -> (
        vars
      )
    ) in
    let strip_if (stack : token list) = (
      let rec loop (stack : token list) (cnt : int) = (
        if cnt > 0 then (
          match stack with
          | IF :: stack -> loop stack (cnt + 1)
          | ELSE :: stack when cnt = 1 -> stack
          | ENDIF :: stack -> loop stack (cnt - 1)
          | _ :: stack -> loop stack cnt
          | [] -> []
        ) else (
          stack
        )
      ) in
      loop stack 1
    ) in
    let strip_else (stack : token list) = (
      let rec loop (stack : token list) (buffer : token list) (cnt : int) = (
        if cnt > 0 then (
          match stack with
          | IF :: tl -> loop tl buffer (cnt + 1)
          | ENDIF :: tl -> loop tl buffer (cnt - 1)
          | _ :: tl -> loop tl buffer cnt
          | [] -> []
        ) else (
          match buffer with
          | hd :: tl -> loop (hd :: stack) tl cnt
          | [] -> stack
        )
      ) in
      let rec store_if (input : token list) (buffer : token list) (cnt : int) = (
        if cnt > 0 then (
          match input with
          | IF :: tl -> store_if tl (IF :: buffer) (cnt + 1)
          | ELSE :: tl when cnt = 1 -> loop tl buffer 1
          | ENDIF :: tl -> store_if tl (ENDIF :: buffer) (cnt - 1)
          | hd :: tl -> store_if tl (hd :: buffer) cnt
          | [] -> []
        ) else (
          stack
        )
      ) in
      store_if stack [] 1
    ) in
    let rec strip_while (stack : token list) (cnt : int) = (
      match stack with
      | WHILE :: tl -> strip_while tl (cnt + 1)
      | ENDWHILE :: tl when cnt = 1 -> tl
      | ENDWHILE :: tl -> strip_while tl (cnt - 1)
      | hd :: tl -> strip_while tl cnt
      | [] -> []
    ) in
    let copy_while (stack : token list) = (
      let rec back (stack : token list) (buffer : token list)= (
        match buffer with
        | hd :: tl -> back (hd :: stack) tl
        | [] -> stack
      ) in
      let rec create_buffer (stack : token list) (buffer : token list) (cnt : int) = (
        match stack with
        | WHILE :: tl -> create_buffer tl (WHILE :: buffer) (cnt + 1)
        | ENDWHILE :: tl when cnt = 1 -> buffer
        | ENDWHILE :: tl -> create_buffer tl (ENDWHILE :: buffer) (cnt - 1)
        | hd :: tl -> create_buffer tl (hd :: buffer) cnt
        | [] -> buffer
      ) in
      let rec store_while (stack : token list) (buffer : token list) = (
        match stack with
        | NEWLINE :: tl -> (
          let stmts = create_buffer tl [] 1 in
          let fst_stmts = List.append (NEWLINE :: buffer) stmts in
          back tl fst_stmts
        )
        | hd :: tl -> store_while tl (hd :: buffer)
        | [] -> []
      ) in
      store_while stack []
    ) in
    let rec iterate (input : token list) (vars : (string * token) list) = (
      match input with
      | [] -> ()
      | NEWLINE :: input -> iterate input vars
      | IF :: input -> (
        let expr, input = rpn (LET :: NAME "_EVAL_" :: input) [] [] in
        let vars = eval_rpn expr vars [] in
        match List.assoc "_EVAL_" vars with
        | BOOL true -> iterate (strip_else input) vars
        | BOOL false -> iterate (strip_if input) vars
        | _ -> raise InvalidToken
      )
      | WHILE :: tl -> (
        let expr, tl = rpn (LET :: NAME "_EVAL_" :: tl) [] [] in
        let vars = eval_rpn expr vars [] in
        match List.assoc "_EVAL_" vars with
        | BOOL true -> iterate (copy_while input) vars
        | BOOL false -> iterate (strip_while tl 1) vars
        | _ -> raise InvalidToken
      )
      | ENDWHILE :: input
      | ENDIF :: input ->
        iterate input (List.remove_assoc "_EVAL_" vars)
      | _ -> (
        let expr, input = rpn input [] [] in
        let vars = eval_rpn expr vars [] in
        iterate input vars
      )
    ) in
    iterate tokens []
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
        try (
          INT (int_of_string buffer), stack
        ) with exn -> (
          ERROR, stack
        )
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
    let rec name_token (buffer : string) (input : char list) = (
      match input with
      | '-' :: stack
      | '+' :: stack
      | '!' :: stack
      | '(' :: stack
      | ' ' :: stack -> (
        NAME buffer, input
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
      | ';' :: stack
      | '\n' :: stack ->
        main_parser (NEWLINE :: buffer) stack
      | ')' :: stack ->
        main_parser (END :: buffer) stack
      | '(' :: stack
      | '+' :: '(' :: stack ->
        main_parser (START :: buffer) stack
      | '-' :: '(' :: stack ->
        main_parser (START :: UNOP_MINUS :: buffer) stack
      | '-' :: stack ->
        main_parser (MINUS :: buffer) stack
      | '+' :: stack ->
        main_parser (PLUS :: buffer) stack
      | '*' :: stack ->
        main_parser (MULT :: buffer) stack
      | '/' :: stack ->
        main_parser (DIV :: buffer) stack
      | '%' :: stack ->
        main_parser (MOD :: buffer) stack
      | '!' :: stack ->
        main_parser (NOT :: buffer) stack
      | '|' :: '|' :: stack ->
        main_parser (OR :: buffer) stack
      | '&' :: '&' :: stack ->
        main_parser (AND :: buffer) stack
      | '=' :: '>' :: stack ->
        main_parser (GEQ :: buffer) stack
      | '=' :: '<' :: stack ->
        main_parser (NEQ :: buffer) stack
      | '=' :: '!' :: stack ->
        main_parser (NEQ :: buffer) stack
      | '=' :: stack ->
        main_parser (EQ :: buffer) stack
      | '<' :: stack ->
        main_parser (LESS :: buffer) stack
      | '>' :: stack ->
        main_parser (GREATER :: buffer) stack
      | '#' :: stack ->
        main_parser (ignore_token buffer) stack
      | '\t' :: stack
      | ' ' :: stack ->
        main_parser buffer stack
      | '"' :: stack ->
        let tok, stack = str_token "" stack in
        main_parser (tok :: buffer) stack
      (* PRINT *)
      | 'T' :: 'N' :: 'I' :: 'R' :: 'P' :: tl ->
        main_parser (PRINT :: buffer) tl
      (* LET *)
      | 'T' :: 'E' :: 'L' :: tl ->
        main_parser (LET :: buffer) tl
      (* TRUE *)
      | 'E' :: 'U' :: 'R' :: 'T' :: tl ->
        main_parser (BOOL true :: buffer) tl
      (* FALSE *)
      | 'E' :: 'S' :: 'L' :: 'A' :: 'F' :: tl ->
        main_parser (BOOL false :: buffer) tl
      (* ENDIF *)
      | 'F' :: 'I' :: 'D' :: 'N' :: 'E' :: tl ->
        main_parser (ENDIF :: buffer) tl
      (* IF *)
      | 'F' :: 'I' :: tl ->
        main_parser (IF :: buffer) tl
      (* ELSE *)
      | 'E' :: 'S' :: 'L' :: 'E' :: tl ->
        main_parser (ELSE :: buffer) tl
      (* ENDWHILE *)
      | 'E' :: 'L' :: 'I' :: 'H' :: 'W' :: 'D' :: 'N' :: 'E' :: tl ->
        main_parser (ENDWHILE :: buffer) tl
      (* WHILE *)
      | 'E' :: 'L' :: 'I' :: 'H' :: 'W' :: tl ->
        main_parser (WHILE :: buffer) tl 
      | c :: stack when c >= '0' && c <= '9' ->
        let tok, stack = int_token "" (c :: stack) in
        main_parser (tok :: buffer) stack
      | c :: stack ->
        let tok, stack = name_token "" (c :: stack) in
        main_parser (tok :: buffer) stack
      | [] ->
        buffer
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
end

open Interpreter

let _ = (
  let file =
  if Array.length Sys.argv > 1 then (
    open_in Sys.argv.(1)
  ) else (
    open_in "test"
  ) in
  file
  |> char_stack
  |> tokenizer
  |> interpreter
)