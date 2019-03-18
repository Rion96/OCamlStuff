module Interpreter = struct

  type token =
    | NAME of string
    | FUN of string | ENDFUN
    | INT of int
    | FLOAT of float
    | BOOL of bool
    | STR of string
    | LET | PRINT | PRINTLN
    | IF | ELSE | ENDIF | WHILE | ENDWHILE
    | NEWLINE | ERROR | LIST of token list | SEQ | RETURN
    | UNOP_MINUS | NOT | POW | ARR
    | PLUS | MINUS | MULT | DIV | MOD
    | START | END | BR_START | BR_END
    | AND | OR | GREATER | LESS | GEQ | LEQ | NEQ | EQ

  exception InvalidToken of (token * string)
  exception InvalidFunction

  let interpreter (tokens : token list) = (
    let rec iterate (input : token list)
                    (vars : (string * token) list)
                    (funs : (string * (int * string list * token list)) list) = (
      let rec rpn (input : token list) (stack : token list) (output : token list) = (
        let rec precedence (op : token) (stack : token list) (output : token list) = (
          let higher_order (op : token) (stack_op : token) = (
            let get_lvl (op : token) = (
              match op with
              | ARR | FUN _ -> 0
              | UNOP_MINUS | NOT | POW -> 1
              | MULT | DIV | MOD -> 2
              | PLUS | MINUS -> 3
              | LESS | LEQ | GREATER | GEQ -> 4
              | NEQ | EQ -> 5
              | AND -> 6
              | OR -> 7
              | LET | PRINT | PRINTLN | RETURN -> 8
              | tok -> raise (InvalidToken (tok, "at higher_order"))
            ) in
            let op, stack_op = get_lvl op, get_lvl stack_op in
            op < stack_op
          ) in
          match stack with
          | [] ->
            [op], output
          | stack_op :: tl -> (
            match op, stack_op with
            (* Special cases with parentheses and brackets *)
            | BR_END, BR_START ->
              tl, (ARR :: output)
            | END, START ->
              tl, output
            | BR_END, _
            | END, _ ->
              precedence op tl (stack_op :: output)
            | BR_START, _
            | START, _ 
            | _, BR_START
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
        | SEQ :: _
        | [] -> (
          match stack with
          | [] -> (List.rev output), input
          | hd :: tl -> rpn input tl (hd :: output)
        )
        | NAME n :: tl when List.mem_assoc n funs -> (
          rpn tl (FUN n :: stack) output
        )
        | NAME _ :: tl
        | STR _ :: tl
        | INT _ :: tl 
        | FLOAT _ :: tl
        | BOOL _ :: tl ->
          let elem = List.hd input in
          rpn tl stack (elem :: output)
        | op :: tl ->
          let stack, output = precedence op stack output in
          rpn tl stack output
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
      let copy_while (stack : token list) = (
        let rec strip_while (stack : token list) (buffer : token list) (cnt : int) = (
          match stack with
          | WHILE :: tl -> strip_while tl (WHILE :: buffer) (cnt + 1)
          | ENDWHILE :: tl when cnt = 1 -> List.rev buffer, tl
          | ENDWHILE :: tl -> strip_while tl (ENDWHILE :: buffer) (cnt - 1)
          | hd :: tl -> strip_while tl (hd :: buffer) cnt
          | [] -> raise (InvalidToken (LIST [], "at copy_while"))
        ) in
        strip_while stack [] 1
      ) in
      let store_fun (input : token list) = (
        let rec parse_args (input : token list) (output : string list) (counter : int) = (
          match input with
          | NEWLINE :: input
          | SEQ :: input -> (
            input, counter, output
          )
          | NAME n :: input -> (
            parse_args input (n :: output) (counter + 1)
          )
          | _ -> raise InvalidFunction
        ) in 
        let rec skip_fun (input : token list) (output : token list) (counter : int) = (
          match input with
          | FUN "_DEF_" :: tl ->
            skip_fun tl (List.hd input :: output) (counter + 1)
          | ENDFUN :: tl when counter = 1 ->
            tl, List.rev output
          | ENDFUN :: tl ->
            skip_fun tl (List.hd input :: output) (counter - 1)
          | hd :: tl ->
            skip_fun tl (hd :: output) counter
          | [] ->
            raise InvalidFunction
        ) in
        let (input, argc, argv : token list * int * string list) = parse_args input [] 0 in
        let (output, sequence : token list * token list) = skip_fun input [] 1 in
        (argc, argv, sequence), output
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
              | stack -> raise (InvalidToken (LIST stack, "at dref"))
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
        | FLOAT _ :: tl
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
            | stack -> raise (InvalidToken (LIST stack, "at LET"))
          )
          | FUN n -> (
            let argc, args, sequence = List.assoc n funs in
            let stack = dref stack argc in
            let rec assign (args : string list) (stack : token list) (vars : (string * token) list) = (
              match args, stack with
              | n :: tl, v :: stack ->
                assign tl stack ((n, v) :: (List.remove_assoc n vars))
              | [], _ -> vars, stack
              | _ -> print_endline "Invalid function evaluation!"; raise InvalidFunction
            ) in
            let fn_vars, stack = assign args stack vars in
            let res_vars = iterate sequence fn_vars funs in
            if List.mem_assoc "_RETURN_" res_vars then
              eval_rpn input vars ((List.assoc "_RETURN_" res_vars) :: stack)
            else
              eval_rpn input vars stack
          )
          | PRINT -> (
            let stack = dref stack 1 in
            match stack with
            | STR a :: stack -> (
              print_string a;
              eval_rpn input vars stack
            )
            | INT a :: stack -> (
              print_int a;
              eval_rpn input vars stack
            )
            | FLOAT a :: stack -> (
              print_float a;
              eval_rpn input vars stack
            )
            | BOOL a :: stack -> (
              if a then (
                print_string "TRUE"
              ) else (
                print_string "FALSE"
              );
              eval_rpn input vars stack
            )
            | stack -> raise (InvalidToken (LIST stack, "at PRINT"))
          )
          | PRINTLN -> (
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
            | FLOAT a :: stack -> (
              print_float a;
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
            | stack -> raise (InvalidToken (LIST stack, "at PRINTLN"))
          )
          | PLUS -> (
            let stack = dref stack 2 in
            match stack with
            | INT b :: INT a :: stack -> (
              eval_rpn input vars ((INT (a + b)) :: stack)
            )
            | FLOAT b :: FLOAT a :: stack -> (
              eval_rpn input vars ((FLOAT (a +. b) :: stack))
            )
            | FLOAT f :: INT i :: stack
            | INT i :: FLOAT f :: stack -> (
              let i = float_of_int i in
              eval_rpn input vars ((FLOAT (i +. f) :: stack))
            )
            | STR b :: STR a :: stack -> (
              eval_rpn input vars ((STR (a ^ b)) :: stack)
            )
            | stack -> raise (InvalidToken (LIST stack, "at PLUS"))
          )
          | MINUS -> (
            let stack = dref stack 2 in
            match stack with
            | INT b :: INT a :: stack -> (
              eval_rpn input vars ((INT (a - b)) :: stack)
            )
            | FLOAT b :: FLOAT a :: stack -> (
              eval_rpn input vars ((FLOAT (a -. b) :: stack))
            )
            | FLOAT b :: INT a :: stack -> (
              let a = float_of_int a in
              eval_rpn input vars ((FLOAT (a -. b) :: stack))
            )
            | INT b :: FLOAT a :: stack -> (
              let b = float_of_int b in
              eval_rpn input vars ((FLOAT (a -. b) :: stack))
            )
            | stack -> raise (InvalidToken (LIST stack, "at MINUS"))
          )
          | UNOP_MINUS -> (
            let stack = dref stack 1 in
            match stack with
            | INT a :: stack -> (
              eval_rpn input vars ((INT (-a)) :: stack)
            )
            | FLOAT a :: stack -> (
              eval_rpn input vars ((FLOAT (-.a) :: stack))
            )
            | stack -> raise (InvalidToken (LIST stack, "at UNOP_MINUS"))
          )
          | MULT -> (
            let stack = dref stack 2 in
            match stack with
            | INT b :: INT a :: stack -> (
              eval_rpn input vars ((INT (a * b)) :: stack)
            )
            | FLOAT b :: FLOAT a :: stack -> (
              eval_rpn input vars ((FLOAT (a *. b) :: stack))
            )
            | FLOAT f :: INT i :: stack
            | INT i :: FLOAT f :: stack -> (
              let i = float_of_int i in
              eval_rpn input vars ((FLOAT (i *. f) :: stack))
            )
            | stack -> raise (InvalidToken (LIST stack, "at MULT"))
          )
          | DIV -> (
            let stack = dref stack 2 in
            match stack with
            | INT b :: INT a :: stack -> (
              eval_rpn input vars ((INT (a / b)) :: stack)
            )
            | FLOAT b :: FLOAT a :: stack -> (
              eval_rpn input vars ((FLOAT (a /. b) :: stack))
            )
            | FLOAT b :: INT a :: stack -> (
              let a = float_of_int a in
              eval_rpn input vars ((FLOAT (a /. b) :: stack))
            )
            | INT b :: FLOAT a :: stack -> (
              let b = float_of_int b in
              eval_rpn input vars ((FLOAT (a /. b) :: stack))
            )
            | stack -> raise (InvalidToken (LIST stack, "at DIV"))
          )
          | MOD -> (
            let stack = dref stack 2 in
            match stack with
            | INT b :: INT a :: stack ->
              eval_rpn input vars ((INT (a mod b)) :: stack)
            | stack -> raise (InvalidToken (LIST stack, "at MOD"))
          )
          | LESS -> (
            let stack = dref stack 2 in
            match stack with
            | INT b :: INT a :: stack ->
              let v = BOOL (a < b) in
              eval_rpn input vars (v :: stack)
            | FLOAT b :: FLOAT a :: stack ->
              eval_rpn input vars (BOOL (a < b) :: stack)
            | FLOAT b :: INT a :: stack -> (
              let a = float_of_int a in
              eval_rpn input vars (BOOL (a < b) :: stack)
            )
            | INT b :: FLOAT a :: stack -> (
              let b = float_of_int b in
              eval_rpn input vars (BOOL (a < b) :: stack)
            )
            | stack -> raise (InvalidToken (LIST stack, "at LESS"))
          )
          | GREATER -> (
            let stack = dref stack 2 in
            match stack with
            | INT b :: INT a :: stack ->
              let v = BOOL (a > b) in
              eval_rpn input vars (v :: stack)
            | FLOAT b :: FLOAT a :: stack ->
              eval_rpn input vars (BOOL (a > b) :: stack)
            | FLOAT b :: INT a :: stack -> (
              let a = float_of_int a in
              eval_rpn input vars (BOOL (a > b) :: stack)
            )
            | INT b :: FLOAT a :: stack -> (
              let b = float_of_int b in
              eval_rpn input vars (BOOL (a > b) :: stack)
            )
            | stack -> raise (InvalidToken (LIST stack, "at GREATER"))
          )
          | LEQ -> (
            let stack = dref stack 2 in
            match stack with
            | INT b :: INT a :: stack ->
              let v = BOOL (a <= b) in
              eval_rpn input vars (v :: stack)
            | FLOAT b :: FLOAT a :: stack ->
              eval_rpn input vars (BOOL (a <= b) :: stack)
            | FLOAT b :: INT a :: stack -> (
              let a = float_of_int a in
              eval_rpn input vars (BOOL (a <= b) :: stack)
            )
            | INT b :: FLOAT a :: stack -> (
              let b = float_of_int b in
              eval_rpn input vars (BOOL (a <= b) :: stack)
            )
            | stack -> raise (InvalidToken (LIST stack, "at LEQ"))
          )
          | GEQ -> (
            let stack = dref stack 2 in
            match stack with
            | INT b :: INT a :: stack ->
              let v = BOOL (a >= b) in
              eval_rpn input vars (v :: stack)
            | FLOAT b :: FLOAT a :: stack ->
              eval_rpn input vars (BOOL (a >= b) :: stack)
            | FLOAT b :: INT a :: stack -> (
              let a = float_of_int a in
              eval_rpn input vars (BOOL (a >= b) :: stack)
            )
            | INT b :: FLOAT a :: stack -> (
              let b = float_of_int b in
              eval_rpn input vars (BOOL (a >= b) :: stack)
            )
            | stack -> raise (InvalidToken (LIST stack, "at GEQ"))
          )
          | NEQ -> (
            let stack = dref stack 2 in
            match stack with
            | INT b :: INT a :: stack ->
              let v = BOOL (a <> b) in
              eval_rpn input vars (v :: stack)
            | FLOAT b :: FLOAT a :: stack -> (
              eval_rpn input vars ((BOOL (a <> b) :: stack))
            )
            | FLOAT f :: INT i :: stack
            | INT i :: FLOAT f :: stack -> (
              let i = float_of_int i in
              eval_rpn input vars ((BOOL (i <> f) :: stack))
            )
            | STR b :: STR a :: stack ->
              let v = BOOL (a <> b) in
              eval_rpn input vars (v :: stack)
            | stack -> raise (InvalidToken (LIST stack, "at NEQ"))
          )
          | EQ -> (
            let stack = dref stack 2 in
            match stack with
            | INT b :: INT a :: stack ->
              let v = BOOL (a = b) in
              eval_rpn input vars (v :: stack)
            | FLOAT b :: FLOAT a :: stack -> (
              eval_rpn input vars ((BOOL (a = b) :: stack))
            )
            | FLOAT f :: INT i :: stack
            | INT i :: FLOAT f :: stack -> (
              let i = float_of_int i in
              eval_rpn input vars ((BOOL (i = f) :: stack))
            )
            | STR b :: STR a :: stack ->
              let v = BOOL (a = b) in
              eval_rpn input vars (v :: stack)
            | stack -> raise (InvalidToken (LIST stack, "at EQ"))
          )
          | AND -> (
            let stack = dref stack 2 in
            match stack with
            | BOOL b :: BOOL a :: stack ->
              let v = BOOL (a && b) in
              eval_rpn input vars (v :: stack)
            | stack -> raise (InvalidToken (LIST stack, "at AND"))
          )
          | OR -> (
            let stack = dref stack 2 in
            match stack with
            | BOOL b :: BOOL a :: stack ->
              let v = BOOL (a || b) in
              eval_rpn input vars (v :: stack)
            | stack -> raise (InvalidToken (LIST stack, "at OR"))
          )
          | NOT -> (
            let stack = dref stack 1 in
            match stack with
            | BOOL a :: stack ->
              let v = BOOL (not a) in
              eval_rpn input vars (v :: stack)
            | stack -> raise (InvalidToken (LIST stack, "at NOT"))
          )
          | ARR -> (
            let stack = dref stack 1 in
            match stack with
            | INT i :: NAME n :: stack when i >= 0 ->
              let arr = "ARR_" ^ n ^ "_" ^ (string_of_int i) in
              eval_rpn input vars ((NAME arr) :: stack)
            | stack -> raise (InvalidToken (LIST stack, "at ARR"))
          )
          | POW -> (
            let stack = dref stack 2 in
            match stack with
            | INT b :: INT a :: stack ->
              let b = float_of_int b in
              let a = float_of_int a in
              eval_rpn input vars ((FLOAT (a ** b)) :: stack)
            | FLOAT b :: INT a :: stack ->
              let a = float_of_int a in
              eval_rpn input vars ((FLOAT (a ** b)) :: stack)
            | INT b :: FLOAT a :: stack ->
              let b = float_of_int b in
              eval_rpn input vars ((FLOAT (a ** b)) :: stack)
            | stack -> raise (InvalidToken (LIST stack, "at POW"))
          )
          | op -> raise (InvalidToken (op, "at eval_rpn"))
        )
        | [] -> (
          vars
        )
      ) in  
      match input with
      | [] -> vars
      | SEQ :: input
      | NEWLINE :: input -> iterate input vars funs
      | IF :: input -> (
        let expr, input = rpn (LET :: NAME "_EVAL_" :: input) [] [] in
        let vars = eval_rpn expr vars [] in
        match List.assoc "_EVAL_" vars with
        | BOOL true -> iterate (strip_else input) vars funs
        | BOOL false -> iterate (strip_if input) vars funs
        | eval -> raise (InvalidToken (eval, "at IF"))
      )
      | RETURN :: input -> (
        let expr, _ = rpn (LET :: NAME "_RETURN_" :: input) [] [] in
        let vars = eval_rpn expr vars [] in
        vars
      )
      | WHILE :: tl -> (
        let cond, stack = rpn (LET :: NAME "_EVAL_" :: tl) [] [] in
        let copy, stack = copy_while stack in
        let rec loop (vars : (string * token) list) = (
          let vars = eval_rpn cond vars [] in
          match List.assoc "_EVAL_" vars with
          | BOOL true -> (
            let vars = iterate copy vars funs in
            if List.mem_assoc "_RETURN_" vars then
              vars
            else
              loop vars
          )
          | BOOL false -> iterate stack vars funs
          | eval -> raise (InvalidToken (eval, "at WHILE"))
        ) in
        loop vars
      )
      | ENDWHILE :: input
      | ENDIF :: input ->
        iterate input (List.remove_assoc "_EVAL_" vars) funs
      | FUN "_DEF_" :: NAME n :: input ->
        let new_fun, input = store_fun input in
        iterate input vars ((n, new_fun) :: (List.remove_assoc n funs))
      | _ -> (
        let expr, input = rpn input [] [] in
        let vars = eval_rpn expr vars [] in
        iterate input vars funs
      )
    ) in
    iterate tokens [] []
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
    let rec num_token (buffer : string) = (
      function
      | hd :: tl when hd = '.' || hd >= '0' && hd <= '9' -> (
        num_token ((String.make 1 hd) ^ buffer) tl
      )
      | stack -> (
        try (
          INT (int_of_string buffer), stack
        ) with exn -> (
          try (
            FLOAT (float_of_string buffer), stack
          ) with exn -> (
            ERROR, stack
          )
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
      | '\n' :: _
      | ';' :: _
      | '-' :: _
      | '+' :: _
      | '!' :: _
      | '(' :: _
      | '[' :: _
      | ' ' :: _ -> (
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
      | ';' :: stack ->
        main_parser (SEQ :: buffer) stack
      | '\n' :: stack ->
        main_parser (NEWLINE :: buffer) stack
      | ']' :: stack ->
        main_parser (BR_END :: buffer) stack
      | '[' :: stack ->
        main_parser (BR_START :: buffer) stack
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
      | '^' :: stack ->
        main_parser (POW :: buffer) stack
      | '!' :: stack ->
        main_parser (NOT :: buffer) stack
      | '|' :: '|' :: stack ->
        main_parser (OR :: buffer) stack
      | '&' :: '&' :: stack ->
        main_parser (AND :: buffer) stack
      | '=' :: '>' :: stack ->
        main_parser (GEQ :: buffer) stack
      | '=' :: '<' :: stack ->
        main_parser (LEQ :: buffer) stack
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
      (* PRINTLN *)
      | 'N' :: 'L' :: 'T' :: 'N' :: 'I' :: 'R' :: 'P' :: tl ->
        main_parser (PRINTLN :: buffer) tl
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
      (* ENDFUN *)
      | 'N' :: 'U' :: 'F' :: 'D' :: 'N' :: 'E' :: tl ->
        main_parser (ENDFUN :: buffer) tl
      (* FUN *)
      | 'N' :: 'U' :: 'F' :: tl ->
        main_parser (FUN "_DEF_" :: buffer) tl
      (* RETURN *)
      | 'N' :: 'R' :: 'U' :: 'T' :: 'E' :: 'R' :: tl ->
        main_parser (RETURN :: buffer) tl
      | '.' :: c :: stack when c >= '0' && c <= '9' ->
        let tok, stack = num_token "" ('.' :: c :: stack) in
        main_parser (tok :: buffer) stack
      | c :: stack when c >= '0' && c <= '9' ->
        let tok, stack = num_token "" (c :: stack) in
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