(* A nicer interpreter. In my opinion. *)
module Interpreter = struct

  let debug = false

  type token =
    | NAME of string
    | INT of int
    | STR of string
    | LET
    | PRINT
    | NEWLINE
    | ERROR
    | UNOP_MINUS
    | PLUS | MINUS | MULT | DIV | MOD
    | START | END
    | AND | OR | GREATER | LESS | GEQ | LEQ | NEQ | EQ

  exception InvalidToken

  let interpreter (tokens : token list) = (
    let rec rpn (input : token list) (stack : token list) (output : token list) = (
      let rec precedence (op : token) (stack : token list) (output : token list) = (
        let higher_order (op : token) (stack_op : token) = (
          let order = [UNOP_MINUS; NEWLINE;
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
            | hd :: tl -> loop tl level op
            | [] -> raise InvalidToken
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
      | INT _ :: tl ->
        let elem = List.hd input in
        rpn tl stack (elem :: output)
      | op :: tl ->
        let stack, output = precedence op stack output in
        rpn tl stack output
    ) in
    let rec eval_rpn (input : token list) (vars : (string * token) list) (stack : token list) = (
      match input with
      | NAME _ :: tl
      | STR _ :: tl
      | INT _ :: tl ->
        eval_rpn tl vars ((List.hd input) :: stack)
      | op :: input -> (
        match op with
        | LET -> (
          match stack with
          | v :: NAME n :: stack -> (
            eval_rpn input ((n, v) :: (List.remove_assoc n vars)) stack
          )
          | _ -> raise InvalidToken
        )
        | PRINT -> (
          match stack with
          | NAME a :: stack -> (
            let v = List.assoc a vars in
            eval_rpn (PRINT :: input) vars (v :: stack)
          )
          | STR a :: stack -> (
            print_endline a;
            eval_rpn input vars stack
          )
          | INT a :: stack -> (
            print_int a;
            print_newline ();
            eval_rpn input vars stack
          )
          | _ -> raise InvalidToken
        )
        | PLUS -> (
          match stack with
          | INT b :: INT a :: stack -> (
            eval_rpn input vars ((INT (a + b)) :: stack)
          )
          | STR b :: STR a :: stack -> (
            eval_rpn input vars ((STR (a ^ b)) :: stack)
          )
          | NAME a :: stack -> (
            let v = List.assoc a vars in
            eval_rpn (PLUS :: input) vars (v :: stack)
          )
          | b :: NAME a :: stack -> (
            let v = List.assoc a vars in
            eval_rpn (PLUS :: input) vars (b :: v :: stack)
          )
          | _ -> raise InvalidToken
        )
        | MINUS -> (
          match stack with
          | INT b :: INT a :: stack -> (
            eval_rpn input vars ((INT (a - b)) :: stack)
          )
          | NAME a :: stack -> (
            let v = List.assoc a vars in
            eval_rpn (MINUS :: input) vars (v :: stack)
          )
          | b :: NAME a :: stack -> (
            let v = List.assoc a vars in
            eval_rpn (MINUS :: input) vars (b :: v :: stack)
          )
          | _ -> raise InvalidToken
        )
        | UNOP_MINUS -> (
          match stack with
          | INT a :: stack -> (
            eval_rpn input vars ((INT (-a)) :: stack)
          )
          | NAME a :: stack -> (
            let v = List.assoc a vars in
            eval_rpn (UNOP_MINUS :: input) vars (v :: stack)
          )
          | _ -> raise InvalidToken
        )
        | MULT -> (
          match stack with
          | INT b :: INT a :: stack -> (
            eval_rpn input vars ((INT (a * b)) :: stack)
          )
          | NAME a :: stack -> (
            let v = List.assoc a vars in
            eval_rpn (MULT :: input) vars (v :: stack)
          )
          | b :: NAME a :: stack -> (
            let v = List.assoc a vars in
            eval_rpn (MULT :: input) vars (b :: v :: stack)
          )
          | _ -> (
            raise InvalidToken
          )
        )
        | DIV -> (
          match stack with
          | INT b :: INT a :: stack -> (
            eval_rpn input vars ((INT (a / b)) :: stack)
          )
          | NAME a :: stack -> (
            let v = List.assoc a vars in
            eval_rpn (DIV :: input) vars (v :: stack)
          )
          | b :: NAME a :: stack -> (
            let v = List.assoc a vars in
            eval_rpn (DIV :: input) vars (b :: v :: stack)
          )
          | _ -> raise InvalidToken
        )
        | MOD -> (
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars ((INT (a mod b)) :: stack)
          | NAME a :: stack ->
            eval_rpn (MOD :: input) vars ((List.assoc a vars) :: stack)
          | b :: NAME a :: stack ->
            eval_rpn (MOD :: input) vars (b :: (List.assoc a vars) :: stack)
          | _ -> raise InvalidToken
        )
        | _ -> raise InvalidToken
      )
      | [] -> (
        vars
      )
    ) in
    let rec iterate (input : token list) (vars : (string * token) list) = (
      match input with
      | [] -> ()
      | NEWLINE :: input -> iterate input vars
      | _ -> (
        let expr, input = rpn input [] [] in
        let vars = eval_rpn expr vars [] in
        iterate input vars
      )
    ) in
    let rec debug_iter (input : token list) (vars : (string * token) list) (output : token list) = (
      match input with
      | [] -> output
      | NEWLINE :: input -> debug_iter input vars output
      | _ -> (
        let expr, input = rpn input [] [] in
        try (
          let vars = eval_rpn expr vars [] in
          debug_iter input vars (List.append output expr)
        ) with InvalidToken -> (
          print_endline "Failed!";
          List.append output expr
        )
      )
    ) in
    if debug then (
      debug_iter tokens [] []
    ) else (
      iterate tokens [];
      []
    )
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
      | '#' :: stack ->
        main_parser (ignore_token buffer) stack
      | ' ' :: stack ->
        main_parser buffer stack
      | '"' :: stack ->
        let tok, stack = str_token "" stack in
        main_parser (tok :: buffer) stack
      | 'P' :: tl ->
        main_parser (PRINT :: buffer) tl
      | 'T' :: 'E' :: 'L' :: tl ->
        main_parser (LET :: buffer) tl
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
  open_in "test"
  |> char_stack
  |> tokenizer
  |> interpreter
)