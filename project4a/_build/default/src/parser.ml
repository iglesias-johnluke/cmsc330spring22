open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)


(* let rec parse_expr toks = ([], Value(Int 2)) *)
let rec parse_expr toks = 
  let (t, exp) = parseExpr toks in
  if t <> [] then (*if there's still unprocessed tokens, try to keep parsing *)
    parseExpr t
  else (*if all tokens processed, return AST expression *)
    ([], exp)
  (*parses all non-terminals and terminals*)
  and parseExpr toks = 
    match (lookahead toks) with
    | Some Tok_Let -> parseLet toks
    | Some Tok_If -> parseIf toks
    | Some Tok_Fun -> parseFunctionExpr toks
    (*parse Or *)
    | _ ->  parseOr toks

  (* Tok_Int | Tok_Bool | Tok_String | Tok_ID | ( Expr ) *)
  and parsePrimary toks = 
    match lookahead toks with 
      | Some (Tok_Int i) -> let t = match_token toks ( (Tok_Int i)) in (t, Value(Int(i)))
      | Some (Tok_Bool b) -> let t = match_token toks ( (Tok_Bool b)) in
                      (t, Value(Bool(b)))
      | Some (Tok_String s) -> let t = match_token toks ( (Tok_String s)) in
                      (t, Value(String(s)))
      | Some (Tok_ID s) -> let t = match_token toks ( (Tok_ID s)) in
                      (t,  ID s)
      | Some Tok_LParen -> let t = match_token toks Tok_LParen in
                          let (t', exp) = (parseExpr t) in
                      (t', exp)
      | _ ->  (raise (InvalidInputException("InvalidInputException")))

  
  and parseLet toks = 
    match lookahead toks with
      | Some Tok_Let -> 
        (
          let t = match_token toks (Tok_Let) in 
          match (lookahead t) with 
          | Some Tok_Rec -> (* rec case *)
            let t' = match_token t Tok_Rec in 
            let idString =
              match (lookahead t') with
              | Some (Tok_ID str) -> str
              | _ -> (raise (InvalidInputException("InvalidInputException"))) in
            let t'' = match_token t' (Tok_ID idString) in
            let t''' = match_token t'' Tok_Equal in
            let (t'''', exp1) = parseExpr t''' in
            let t''''' = match_token (t'''') Tok_In in
            let (t'''''', exp') = parseExpr t''''' in
            let exp2str = match exp' with 
                          | Value(String s) -> s
                          | _ -> (raise (InvalidInputException("InvalidInputException"))) in
            (t'''''', Let( idString, true, exp1, ID((exp2str)) ))

          | Some (Tok_ID idString) -> (*no rec case *)
           let t' = match_token t (Tok_ID idString) in
            let t'' = match_token t' Tok_Equal in
            let (t''', exp1) = parseExpr t'' in
            let t'''' = match_token (t''') Tok_In in
            let (t''''', exp') = parseExpr t'''' in  
            let exp2str = match exp' with 
                          | Value(String s) -> s
                          | _ -> (raise (InvalidInputException("InvalidInputException"))) in 

            (t''''', Let( idString, false, exp1, ID((exp2str)) ))
            
          | _ -> (raise (InvalidInputException("InvalidInputException")))
        )
      | _ -> (raise (InvalidInputException("InvalidInputException")))

  and parseIf toks =
    match (lookahead toks) with 
    | Some Tok_If -> let t = match_token toks Tok_If in
        let (t', exp') = parseExpr t in
        let t'' = match_token t' Tok_Then in
        let (t''', exp''') = parseExpr t'' in
        let t'''' = match_token t''' Tok_Else in
        let (t''''', exp''''') = parseExpr t'''' in

        (t''''', If(exp', exp''', exp''''' ))
    | _ ->  (raise (InvalidInputException("InvalidInputException")))
  
  and parseFunctionExpr toks = 
    match (lookahead toks) with 
    | Some Tok_Fun -> let t = match_token toks Tok_Fun in
                let idString = 
                  match (lookahead t) with 
                  | Some (Tok_ID s) -> s
                  | _ -> (raise (InvalidInputException("InvalidInputException"))) in
                let t'' = match_token t (Tok_ID idString) in
                let t''' = match_token t'' Tok_Arrow in
                let (t'''', expr) = parseExpr t''' in
                (t'''', Fun( idString, expr) )
    | _ -> (raise (InvalidInputException("InvalidInputException")))

  
  and parseFunctionCall toks =
    let (t, exp) = parsePrimary toks in
      match (lookahead t) with 
      (*if theres a subsequent primaryExpr, parsePrimary it, then return FunctionCall *)
      | Some (Tok_Int i) -> let (t', exp') = parsePrimary t in (t', FunctionCall(exp, exp'))
      | Some (Tok_Bool b) -> let (t', exp') = parsePrimary t in (t', FunctionCall(exp, exp'))
      | Some (Tok_String s) -> let (t', exp') = parsePrimary t in (t', FunctionCall(exp, exp'))
      | Some (Tok_ID s) -> let (t', exp') = parsePrimary t in (t', FunctionCall(exp, exp'))
      | Some Tok_LParen -> let (t', exp') = parsePrimary t in (t', FunctionCall(exp, exp'))
      | _ -> (t, exp) (*if theres only 1 primaryExpr, return primary exp *)
  
  and parseUnary toks = 
    match (lookahead toks) with 
    | Some Tok_Not -> (* not UnaryExpr *)
          let t = match_token toks Tok_Not in 
          let (t', expr) = (parseUnary t) in
          (t', Not(expr))
    (* FunctionCallExpr *)
    | Some (Tok_Int i) -> parseFunctionCall toks
    | Some (Tok_Bool b) -> parseFunctionCall toks
    | Some (Tok_String s) -> parseFunctionCall toks
    | Some (Tok_ID s) -> parseFunctionCall toks
    | Some Tok_LParen -> parseFunctionCall toks
    | _ -> (raise (InvalidInputException("InvalidInputException")))
  
  and parseConcat toks = 
    let (t, exp) = parseUnary toks in
    match (lookahead t) with 
    | Some Tok_Concat -> 
      let t' = match_token t Tok_Concat in
      let (t'', exp'') = parseConcat t' in
      (t'', Binop(Concat, exp, exp'') )
    | _ -> (t, exp)
  and parseMult toks = 
    let (t, exp) = parseConcat toks in
    match (lookahead t) with 
    | Some Tok_Mult -> 
        let t' = match_token t Tok_Mult in
        let (t'', exp'') = parseMult t' in
        (t'', Binop(Mult, exp, exp'' ))
    | Some Tok_Div -> 
        let t' = match_token t Tok_Div in
        let (t'', exp'') = parseMult t' in
        (t'', Binop(Div, exp, exp'' ))
    | _ -> (t, exp)
  and parseAdditive toks = 
    let (t, exp) = parseMult toks in
    match (lookahead t) with 
    | Some Tok_Add -> 
      let t' = match_token t Tok_Add in
      let (t'', exp'') = parseAdditive t' in
      (t'', Binop(Add, exp, exp''))
    | Some Tok_Sub -> 
      let t' = match_token t Tok_Sub in
      let (t'', exp'') = parseAdditive t' in
      (t'', Binop(Sub, exp, exp''))
    | _ -> (t, exp)
  and parseRelational toks =
    let (t, exp) = parseAdditive toks in
    match (lookahead t) with 
    | Some Tok_Greater -> 
      let t' = match_token t Tok_Greater in
      let (t'', exp'') = parseRelational t' in
      (t'', Binop(Greater, exp, exp''))
    | Some Tok_Less -> 
      let t' = match_token t Tok_Less in
      let (t'', exp'') = parseRelational t' in
      (t'', Binop(Less, exp, exp''))
    | Some Tok_GreaterEqual -> 
      let t' = match_token t Tok_GreaterEqual in
      let (t'', exp'') = parseRelational t' in
      (t'', Binop(GreaterEqual, exp, exp''))
    | Some Tok_LessEqual -> 
      let t' = match_token t Tok_LessEqual in
      let (t'', exp'') = parseRelational t' in
      (t'', Binop(LessEqual, exp, exp''))
    | _ -> (t, exp)
  and parseEquality toks = 
    let (t, exp) = parseRelational toks in
    match (lookahead t) with 
    | Some Tok_Equal -> 
      let t' = match_token t Tok_Equal in
      let (t'', exp'') = parseEquality t' in
      (t'', Binop(Equal, exp, exp''))
    | Some Tok_NotEqual -> 
      let t' = match_token t Tok_NotEqual in
      let (t'', exp'') = parseEquality t' in
      (t'', Binop(NotEqual, exp, exp''))
    | _ -> (t, exp)
  
  and parseAnd toks = 
    let (t, exp) = parseEquality toks in
    match (lookahead t) with 
    | Some Tok_And -> 
      let t' = match_token t Tok_And in
      let (t'', exp'') = parseAnd t' in
      (t'', Binop(And, exp, exp''))
    | _ -> (t, exp)

  and parseOr toks = 
    let (t, exp) = parseAnd toks in
    match (lookahead t) with 
    | Some Tok_Or -> 
      let t' = match_token t Tok_Or in
      let (t'', exp'') = parseOr t' in
      (t'', Binop(Or, exp, exp''))
    | _ -> (t, exp)
(* Part 3: Parsing mutop *)

let rec parse_mutop toks = failwith "unimplemented"