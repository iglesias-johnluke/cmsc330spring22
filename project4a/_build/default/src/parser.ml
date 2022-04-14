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
  if t <> [] then (*if there's still unprocessed tokens, fail *)
    (raise (InvalidInputException("InvalidInputException")))
  else (*if all tokens processed, return AST expression *)
    ([], exp)
  (*parses all non-terminals and terminals*)
  and parseExpr toks = 
    match (lookahead toks) with
    | Some Tok_Let -> parseLet toks
    | Some Tok_If -> parseIf toks
    | Some token -> parsePrimary toks (*parse primary/terminals *)
    | _ ->  (raise (InvalidInputException("InvalidInputException")))

    (* | Tok_Fun -> parseFun toks
    | Tok_Or -> parseOr toks *)

  (* Tok_Int | Tok_Bool | Tok_String | Tok_ID | ( Expr ) *)
  and parsePrimary toks = 
    match lookahead toks with 
      | Some (Tok_Int i) -> let t = match_token toks ( (Tok_Int i)) in
                      (t, Value(Int(i)))
      | Some (Tok_Bool b) -> let t = match_token toks ( (Tok_Bool b)) in
                      (t, Value(Bool(b)))
      | Some (Tok_String s) -> let t = match_token toks ( (Tok_String s)) in
                      (t, Value(String(s)))
      | Some (Tok_ID s) -> let t = match_token toks ( (Tok_ID s)) in
                      (t, Value(String s))
      | Some Tok_LParen -> let t = match_token toks Tok_LParen in
                          let (t', exp) = (parseExpr t) in
                      (t', exp)
      | _ ->  (raise (InvalidInputException("InvalidInputException")))

  
  and parseLet toks = 
    match lookahead toks with
      | Some Tok_Let -> 
        let t = match_token toks (Tok_Let) in
        let idString = match lookahead t with 
            | Some (Tok_ID str ) -> str
            |  _ -> (raise (InvalidInputException("InvalidInputException"))) in
        let t' = match_token t (Tok_ID(idString)) in
        let t'' = match_token t' Tok_Equal in
        let boolItem = match lookahead t'' with
            | Some (Tok_Bool b) -> b 
            | _ -> (raise (InvalidInputException("InvalidInputException"))) in
        let t''' = match_token t'' (Tok_Bool boolItem) in
        let (t'''', exp) = parseExpr t''' in 
        let (t''''', exp') = parseExpr t'''' in 

        (t''''', Let( idString, boolItem, exp, exp' ))

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




(* Part 3: Parsing mutop *)

let rec parse_mutop toks = failwith "unimplemented"