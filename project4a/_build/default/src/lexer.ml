open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

(*returns true if s2 is a substring of s1, false ow *)
let contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false

(*given a list of words, if last item is for example "item;;", returns new list
where last item is replaced with "item", ";;" *)
let handleDoubleSemiInLastItem lis = 
    let length = List.length lis in
    let lastItem = List.nth lis (length - 1) in
    if (contains lastItem ";;") = false then lis (*only process if last item has ;; *)
    else
        let lastItemWithoutSemi = Str.(global_replace (regexp ";;") "" lastItem ) in
        let rec listCopyWithoutLastItem lis = 
            match lis with 
                | [] -> []
                | [item] -> []
                | h::t -> [h] @ (listCopyWithoutLastItem t) in
        (listCopyWithoutLastItem lis) @ [lastItemWithoutSemi] @ [";;"]

(*given a list of strings, if a string item is in format "(item" or "item)",
returns list where parentheses are their own items *)
let rec splitParentheses lis = 
    match lis with 
    | [] -> []
    | [h] -> (*if item starts with "(" and length > 1, then split further *)
        if (h.[0] = '(' && ((String.length h) > 1)) then  
            let lastIndex = (String.length h) - 1 in
            "(" :: splitParentheses([String.sub h 1 (lastIndex)])
        (*if last char in item is ) and item length > 1, then split *)
        else if ((String.length h) > 1) && h.[(String.length h) - 1] = ')'   then
            let length = (String.length h) - 1 in
            (String.sub h 0 length) :: ")" :: []
        else [h]
    | h::t -> 
        (*if first char is "(" *)
        if (h.[0] = '(' && ((String.length h) > 1)) then  
            let length = (String.length h) - 1 in
            ["("] @ splitParentheses([String.sub h 1 length]) @ splitParentheses t
        (*if last char in item is ) and item length > 1, then split *)
        else if ((String.length h) > 1) && h.[(String.length h) - 1] = ')' then
            let length = (String.length h) - 1 in
            [(String.sub h 0 length)] @ [")"] @ (splitParentheses t)
        else h::(splitParentheses t)

let tokenize input = 
    let re_int = Str.regexp "[0-9]+" in(* single digit *) 
    let re_add = Str.regexp "+" in
    let re_negative_int = Str.regexp "(-[0-9])" in
    let re_subtract = Str.regexp "-" in
    let re_left_paren = Str.regexp "(" in
    let re_right_paren = Str.regexp ")" in
    let re_equals = Str.regexp "=" in
    let re_not_equals = Str.regexp "<>" in
    let re_greater = Str.regexp ">" in
    let re_less = Str.regexp "<" in
    let re_greater_equal = Str.regexp ">=" in
    let re_less_equal = Str.regexp "<=" in
    let re_or = Str.regexp "||" in
    let re_and = Str.regexp "&&" in
    let re_not = Str.regexp "not " in
    let re_if = Str.regexp "if" in
    let re_then = Str.regexp "then" in
    let re_else = Str.regexp "else" in
    let re_mult = Str.regexp "*" in
    let re_div = Str.regexp "/" in
    let re_concat = Str.regexp "\\^" in
    let re_let = Str.regexp "let" in
    let re_def = Str.regexp "def" in
    let re_in = Str.regexp "in" in
    let re_rec = Str.regexp "^rec$" in
    let re_fun = Str.regexp "fun" in
    let re_arrow = Str.regexp "->" in
    let re_double_semi = Str.regexp ";;" in
    let re_true = Str.regexp "true" in 
    let re_false = Str.regexp "false" in 
    let re_string =  Str.regexp "\"[^\"]*\"" in
     let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in  

    let rec tok position input = 
        if position >= (String.length input) then [] (*end of string, return list *)
        (*handle valid tokens *)
        else 
            if (Str.string_match re_int input position) then (*Tok_int *)
                let token = int_of_string (Str.matched_string input) in
                let tokenLength = String.length (Str.matched_string input) in
                (Tok_Int token)::(tok (position+tokenLength) input)
            else if ((Str.string_match re_arrow)  input position) then (*arrow *)
                (Tok_Arrow::(tok (position+2) input))

            else if (Str.string_match re_string input position) then (*Tok_string*)
                let token = (Str.matched_string input) in
                let tokenLength = String.length (Str.matched_string input) in
                let itemWithoutQuotes = Str.(global_replace (regexp "\"") "" token ) in
                if (itemWithoutQuotes = "") then (raise (InvalidInputException("InvalidInputException")))
                else  (Tok_String itemWithoutQuotes)::(tok (position+tokenLength) input)
            else if (Str.string_match re_true input position)  then (*true*)
                (Tok_Bool true)::(tok (position+4) input)
            else if (Str.string_match re_false input position) then (*false*)
                (Tok_Bool false)::(tok (position+5) input)
           
            else if (Str.string_match re_add input position) then (*Tok_add *)
                Tok_Add::(tok (position+1) input)
            
            else if (Str.string_match re_subtract input position) then (*Tok_sub *)
                Tok_Sub::(tok (position+1) input) 
            else if (Str.string_match re_negative_int input position) then (*NEGATIVE Tok_int *)
                let tokenLength = String.length (Str.matched_string input) in
                let removeParentheses = Str.(global_replace (regexp "[(|)]") "" (Str.matched_string input) ) in
                let token = int_of_string (removeParentheses) in
                (Tok_Int (token) )::(tok (position+tokenLength) input)
            else if ((Str.string_match re_left_paren)  input position) then (*left parentheses *)
                (Tok_LParen::(tok (position+1) input))
            else if ((Str.string_match re_right_paren) input position) then (*right parentheses *)
                (Tok_RParen::(tok (position+1) input))
            else if ((Str.string_match re_equals)  input position) then (*equals *)
                (Tok_Equal::(tok (position+1) input))
            else if ((Str.string_match re_not_equals)  input position) then (*not equals *)
                (Tok_NotEqual::(tok (position+2) input))
            else if ((Str.string_match re_greater_equal)  input position) then (*greater equal*)
                (Tok_GreaterEqual::(tok (position+2) input))
            else if ((Str.string_match re_less_equal)  input position) then (*less equal *)
                (Tok_LessEqual::(tok (position+2) input))
            else if ((Str.string_match re_greater)  input position) then (*greater *)
                (Tok_Greater::(tok (position+1) input))
            else if ((Str.string_match re_less)  input position) then (*less *)
                (Tok_Less::(tok (position+1) input))
            else if ((Str.string_match re_or)  input position) then (*or*)
                (Tok_Or::(tok (position+2) input))
            else if ((Str.string_match re_and)  input position) then (*and *)
                (Tok_And::(tok (position+2) input))
            else if ((Str.string_match re_not)  input position) then (*not*)
                let tokenLength = String.length (Str.matched_string input) in
                (Tok_Not::(tok (position+tokenLength) input))
            else if ((Str.string_match re_if)  input position) then (*if *)
                (Tok_If::(tok (position+2) input))
            else if ((Str.string_match re_then)  input position) then (*then*)
                (Tok_Then::(tok (position+4) input))
            else if ((Str.string_match re_else)  input position) then (*else *)
                (Tok_Else::(tok (position+4) input))
            else if ((Str.string_match re_mult)  input position) then (*multiply *)
                (Tok_Mult::(tok (position+1) input))
            else if ((Str.string_match re_div)  input position) then (*divide *)
                (Tok_Div::(tok (position+1) input))
            else if ((Str.string_match re_concat)  input position) then (*concat *)
                (Tok_Concat::(tok (position+1) input))
            else if ((Str.string_match re_let)  input position) then (*let*)
                (Tok_Let::(tok (position+3) input))
            else if ((Str.string_match re_def)  input position) then (*def *)
                (Tok_Def::(tok (position+3) input))
            else if ((Str.string_match re_in)  input position) then (*in *)
                (Tok_In::(tok (position+2) input))
            else if ((Str.string_match re_rec)  input position) then (*rec *)
                (Tok_Rec::(tok (position+3) input))
            else if ((Str.string_match re_fun)  input position) then (*fun *)
                (Tok_Fun::(tok (position+3) input))
            else if ((Str.string_match re_double_semi)  input position) then (*double semi-colon *)
                (Tok_DoubleSemi::(tok (position+2) input))
             
            else if (Str.string_match re_id input position) then (*Tok_id *)
                let token = (Str.matched_string input) in
                let tokenLength = String.length (Str.matched_string input) in
                (Tok_ID token)::(tok (position+tokenLength) input)
            
            
            else if input.[position] = ' ' || input.[position] = '\t' || input.[position] = '\n' 
                then (tok (position+1) input) 
            else
                (raise (InvalidInputException("No matching regexes"))) in (*handle no match of curr item *)
    tok 0 input
