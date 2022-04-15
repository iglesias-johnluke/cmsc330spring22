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
    let re_subtract = Str.regexp "^-$" in
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
    let re_not = Str.regexp "^not$" in
    let re_if = Str.regexp "^if$" in
    let re_then = Str.regexp "^then$" in
    let re_else = Str.regexp "^else$" in
    let re_mult = Str.regexp "*" in
    let re_div = Str.regexp "/" in
    let re_concat = Str.regexp "\\^" in
    let re_let = Str.regexp "^let$" in
    let re_def = Str.regexp "^def$" in
    let re_in = Str.regexp "^in$" in
    let re_rec = Str.regexp "^rec$" in
    let re_fun = Str.regexp "^fun$" in
    let re_arrow = Str.regexp "->" in
    let re_double_semi = Str.regexp ";;" in
    let re_true = Str.regexp "^true$" in 
    let re_false = Str.regexp "^false$" in 
    let re_string =  Str.regexp "\"[^\"]*\"" in
     let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in 

    let rec tok position wordList = 
        if position >= (List.length wordList) then [] (*end of string, return list *)
        (*handle valid tokens *)
        else 
            let currItem = List.nth wordList position in
            if (Str.string_match re_int currItem 0) then (*Tok_int *)
                let token = int_of_string (Str.matched_string currItem) in
                (Tok_Int token)::(tok (position+1) wordList)
            else if (Str.string_match re_string currItem 0) then (*Tok_string*)
                let itemWithoutQuotes = Str.(global_replace (regexp "\"") "" currItem ) in
                if (itemWithoutQuotes = "") then (raise (InvalidInputException("InvalidInputException")))
                else  (Tok_String itemWithoutQuotes)::(tok (position+1) wordList)
            else if (Str.string_match re_true currItem 0)  then (*true*)
                (Tok_Bool true)::(tok (position+1) wordList)
            else if (Str.string_match re_false currItem 0) then (*false*)
                (Tok_Bool false)::(tok (position+1) wordList)

            else if (Str.string_match re_add currItem 0) then (*Tok_add *)
                Tok_Add::(tok (position+1) wordList)
            else if (Str.string_match re_subtract currItem 0) then (*Tok_sub *)
                Tok_Sub::(tok (position+1) wordList) 
            else if (Str.string_match re_negative_int currItem 0) then (*NEGATIVE Tok_int *)
                let removeParentheses = Str.(global_replace (regexp "[(|)]") "" (Str.matched_string currItem) ) in
                let token = int_of_string (removeParentheses) in
                (Tok_Int (token) )::(tok (position+1) wordList)
            else if ((Str.string_match re_left_paren)  currItem 0) then (*left parentheses *)
                (Tok_LParen::(tok (position+1) wordList))
            else if ((Str.string_match re_right_paren)  currItem 0) then (*right parentheses *)
                (Tok_RParen::(tok (position+1) wordList))
            else if ((Str.string_match re_equals)  currItem 0) then (*equals *)
                (Tok_Equal::(tok (position+1) wordList))
            else if ((Str.string_match re_not_equals)  currItem 0) then (*not equals *)
                (Tok_NotEqual::(tok (position+1) wordList))
            else if ((Str.string_match re_greater_equal)  currItem 0) then (*greater equal*)
                (Tok_GreaterEqual::(tok (position+1) wordList))
            else if ((Str.string_match re_less_equal)  currItem 0) then (*less equal *)
                (Tok_LessEqual::(tok (position+1) wordList))
            else if ((Str.string_match re_greater)  currItem 0) then (*greater *)
                (Tok_Greater::(tok (position+1) wordList))
            else if ((Str.string_match re_less)  currItem 0) then (*less *)
                (Tok_Less::(tok (position+1) wordList))
            else if ((Str.string_match re_or)  currItem 0) then (*or*)
                (Tok_Or::(tok (position+1) wordList))
            else if ((Str.string_match re_and)  currItem 0) then (*and *)
                (Tok_And::(tok (position+1) wordList))
            else if ((Str.string_match re_not)  currItem 0) then (*not*)
                (Tok_Not::(tok (position+1) wordList))
            else if ((Str.string_match re_if)  currItem 0) then (*if *)
                (Tok_If::(tok (position+1) wordList))
            else if ((Str.string_match re_then)  currItem 0) then (*then*)
                (Tok_Then::(tok (position+1) wordList))
            else if ((Str.string_match re_else)  currItem 0) then (*else *)
                (Tok_Else::(tok (position+1) wordList))
            else if ((Str.string_match re_mult)  currItem 0) then (*multiply *)
                (Tok_Mult::(tok (position+1) wordList))
            else if ((Str.string_match re_div)  currItem 0) then (*divide *)
                (Tok_Div::(tok (position+1) wordList))
            else if ((Str.string_match re_concat)  currItem 0) then (*concat *)
                (Tok_Concat::(tok (position+1) wordList))
            else if ((Str.string_match re_let)  currItem 0) then (*let*)
                (Tok_Let::(tok (position+1) wordList))
            else if ((Str.string_match re_def)  currItem 0) then (*def *)
                (Tok_Def::(tok (position+1) wordList))
            else if ((Str.string_match re_in)  currItem 0) then (*in *)
                (Tok_In::(tok (position+1) wordList))
            else if ((Str.string_match re_rec)  currItem 0) then (*rec *)
                (Tok_Rec::(tok (position+1) wordList))
            else if ((Str.string_match re_fun)  currItem 0) then (*fun *)
                (Tok_Fun::(tok (position+1) wordList))
            else if ((Str.string_match re_arrow)  currItem 0) then (*arrow *)
                (Tok_Arrow::(tok (position+1) wordList))
            else if ((Str.string_match re_double_semi)  currItem 0) then (*double semi-colon *)
                (Tok_DoubleSemi::(tok (position+1) wordList))
             
            else if (Str.string_match re_id currItem 0) then (*Tok_id *)
                let token = (Str.matched_string currItem) in
                (Tok_ID token)::(tok (position+1) wordList)
            else if currItem = "" || currItem = "\t" || currItem = "\n" 
                then (tok (position+1) wordList) (*skip empty space *)
            else
                (raise (InvalidInputException("InvalidInputException"))) in (*handle no match of curr item *)
    let rawWordList = Str.split (Str.regexp " +") input in
    let splitParenthesesList = splitParentheses rawWordList in
    let wordList = handleDoubleSemiInLastItem splitParenthesesList in
    tok 0 wordList
