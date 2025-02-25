open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)


(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with 
  | Value(value) -> evalValue env value
  | ID str -> lookup env str
  | Not exp -> evalNot env exp
  | Binop(op, e1, e2) -> evalBinop env op e1 e2
  | If(e1, e2, e3) -> evalIf env e1 e2 e3
  | Let(v, b, e1, e2) -> evalLet env v b e1 e2
  | Fun(v, exp1) -> evalFun env v exp1
  | FunctionCall(e1, e2) -> evalFunCall env e1 e2



and evalFunCall env e1 e2 = 
  let closure_e1 = (*eval e1 as a closure *)
      match (eval_expr env e1) with
        | Closure(environment, x, exp) -> Closure(environment, x, exp)
        | _ -> (raise (TypeError("Expected Closure type upon (eval_expr e1) within evalFunCall")))
   in
  let environment = 
    match closure_e1 with 
    | Closure(environment, x, exp) -> environment
    | _ -> (raise (TypeError("Expected Closure type upon (eval_expr e1) within evalFunCall"))) in
  let x = 
    match closure_e1 with 
    | Closure(environment, x, exp) -> x
    | _ -> (raise (TypeError("Expected Closure type upon (eval_expr e1) within evalFunCall"))) in
  let exp = 
    match closure_e1 with 
    | Closure(environment, x, exp) -> exp
    | _ -> (raise (TypeError("Expected Closure type upon (eval_expr e1) within evalFunCall"))) in

  let v = (*eval e2 as a value v *)
    match (eval_expr env e2) with
      | Int(i) -> Int(i)
      |  Bool(b) -> Bool(b)
      |  String(s) -> String s
      |  Closure(env, v, exp) -> Closure(env, v, exp) 
   in
  (*evaluate e (the closure's body) in environment A (the closure's environment), returning the result.*)
  let newEnv = (extend environment x v) in
  (eval_expr newEnv exp) 

and evalFun env v exp1 = Closure(env, v, exp1)
and evalLet env x b e1 e2 =
  match b with 
  (*not recursive case *)
  | false -> 
    let newEnv = extend env x (eval_expr env e1) in (*extend env with x:v map *)
    eval_expr newEnv e2
  (*recursive case *)
  | true -> 
    let newEnv = extend_tmp env x in
    let v = eval_expr newEnv e1 in
    let _ = update newEnv x v in
    eval_expr newEnv e2

and evalIf env e1 e2 e3 = 
  match (eval_expr env e1) with 
  | Bool(b) -> if b = true then (eval_expr env e2) else (eval_expr env e3)
  | _ -> (raise (TypeError("Expected 1st argument of If to be type Bool")))

and evalBinop env op exp1 exp2 = 
  match op with 
  | Add -> 
    (
      let int1 = 
        match (eval_expr env exp1) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp1) to be type Int in evalBinop"))) in
      let int2 = 
        match (eval_expr env exp2) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp2) to be type Int in evalBinop"))) in
      Int(int1 + int2)
    )
  | Sub -> 
    (
      let int1 = 
        match (eval_expr env exp1) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp1) to be type Int in evalBinop"))) in
      let int2 = 
        match (eval_expr env exp2) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp2) to be type Int in evalBinop"))) in
      Int(int1 - int2)
    )
  | Mult -> 
    (
      let int1 = 
        match (eval_expr env exp1) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp1) to be type Int in evalBinop"))) in
      let int2 = 
        match (eval_expr env exp2) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp2) to be type Int in evalBinop"))) in
      Int(int1 * int2)
    )
  | Div -> 
    (
      let int1 = 
        match (eval_expr env exp1) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp1) to be type Int in evalBinop"))) in
      let int2 = 
        match (eval_expr env exp2) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp2) to be type Int in evalBinop"))) in
      if int2 != 0 then Int(int1 / int2) else (raise DivByZeroError)
    )
  | Greater -> 
    (
      let int1 = 
        match (eval_expr env exp1) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp1) to be type Int in evalBinop"))) in
      let int2 = 
        match (eval_expr env exp2) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp2) to be type Int in evalBinop"))) in
      Bool (int1 > int2)
    )
  | Less -> 
    (
      let int1 = 
        match (eval_expr env exp1) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp1) to be type Int in evalBinop"))) in
      let int2 = 
        match (eval_expr env exp2) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp2) to be type Int in evalBinop"))) in
      Bool (int1 < int2)
    )
  | GreaterEqual -> 
    (
      let int1 = 
        match (eval_expr env exp1) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp1) to be type Int in evalBinop"))) in
      let int2 = 
        match (eval_expr env exp2) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp2) to be type Int in evalBinop"))) in
      Bool (int1 >= int2)
    )
  | LessEqual -> 
    (
      let int1 = 
        match (eval_expr env exp1) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp1) to be type Int in evalBinop"))) in
      let int2 = 
        match (eval_expr env exp2) with
        | Int i -> i
        | _ -> (raise (TypeError("Expected (eval_expr env exp2) to be type Int in evalBinop"))) in
      Bool (int1 <= int2)
    )
  | Concat -> 
    (
      let str1 = 
        match (eval_expr env exp1) with
        | String s -> s
        | _ -> (raise (TypeError("Expected (eval_expr env exp1) to be type String in evalBinop"))) in
      let str2 = 
        match (eval_expr env exp2) with
        | String s -> s
        | _ -> (raise (TypeError("Expected (eval_expr env exp2) to be type String in evalBinop"))) in
      String (String.concat str1 [""; str2])
    )
  | Equal -> 
    (
      match (eval_expr env exp1) with 
      | Int(i) -> 
        (
        match (eval_expr env exp2) with 
          
            | Int(i2) -> Bool( Int(i) = Int(i2) )
            | _ -> (raise (TypeError("Expected 2nd arg of Binop to be type op Int Equal")))
        )
      | String(i) -> 
        (
        match (eval_expr env exp2) with 
            | String(i2) -> Bool( String(i) = String(i2) )
            | _ -> (raise (TypeError("Expected 2nd arg of Binop to be type String for op Equal")))
        )
      | Bool(i) ->  Bool( Bool(i) = (eval_expr env exp2) )
      | _ -> (raise (TypeError("Expected type Int, String, or Bool as 2nd arg for Equal operator in Binop")))
    )
  | NotEqual -> 
    (
      match (eval_expr env exp1) with 
      | Int(i) -> 
        (
        match (eval_expr env exp2) with 
          
            | Int(i2) -> Bool( Int(i) != Int(i2) )
            | _ -> (raise (TypeError("Expected 2nd arg of Binop to be type op Int Equal")))
        )
      | String(i) -> 
        (
        match (eval_expr env exp2) with 
            | String(i2) -> Bool( String(i) != String(i2) )
            | _ -> (raise (TypeError("Expected 2nd arg of Binop to be type String for op Equal")))
        )
      | Bool(i) ->  Bool( Bool(i) != (eval_expr env exp2) )
      | _ -> (raise (TypeError("Expected type Int, String, or Bool as 2nd arg for Equal operator in Binop")))
    )
  | Or ->
    (
      let bool1 = 
        (
        match (eval_expr env exp1) with
        | Bool(b) -> b
        | _ -> (raise (TypeError("Expected bool for argument in Or operator"))) 
        ) in
      let bool2 =
        (
        match (eval_expr env exp2) with
        | Bool(b) -> b
        | _ -> (raise (TypeError("Expected bool for argument in Or operator"))) 
        ) in
      Bool( bool1 || bool2)
    )

  | And ->
    (
      let bool1 =
        (
        match (eval_expr env exp1) with
        | Bool(b) -> b
        | _ -> (raise (TypeError("Expected bool for argument in And operator"))) 
        ) in
      let bool2 = 
        (
        match (eval_expr env exp2) with
        | Bool(b) -> b
        | _ -> (raise (TypeError("Expected bool for argument in And operator"))) 
        ) in
      Bool( bool1 && bool2)

    )
    
    
  

and evalNot env exp = 
  match (eval_expr env exp) with
  | Bool b -> Bool (Bool.not b)
  | _ -> (raise (TypeError("Expected type bool for evalNot")))

and evalValue env value = 
  match value with
  |  Int(i) -> Int i
  |  Bool(b) -> Bool(b)
  |  String(s) -> String s
  |  Closure(env, v, exp) -> Closure(env, v, exp)

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  | NoOp -> ([], None)
  | Expr(e) -> (env, Some (eval_expr env e))
  | Def(x, e) -> 
      let newEnv = extend_tmp env x in
      let v = eval_expr env e in
      let _ = update newEnv x v in
      (newEnv, Some v)
