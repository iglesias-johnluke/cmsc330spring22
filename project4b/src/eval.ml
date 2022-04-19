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
  (* | Binop(op, e1, e2) -> evalBinop(env Binop(op, e1, e2)) *)
  | _ -> (raise (TypeError("No match with current AST item!")))

and evalNot env exp = 
  match (eval_expr env exp) with
  | Bool b -> Bool (Bool.not b)
  | _ -> (raise (TypeError("Expected type bool for evalNot")))

and evalValue env value = 
  match value with
  | Int(i) -> Int i
  |  Bool(b) -> Bool(b)
  |  String(s) -> String s
  |  Closure(env, v, exp) -> Closure(env, v, exp)

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = failwith "unimplemented"