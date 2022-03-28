open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

(*aux function returns endState if a 3-arg tuple has matching
starting state and transition symbol, None ow *)
let getEndState (tup : ('q * 's option * 'q)) (startState : 'q) (symbol: 's option) = 
  match tup with 
  | (otherStartState, Some otherSymbol, endState) -> 
      if (startState = otherStartState) && (symbol = Some otherSymbol) then Some endState
      else None
  | (otherStartState, None, endState) -> 
      if (startState = otherStartState) && (symbol = None) then Some endState
      else None


let rec fold f a xs = match xs with
| [] -> a
| x :: xt -> fold f (f a x) xt


(* if symbol is None, then check in delta where there is a None symbol and 
matching start state. 
Check if symbol is in alphabet, if so then check delta for
tuple with matching start state + transition symbol, return end state in tuple *)
let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let output = [] in 
  let iterator = List.iter (fun currState -> (*loop thru states in qs *)
                              (*fold over nfa.delta, appending matching
                              ending states to output list *)
                              fold (fun acc currTuple -> 
                                  if (getEndState currTuple currState s) != None then 
                                  output@[getEndState currTuple currState s]
                                  else output
                                  ) output nfa.delta  
                              
                            ) qs 
  in output

  

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  failwith "unimplemented"

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  failwith "unimplemented"

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  failwith "unimplemented"

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  failwith "unimplemented"

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  failwith "unimplemented"

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  failwith "unimplemented"
