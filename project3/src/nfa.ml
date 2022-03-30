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
let getEndState (tup : ('q * 's option * 'q)) (startState : 'q) (symbol: 's option) (lis : 'q list) = 
  match tup with 
  | (otherStartState, Some otherSymbol, endState) -> 
      if (startState = otherStartState) && (symbol = Some otherSymbol) then lis@[endState]
      else lis
  | (otherStartState, None, endState) -> 
      if (startState = otherStartState) && (symbol = None) then lis@[endState]
      else lis

(* given a list of start states (rawOutput), returns new list with any additional
valid epsilon transition states *)  

let rec fold f a xs = match xs with
   [] -> a
  | x :: xt -> fold f (f a x) xt

(*  returns new list with duplicates removed*)
let getSet lis = 
  fold (fun acc currItem -> insert currItem acc) [] lis

(* returns list containing all rawOutput values along with
any other states that were reachable from epsilon transitions from the initial
values *)
let getAllEpsilonTransitions (nfa: ('q,'s) nfa_t) (rawOutput: 'q list) : 'q list = 
  let output = rawOutput in
  fold (fun acc currState -> (*loop over states in rawOutput list *)
            (*loop over nfa's transition tuples, add to output any matches
            where state transitions to another state thru e-closure *)
            acc @ (fold (fun a currTuple -> a @ (getEndState currTuple currState None a)) [] nfa.delta)
        ) output rawOutput 

let nfa_ex = {
    sigma = ['a'];
    qs = [0; 1; 2];
    q0 = 0;
    fs = [2];
    delta = [(1, None, 2); (2, None, 3)]
}

(* if symbol is None, then check in delta where there is a None symbol and 
matching start state. 
Check if symbol is in alphabet, if so then check delta for
tuple with matching start state + transition symbol, return end state in tuple *)
let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = 
  let output = [] in
  let rawOutput = fold (fun acc currState -> 
            acc @ (fold (fun acc currTuple -> 
                      acc @ (getEndState currTuple currState s output) 
                      ) output nfa.delta
                  )
        ) output qs in
  getSet rawOutput

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
    let output = [] in
    let rawOutput = fold (fun acc currState -> (*loop over start states *)
              (* loop over nfa tuples, appending matches to output*)
              acc @ [currState] @ (fold (fun acc currTuple -> 
                        acc @ (getEndState currTuple currState None output) 
                        ) output nfa.delta
                    )
          ) output qs in
    (*allTransitions adds any states to output that are reachable from MULTIPLE 
    epsilon transitions *)
    let allTransitions = getAllEpsilonTransitions nfa rawOutput in
    getSet allTransitions (*remove duplicates *)

(* returns first element in list or None if list is empty *)
let getFirst lis = match lis with 
  | [] -> None
  | a::b -> Some a

let removeFirst lis = match lis with
  | [] -> []
  | [a] -> []
  | a::b -> b

(* returns false if state and transition symbol can move to a DIFFERENT STATE
 within the NFA, true ow. However if state is an ENDINGSTATE in nfa, returns false*)
 let isStateStuck (nfa: ('q,char) nfa_t) strList state = 
  if (elem state nfa.fs) then false (*state is valid ending state *)
  else 
    let nextStateList =  insert_all (move nfa [state] (getFirst strList)) (e_closure nfa [state]) in 
    nextStateList = [state] (*return whether this state can move to a DIFFERENT STATE *)


let accept (nfa: ('q,char) nfa_t) (s: string) : bool = 
  let stringList = explode s in
  let rec acceptAux nfa strList state =
    if strList = [] then (elem state nfa.fs) (*base case, when at end of strList* *)
    else if (isStateStuck nfa strList state) then false (*if state != endstate and cannot move to DIFFERENT state, deny *)
    else 
    (*nextStateList is list of next possible states reachable from currState 
    and first letter of strList as the transition, list excludes currState *)
      let nextStateList = remove state (insert_all (move nfa [state] (getFirst strList)) (e_closure nfa [state]) ) in
      fold (fun acc currState -> (*loop thru each possible next state *)
                if (acceptAux nfa (removeFirst strList) currState) = true && (acc = false) then true (*target found *)
                else acc
            ) false nextStateList
  in acceptAux nfa stringList nfa.q0

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
