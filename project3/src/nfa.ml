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
let getSet (lis : 'q list) = 
  fold (fun acc currItem -> insert currItem acc) [] lis

(* returns first element in list or None if list is empty *)
let getFirst (lis : 'q list)  = List.hd(lis)

let removeFirst (lis : 'q list) = match lis with
  | [] -> []
  | [a] -> []
  | a::b -> b



let nfa_ex = {
    sigma = ['a'];
    qs = [0; 1; 2];
    q0 = 0;
    fs = [2];
    delta = [(1, None, 2); (0, None, 1 )]
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

(* returns list containing all rawOutput values along with
any other states that were reachable from epsilon transitions from the initial
values *)
let rec getAllEpsilonTransitions (nfa: ('q,'s) nfa_t) (rawOutputLeft: 'q list) 
                                    (output : 'q list) : 'q list = 
  if rawOutputLeft = [] then output
  else  
    let currState =  (getFirst rawOutputLeft) in
    (*newStates is states list reachable from currState thatre NOT in output already *)
    let newStates = fold (fun acc currTuple -> 
                            (* let nextStates = (getEndState currTuple currState None []) in *)
                            let nextStates = insert currState (move nfa [currState] None) in
                            let newStates = diff nextStates (output) in
                            acc@newStates
                      ) [] nfa.delta in
    let rawOutputLeft = diff (union rawOutputLeft newStates) [currState] in 
    let output = union output newStates in (*output now has new states and rawOutputLeft has newStates w/o currState *)
    getAllEpsilonTransitions nfa rawOutputLeft output


let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = getAllEpsilonTransitions nfa qs qs
   
(* returns false if state and transition symbol can move to a DIFFERENT STATE
 within the NFA, true ow. However if state is an ENDINGSTATE in nfa, returns false*)
 let isStateStuck (nfa: ('q,char) nfa_t) (strList : char list) state = 
  if (elem state nfa.fs) then false (*state is valid ending state *)
  else 
    let nextStateList =  insert_all (move nfa [state] (Some (getFirst strList)) ) (e_closure nfa [state]) in 
    nextStateList = [state] (*return whether this state can move to a DIFFERENT STATE *)


let accept (nfa: ('q, char) nfa_t) (s: string) : bool = 
  let stringList = explode s in
  let rec acceptAux nfa (strList : char list) state =
    (*base case, when at end of strList, check if e-closure passes* *)
    if strList = [] then 
      let endingStates = intersection (nfa.fs) (e_closure nfa [state]) in
      List.length(endingStates) > 0
    else if (isStateStuck nfa strList state) then false (*if state != endstate and cannot move to DIFFERENT state, deny *)
    else 
    (*nextStateList is list of next possible states reachable from currState 
    and first letter of strList as the transition, list excludes currState *)
      let nextEpsilonStates = (e_closure nfa [state]) in
      let nextStateList = remove state ( move nfa nextEpsilonStates (Some (getFirst strList)) ) in
      fold (fun acc currState -> (*loop thru each possible next state *)
                if (acceptAux nfa (removeFirst strList) currState) = true && (acc = false) then true (*target found *)
                else acc
            ) false nextStateList
  in acceptAux nfa stringList nfa.q0

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if qs = [] then (List.map (fun letter -> []) nfa.sigma)
  else
    fold (fun acc letter -> 
                  (*if currLetter transition leads to deadstate, add [] *)
                      if (move nfa qs (Some letter)) = [] then acc@[[]]
                      else 
                        let nextEpsilonStates = (e_closure nfa qs) in
                        let nextStateList = e_closure nfa (move nfa nextEpsilonStates (Some letter) ) in
                        acc@[nextStateList]
        ) [] nfa.sigma



let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.map (fun letter -> 
                    let nextStateList = e_closure nfa (move nfa qs (Some letter) ) in
                    (qs, Some letter, nextStateList) 
            ) nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if (List.length (intersection qs nfa.fs) ) > 0 then [qs]
  else []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t = 
  
  if work = [] then dfa (*base case *)
  else 
    let r = getFirst work in
    (*eList is list of tuples, each tuple is (e, letter) *)
    let eList = List.map (fun letter ->
                                 let e = e_closure nfa (move nfa r (Some letter)) in
                                 (e, letter)
                          ) dfa.sigma in
    (*unmarkedR is list of states in R we havent market yet,
    here we're adding any new states from the
    current e_closure *)
    let unmarkedR = fold (fun acc currTuple ->
                            match currTuple with (e, letter) -> 
                                if e = [] then acc (*handle empty e in tuple *)
                                (*dont add e to unmarkedR if it is already a state in dfa *)
                                else if List.length (intersection [e] dfa.qs) > 0 then acc 
                                else acc@[e] (*ow e is a new state, add to unmarkedR *)
                          ) work eList in
    (* allR (R) is (R U {e}) by looping over eList tuples, doing R U e *)
    let allR = fold (fun acc currTuple ->
                                match currTuple with (e, letter) -> 
                                  if e = [] then acc (*dont add [] to R *)
                                  else (union acc [e])
                   ) dfa.qs eList in
    (*update new delta of dfa to be (delta U (r, letter, e)) by looping over eList *)
    let delta = fold (fun acc currTuple ->
                                match currTuple with (e, letter) -> 
                                  if e = [] then acc
                                  else (union acc [(r, Some letter, e)])
                      ) dfa.delta eList in
    let dfa = { 
      qs= allR (*update dfa *)
      ; sigma= dfa.sigma
      ; delta= union dfa.delta delta
      ; q0= dfa.q0
      ; fs= dfa.fs } in
    nfa_to_dfa_step nfa dfa (removeFirst unmarkedR) (*recursive call with updated R/work and dfa*)
    

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t = 
  let dfa = { 
    qs= [e_closure nfa [nfa.q0]]
    ; sigma= nfa.sigma
    ; delta= []
    ; q0= e_closure nfa [nfa.q0]
    ; fs= [] } in
  (nfa_to_dfa_step nfa dfa [e_closure nfa [nfa.q0]])(*work/R initialized as e-closure of q0 *)
