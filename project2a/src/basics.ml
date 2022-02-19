(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)
open Format

let rev_tup tup = let (a, b, c) = tup in (c, b, a)

let is_odd x = 
    if (x mod 2) = 0 then
        false
    else
        true

let area x y = 
    let (lengthStart, _) = x in
    let (lengthEnd, _) = y in
    let ( _ , heightStart) = x in
    let ( _ , heightEnd) = y in
    abs(lengthStart - lengthEnd) * abs(heightStart - heightEnd)


let volume x y = 
    let (lengthStart, _, _ ) = x in
    let (lengthEnd, _, _ ) = y in
    let ( _ , heightStart, _ ) = x in
    let ( _ , heightEnd, _) = y in
    let ( _ , _ , widthStart) = x in
    let ( _ , _, widthEnd) = y in

    abs(lengthStart - lengthEnd) * abs(heightStart - heightEnd) 
        * abs(widthStart - widthEnd)


(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
    if n = 1 then
        1
    else if n = 2 then
        1
    else
        fibonacci (n-1) + fibonacci (n-2)

let rec pow x y = 
    if y = 1 then
        x
    else
        x * pow x (y-1)

let rec log x y = 
    if (y / x) = 1 then
        1
    else if y < x then
        0
    else
        1 + log x (y/x)
    
let rec gcf x y = 
    if y = 0 then
        x
    else
        gcf y (x mod y)

let rec isPrimeAux num currDivisor =
    if num = 1 then
        false
    else if currDivisor >= num then
        true
    else if (num mod currDivisor) = 0 then
        false
    else
        isPrimeAux num (currDivisor+1)

let rec is_prime x = 
    isPrimeAux x 2


(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_aux idx lst curr = 
    if idx = curr then
        match lst with
            | [] -> failwith "Out of bounds"
            | (first::_) -> first
    else
        match lst with
            | [] -> failwith "Out of bounds"
            | (_::rest) -> get_aux idx rest (curr+1)

let rec get idx lst = get_aux idx lst 0


let rec getLength lst total =
    match lst with
            | [] -> total
            | (_::rest) -> getLength rest (total+1)

let larger lst1 lst2 = 
    if (getLength lst1 0) = (getLength lst2 0) then lst1 else
    if (getLength lst1 0) > (getLength lst2 0) then lst1 else lst2

let rec reverseAux lst (newList:'a list) = 
    match lst with
        | [] -> newList
        | (first::rest) -> reverseAux rest (first::newList)

let reverse lst = reverseAux lst []


let rec combine lst1 lst2 = 
    match lst1 with 
        [] -> lst2  (*list1 empty, return list2, which appends list2 to list1 *)
        (* recursively reach list 1's last element, then base case appends list2*)
        | (first :: rest) -> (first :: (combine rest lst2) ) 

let rec merge lst1 lst2 = 
    lst2

let rec insert_at_end l i =
  match l with
    [] -> [i]
  | h :: t -> h :: (insert_at_end t i)

let rec getSecondHalf shift oldList newList currIndex =
    if currIndex = shift - 1 then
        newList
    else
        let newList = insert_at_end(newList (get currIndex oldList)) in
            (getSecondHalf shift oldList newList (currIndex+1))


let rec rotate shift lst = 
    let output = getSecondHalf (shift lst [] 0) in
    output

let rec is_palindrome lst = 
    let reversedList = reverse lst in
    lst = reversedList