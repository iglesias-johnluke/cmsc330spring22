(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

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

let rec fibonacci n = failwith "unimplemented"

let rec pow x y = failwith "unimplemented"

let rec log x y = failwith "unimplemented"

let rec gcf x y = failwith "unimplemented"

let rec is_prime x = failwith "unimplemented"

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = failwith "unimplemented"

let larger lst1 lst2 = failwith "unimplemented"

let reverse lst = failwith "unimplemented"

let rec combine lst1 lst2 = failwith "unimplemented"

let rec merge lst1 lst2 = failwith "unimplemented"

let rec rotate shift lst = failwith "unimplemented"

let rec is_palindrome lst = failwith "unimplemented"