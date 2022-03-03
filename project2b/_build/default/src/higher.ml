open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = 
    let countif isEqual list = 
        fold (fun counter element ->
            if isEqual element then counter+1
            else counter) 0 lst in
    let count = countif (fun item -> item = e) lst in 
    count > 0

let is_present lst x = 
    let isMatch item = if item = x then 1
        else 0 in 
    map isMatch lst

let count_occ lst target = 
    let countif isEqual list = 
        fold (fun counter element ->
            if isEqual element then counter+1
            else counter) 0 lst in
    countif (fun item -> item = target) lst

let uniq lst = 
    let prepend newList item = if (contains_elem newList item) then newList
        else item::newList in
    fold prepend [] lst

let assoc_list lst = 
    let uniqueList = uniq lst in
    let getTuple item = (item, (count_occ lst item) ) in 
     map getTuple uniqueList

let ap fns args =  failwith "unimplemented"

(* if (length fns = 0) || (length args = 0) then []
    else 
        fold_right (fun func -> (map func args) ) fns []*)