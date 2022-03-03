open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t =   
  match t with 
  | IntLeaf -> IntNode(x, None, IntLeaf, IntLeaf, IntLeaf) (*base leaf node *)
  | IntNode (i, None, leaf1, leaf2, leaf3) -> (*insert int next to other int *)
      if (x < i) then IntNode (x, Some i, leaf1, leaf2, leaf3) 
      else if (x = i) then t
      else IntNode (i, Some x, leaf1, leaf2, leaf3)
  | IntNode (i, i2, tree1, tree2, tree3) -> (*insert tree as a branch *)
      let i2 = match i2 with (*convert i2 to an int *)
        | None -> 0
        | Some i2 -> i2 in 
      if x < i then IntNode (i, Some i2, (int_insert x tree1), tree2, tree3 )
      else if (x > i && x < i2) then IntNode (i, Some i2, tree1, (int_insert x tree2), tree3)
      else IntNode (i, Some i2, tree1, tree2, (int_insert x tree3) )


let rec int_mem x t =
  match t with 
    | IntLeaf -> false
    | IntNode (i, None, leaf1, leaf2, leaf3) -> x = i
    | IntNode (i, i2, tree1, tree2, tree3) ->
      let i2 = match i2 with (*convert i2 to an int *)
        | None -> 0
        | Some i2 -> i2 in
      if i2 = x || i = x then true
      else if (int_mem x tree1) = true || (int_mem x tree2) = true || (int_mem x tree3) = true
            then true
      else false

let rec int_size t =
   match t with 
    | IntLeaf -> 0
    | IntNode (i, None, leaf1, leaf2, leaf3) -> 1 + (int_size leaf1) + (int_size leaf2) + (int_size leaf3) 
    | IntNode (i, i2, tree1, tree2, tree3) ->
        2 + (int_size tree1) + (int_size tree2) + (int_size tree3) 

let rec int_max t = 0
  (* match t with 
    | IntLeaf -> invalid_arg("int_max")
    | IntNode (_, _, _, _, _) -> 
      let rec auxFunction tree currMax = 
        let newMax =
          match tree with 
            | IntLeaf -> currMax
            | IntNode (i, None, tree1, tree2, tree3) -> if i > currMax then i else currMax
            | IntNode (i, i2, tree1, tree2, tree3) ->
                let i2 = match i2 with (*convert i2 to an int *)
                  | None -> 0
                  | Some i2 -> i2 in
                if (i > i2 && i > currMax) then i 
                else if (i < i2 && i2 > currMax) then i2
                else currMax in
            if (auxFunction tree1 currMax) > (auxFunction tree2 currMax) && 
              (auxFunction tree1 currMax) > (auxFunction tree3 currMax)
                then (auxFunction tree1 currMax)
            else if (auxFunction tree2 currMax) > (auxFunction tree1 currMax) && 
              (auxFunction tree2 currMax) > (auxFunction tree3 currMax)
                then (auxFunction tree2 currMax)
            else
              (auxFunction tree3 currMax) *)


(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t =   failwith "unimplemented"
  (* match t with 
  | MapLeaf -> MapNode((k, v), None, MapLeaf, MapLeaf, MapLeaf) (*base leaf node *)
  | MapNode ((otherK, otherV), None, leaf1, leaf2, leaf3) -> (*insert int next to other int *)
      if (k < otherK) then MapNode ((k, v), Some (otherK, otherV), leaf1, leaf2, leaf3) 
      else if (k = otherK) then t
      else MapNode ((otherK, otherV), Some (k, v), leaf1, leaf2, leaf3)
  | MapNode ((otherK, otherV), (otherK2, otherV2), tree1, tree2, tree3) -> (*insert tree as a branch *)
      let (otherK2, otherV2) = match (otherK2, otherV2) with (*convert (otherK2, otherV2) option to an a tuple *)
        | None -> 0
        | Some (otherK2, otherV2) -> (otherK2, otherV2) in 
      if k < otherK then MapNode ((otherK, otherV), Some (otherK2, otherV2), (map_put k v tree1), tree2, tree3 )
      else if (k > otherK && k < otherK2) then MapNode ((otherK, otherV), Some (otherK2, otherV2), tree1, (map_put k v tree2), tree3)
      else MapNode ((otherK, otherV), Some (otherK2, otherV2), tree1, tree2, (map_put k v tree3) ) *)

let rec map_contains k t = 
  failwith "unimplemented"

let rec map_get k t =
  failwith "unimplemented"

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = unit

let empty_table : lookup_table = ()

let push_scope (table : lookup_table) : lookup_table = 
  failwith "unimplemented"

let pop_scope (table : lookup_table) : lookup_table =
  failwith "unimplemented"

let add_var name value (table : lookup_table) : lookup_table =
  failwith "unimplemented"

let rec lookup name (table : lookup_table) =
  failwith "unimplemented"