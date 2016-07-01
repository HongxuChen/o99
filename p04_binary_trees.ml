open P01_list;;
  
type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

(* Construct completely balanced binary trees *)

let add_trees_with left right all =
  let add_right_tree all l =
    List.fold_left (fun a r -> Node('x', l, r)::a) all right in
  List.fold_left add_right_tree all left;;

let rec cbal_tree n =
  if n = 0 then [Empty]
  else if n mod 2 = 1 then
    let t = cbal_tree(n/2) in
    add_trees_with t t []
  else
    let t1 = cbal_tree(n/2-1) and t2 = cbal_tree(n/2) in
    add_trees_with t1 t2 (add_trees_with t2 t1 []);;

  (* Symmetric binary trees *)

let rec is_mirror t1 t2 =
  match t1, t2 with
  | Empty, Empty -> true
  | Node (_, l1, r1), Node (_, l2, r2) -> is_mirror l1 l2 && is_mirror r1 r2
  | _ -> false;;

let is_symmetric t = match t with
  | Empty -> true
  | Node (_, t1, t2) -> is_mirror t1 t2;;

  (* Binary search trees (dictionaries) *)

let rec bt_insert t x = match t with
  | Empty -> Node (x, Empty, Empty)
  | Node (y, l, r) -> if (x=y) then t
                      else if (x<y) then Node (y, bt_insert l x, r)
                      else Node (y, l, bt_insert r x);;
let construct l = List.fold_left bt_insert Empty l;;

  (* Generate-and-test paradigm *)

let sym_cbal_trees n = List.filter is_symmetric (cbal_tree n)  ;;

let sym_cbal_trees_length n = List.length (sym_cbal_trees n)  ;;

  (* when n is even, length is 0 *)

  (* Construct height-balanced binary trees *)

let rec hbal_tree n =
  if n = 0 then [Empty]
  else if n = 1 then [Node ('x', Empty, Empty)]
  else let t1 = hbal_tree (n - 1) and t2 = hbal_tree (n-2) in
       add_trees_with t1 t1 (add_trees_with t1 t2 (add_trees_with t2 t1 []));;

  (* Construct height-balanced binary trees with a given number of nodes *)

let rec min_nodes h =
  if h=0 then 0
  else if h=1 then 1
  else min_nodes(h-1) + min_nodes(h-2) + 1;;

let rec max_height h =
  if h=0 then 0 else
    let cur = max_height (h-1) in
    if max_height (h-min_nodes(h-1)-1)=h then h+1 else h;;

(* TODO *)
let hbal_tree_nodes =  ();;

  (* Count the leaves of a binary tree *)

let rec count_leaves t = match t with
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> count_leaves l + count_leaves r;;

  (* Collect the leaves of a binary tree in a list *)

let rec leaves t = match t with
  | Empty -> []
  | Node (x, Empty, Empty) -> [x]
  | Node (x, l, r) ->  leaves l @ leaves r;;

  (* Collect the internal nodes of a binary tree in a list *)

let rec internals t = match t with
  | Empty | Node(_, Empty, Empty) -> []
  | Node(c, l, r) -> internals l @ (c::(internals r));;

  (* Collect the nodes at a given level in a list *)

let rec at_level t l = match t with
  | Empty -> []
  | Node(c, left, right) -> if l=1 then [c] else
                              at_level left (l-1) @ at_level right (l-1);;

  (* Construct a complete binary tree *)

let rec myflatten p c = 
   match (p, c) with
   | (p, []) -> List.map (fun x -> Node (x, Empty, Empty)) p
   | (x::t, [y]) -> Node (x, y, Empty)::myflatten t []
   | (ph::pt, x::y::t) -> (Node (ph, x, y))::(myflatten pt t)
   | _ -> invalid_arg "myflatten"
 
 let complete_binary_tree lst = match lst with
   | [] -> Empty
   | _ ->
      let rec aux l xs = match xs with
        | [] -> []
        | xs -> let p, c = split xs (1 lsl l) in
                 myflatten p (aux (l+1) c) in
      List.hd (aux 0 lst);;

 (* TODO *)
let is_complete_binary_tree = false;;   

  (* Layout a binary tree (1) *)

  

  (* Layout a binary tree (2) *)

  (* Layout a binary tree (3) *)

  (* A string representation of binary trees *)

  (* Preorder and inorder sequences of binary trees *)

  (* Dotstring representation of binary trees *)
