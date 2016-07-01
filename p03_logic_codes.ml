type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

(* Truth tables for logical expressions (2 variables) *)

let rec eval2 a val_a b val_b expr = match expr with
  | Var x -> if x=a then val_a else if x=b then val_b else failwith "expression contains invalid variable"
  | Not e -> not (eval2 a val_a b val_b e)
  | And (e1, e2) -> (eval2 a val_a b val_b e1) && (eval2 a val_a b val_b e2)
  | Or (e1, e2) -> (eval2 a val_a b val_b e1) || (eval2 a val_a b val_b e2);;

let table2 a b expr =
  [(true, true, eval2 a true b true expr);
   (true, false, eval2 a true b false expr);
   (false, true, eval2 a false b true expr);
   (false, false, eval2 a false b false expr)
  ];;

  (* Truth tables for logical expressions *)

let rec evals val_vars expr = match expr with
  | Var x -> List.assoc x val_vars
  | Not e -> not (evals val_vars e)
  | And (e1, e2) -> (evals val_vars e1) && (evals val_vars e2)
  | Or (e1, e2) -> (evals val_vars e1) || (evals val_vars e2);;

let rec table_maker val_vars vars expr = match vars with
  | [] -> [(List.rev val_vars, evals val_vars expr)]
  | v::tl -> table_maker ((v, true)::val_vars) tl expr @ table_maker ((v, false)::val_vars) tl  expr;;

let table vars expr = table_maker [] vars expr;;  
  
  (* Gray code *)

let prepend_char c s = String.make 1 c ^ s;;
let rec gray n =
  if n<=1 then ["0"; "1"] else let g = gray(n-1) in
                               List.map (prepend_char '0') g @ List.rev_map (prepend_char '1') g;;

  (* Huffman code *)

module Pq = struct
    type 'a t = {data: 'a list array; mutable first: int};;
    let make() = {data=Array.make 101 []; first=101};;
    let add q p x = q.data.(p)<-x :: q.data.(p); q.first <- min p q.first;;
    let get_min q = if q.first=101 then None
                    else match q.data.(q.first) with
                         | [] -> (failwith "shouldn't be empty")
                         | x::tl ->
                            let p = q.first in
                            q.data.(q.first) <- tl;
                            while q.first < 101 && q.data.(q.first) = []
                            do
                              q.first <- q.first+1
                            done;
                            Some(p, x)
  end;;

type tree =
  | Leaf of string
  | Node of tree*tree;;

let rec huffman_tree q = match Pq.get_min q, Pq.get_min q with
  | Some(p1, t1), Some(p2, t2) -> Pq.add q (p1+p2) (Node(t1, t2)); huffman_tree q
  | Some(_, t), None | None, Some(_, t) -> t
  | None, None -> (failwith "shouldn't be both none");;

let rec prefixes_of_tree prefix mytree = match mytree with
  | Leaf s -> [(s, prefix)]
  | Node(t0, t1) -> (prefixes_of_tree(prefix ^ "0") t0) @ (prefixes_of_tree(prefix ^ "1") t1);;

let huffman fs = if List.fold_left (fun s (_,p) -> s+p) 0 fs <> 100 then failwith "huffman sum of weights should be 100";
                 let q = Pq.make() in
                 List.iter (fun (s,f) -> Pq.add q f (Leaf s)) fs;
                 prefixes_of_tree "" (huffman_tree q);;
