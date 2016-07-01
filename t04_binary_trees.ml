open P04_binary_trees;;

(* TODO correct it *)

let bt_testable (type a) elt =
  let (module Elt:Alcotest.TESTABLE with type t=a) = elt in
  let module M = struct
      type t = a binary_tree
      let equal x y = match x, y with
        | Empty, Empty -> true
        | Node (label1, l1, r1), Node (label2, l2, r2) -> true
        | _ -> false;;
      let pp fmt t = match t with
        | Empty -> Format.pp_print_string fmt "Empty"
        | Node (label, l, r) -> Format.fprintf fmt "Many";;
    end
  in
  (module M: Alcotest.TESTABLE with type t=M.t);;

let example_tree = Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;  

(* let t_cbal_tree () = *)
(*   let cbal_tree_tt = Alcotest.(check (list bt_testable)) in *)
(*   cbal_tree_tt "" [Node ('x', Node ('x', Empty, Empty), *)
(*   Node ('x', Node ('x', Empty, Empty), Empty)); *)
(*  Node ('x', Node ('x', Empty, Empty), *)
(*   Node ('x', Empty, Node ('x', Empty, Empty))); *)
(*  Node ('x', Node ('x', Node ('x', Empty, Empty), Empty), *)
(*   Node ('x', Empty, Empty)); *)
(*  Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)), *)
(*   Node ('x', Empty, Empty))] (cbal_tree 2);; *)

let t_count_leaves () =
  let count_leaves_tt = Alcotest.(check int) in
  count_leaves_tt "" 3 (count_leaves example_tree);;

let t_leaves () =
  let leaves_tt = Alcotest.(check (list char)) in
  leaves_tt "empty" [] (leaves Empty);
  leaves_tt "single" ['x'] (leaves (Node ('x', Empty, Empty)));
  leaves_tt "multiple" ['d'; 'e'; 'g'] (leaves example_tree);;

let t_internals () =
  let internals_tt = Alcotest.(check (list char)) in
  internals_tt "empty result" [] (internals (Node ('a', Empty, Empty)));
  internals_tt "regular" ['b'; 'a'; 'c'; 'f'] (internals example_tree);;

let t_at_level () =
  let at_level_tt = Alcotest.(check (list char)) in
  at_level_tt "empty result" [] (at_level example_tree 5);
  at_level_tt "normal" ['b'; 'c'] (at_level example_tree 2);;

let ts4 = [
    (* "cbal_tree", t_cbal_tree; *)
    "count_leaves", t_count_leaves;
    "leaves", t_leaves;
    "at_level", t_at_level;
  ];;
