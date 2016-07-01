type 'a mult_tree = T of 'a * 'a mult_tree list;;

  (* Count the nodes of a multiway tree *)

let rec count_nodes (T(_, sub)) =
  List.fold_left (fun n t -> n + count_nodes t) 1 sub;;

  (* Tree construction from a node string *)

  (* Determine the internal path length of a tree *)

  (* Construct the bottom-up order sequence of the tree nodes *)

  (* Lisp-like tree representation *)
