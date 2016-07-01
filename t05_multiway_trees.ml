open P05_multiway_trees;;

let t_count_nodes () =
  let count_nodes_tt = Alcotest.(check int) in
  count_nodes_tt "" 2 (count_nodes (T('a', [T('f',[]) ])));;


let ts5 = [
    "count_nodes", t_count_nodes;
  ];;
