open T01_list;;
open T02_arithmetic;;
open T03_logic_codes;;
open T04_binary_trees;;
open T05_multiway_trees;;
open T06_graphs;;
open T07_misc;;  

let tests ts =
  let tc_gen test_case = ((fst test_case), `Slow, (snd test_case)) in
  List.map tc_gen ts;;

let () =
  Alcotest.run "test suite" [
                 (* "t1", tests ts1; *)
                 (* "t2", tests ts2; *)
                 (* "t3", tests ts3; *)
                 "t4", tests ts4;
                 "t5", tests ts5;
                 "t6", tests ts6;
                 "t7", tests ts7;
               ];;

  
