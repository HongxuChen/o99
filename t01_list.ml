open P01_list;;

let t_last () = Alcotest.(check (option string)) "last" (Some "d") (last ["a";"b";"c";"d"]);;

let t_last_two () =
  let last_two_tt = Alcotest.(check (option (pair string string))) in
  last_two_tt "empty list" None (last_two []);
  last_two_tt "1 element" None (last_two []);
  last_two_tt "2 elements" (Some ("a", "b")) (last_two ["a"; "b"]);
  last_two_tt "more elements" (Some ("b", "c")) (last_two ["a"; "b"; "c"]);;

let t_at () =
  let at_tt = Alcotest.(check (option string)) in
  at_tt "empty" None (at 3 ["a"]);
  at_tt "exceed one"  None (at 3 ["a"; "b"]);
  at_tt "last" (Some "c") (at 3 ["a"; "b"; "c"]);
  at_tt "inside" (Some"c") (at 3 ["a"; "b"; "c"; "d"]);;

let t_length () =
  let length_tt = Alcotest.(check int) in
  length_tt "empty" 0 (length []);
  length_tt "1 element" 1 (length ["1"]);
  length_tt "more elements" 3 (length ["1"; "2"; "3"])

let t_rev () =
  let rev_tt = Alcotest.(check (list int)) in
  rev_tt "empty" [] (rev []);
  rev_tt "normal" [3;2;1] (rev [1;2;3])

let t_is_palindrome () =
  let is_palindrome_tt = Alcotest.(check bool) in
  is_palindrome_tt "empty" true (is_palindrome []);
  is_palindrome_tt "yes odd" true (is_palindrome [1;2;3;2;1]);
  is_palindrome_tt "yes even" true (is_palindrome [1;2;2;1]);
  is_palindrome_tt "no" false (is_palindrome [1;2;3;1])

let t_flatten () =
  let flatten_tt = Alcotest.(check (list string)) in
  flatten_tt ""  ["a"; "b"; "c"; "d"; "e"] (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ])

let t_compress () =
  let compress_tt = Alcotest.(check (list string)) in
  compress_tt ""  ["a"; "b"; "c"; "a"; "d"; "e"]
              (let xs = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] in compress xs)

let t_pack () =
  let pack_tt = Alcotest.(check (list (list string))) in
  pack_tt ""
          [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
          (let l = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] in (pack l))

let t_encode () =
  let encode_tt = Alcotest.(check (list (pair int string))) in
  encode_tt "" [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
            (let l = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] in (encode l))

(* TODO fix insufficient info for pp *)
let rle_testable (type a) elt =
  let (module Elt: Alcotest.TESTABLE with type t = a) = elt in
  let module M = struct
      type t = a rle
      let equal x y = match x, y with
        | One oa, One ob -> Elt.equal oa ob
        | Many (oac, oae), Many (obc, obe) -> if oac=obc then Elt.equal oae obe else false
        | _, _ -> false
      let pp fmt rle_v = match rle_v with
        | One a -> Format.pp_print_string fmt "One; "
        | Many (count, elem) -> Format.pp_print_string fmt "Many; "
    end
  in (module M: Alcotest.TESTABLE with type t = M.t);;

let t_encode_rle' () =
  let encode_tt = Alcotest.(check (list (rle_testable string))) in
  let expected = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
  in encode_tt "" expected (encode_rle' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]);;

let t_decode () =
  let decode_tt = Alcotest.(check (list string)) in
  let expected = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] in
  decode_tt "" expected (decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]);;

let t_encode_rle () =
  let encode_tt = Alcotest.(check (list (rle_testable string))) in
  let expected = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
  in encode_tt "" expected (encode_rle ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]);;

let t_duplicate () =
  let duplicate_tt = Alcotest.(check (list int)) in
  duplicate_tt "empty" [] (duplicate []);
  duplicate_tt "normal" [1;1;2;2;] (duplicate [1;2])

let t_replicate () =
  let replicate_tt = Alcotest.(check (list int)) in
  replicate_tt "empty" [] (replicate [] 3);
  replicate_tt "zero" [] (replicate [1;2] 0);
  replicate_tt "normal" [1;1;1;2;2;2] (replicate [1;2] 3)

let t_drop () =
  let drop_tt = Alcotest.(check (list int)) in
  drop_tt "empty" [] (drop [] 2);
  drop_tt "exceed" [1;2] (drop [1;2] 3);
  drop_tt "exact" [1;2] (drop [1;2;3] 3);
  drop_tt "more" [1;2;4;5;7] (drop [1;2;3;4;5;6;7] 3);;

let t_split () =
  let split_tt = Alcotest.(check (pair (list string) (list string))) in
  split_tt "normal" (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]) (split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3);
  split_tt "length longer" (["a"; "b"; "c"; "d"], []) (split ["a";"b";"c";"d"] 5);;

let t_slice () =
  let slice_tt = Alcotest.(check (list string)) in
  slice_tt "first_n normal" ["c"; "d"; "e"] (first_n ["c"; "d"; "e"; "f"; "g"] 3);
  slice_tt "first_n length" ["c"; "d"; "e"] (first_n ["c"; "d"; "e"] 5);
  slice_tt "slice normal" ["c"; "d"; "e"; "f"; "g"] (slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6);;

let t_rotate () =
  let rotate_tt = Alcotest.(check (list string)) in
  rotate_tt "concat" ["a"; "b"; "c"] (concat ["a"; "b";"c"] []);
  rotate_tt "concat" ["a"; "b"; "c"] (concat ["a"] ["b";"c"]);
  rotate_tt "concat" ["a"; "b"; "c"] (concat ["a"; "b"] ["c"]);
  rotate_tt "concat" ["a"; "b"; "c"] (concat [] ["a"; "b"; "c"]);
  rotate_tt "rotate normal" ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3);
  rotate_tt "rotate negative" ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2));;

let t_remove_at () =
  let remove_at_tt = Alcotest.(check (list string)) in
  remove_at_tt "" ["a"; "c"; "d"] (remove_at 1 ["a";"b";"c";"d"]);
  remove_at_tt "" ["a"; "c"; "d"] (remove_at' 1 ["a";"b";"c";"d"]);;

let t_insert_at () =
  let insert_at_tt = Alcotest.(check (list string)) in
  insert_at_tt "" ["a"; "alfa"; "b"; "c"; "d"] (insert_at "alfa" 1 ["a";"b";"c";"d"]);
  insert_at_tt "" ["a"; "b"; "c"; "alfa"; "d"] (insert_at "alfa" 3 ["a";"b";"c";"d"]);
  insert_at_tt "" ["a"; "b"; "c"; "d"; "alfa"] (insert_at "alfa" 4 ["a";"b";"c";"d"]);;

let t_range () =
  let range_tt = Alcotest.(check (list int)) in
  range_tt "" [4; 5; 6; 7; 8; 9] (range 4 9);
  range_tt "" [] (range 9 4);;

let t_rand_select () =
  let rand_select_tt = Alcotest.(check (list string)) in
  rand_select_tt "" ["g"; "d"; "a"] (rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3);;

let t_lotto_select ()  =
  let lotto_select_tt = Alcotest.(check (list int)) in
  lotto_select_tt "" [10; 20; 44; 22; 41; 2] (lotto_select 6 49);;

let t_permutation ()   =
  let permutation_tt = Alcotest.(check (list string)) in
  permutation_tt "" ["a"; "e"; "f"; "b"; "d"; "c"] (permutation ["a"; "b"; "c"; "d"; "e"; "f"]);;

let t_extract ()  =
  let extract_tt = Alcotest.(check (list (list string))) in
  extract_tt "" [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] (extract 2 ["a";"b";"c";"d"]);;

let t_group ()   =
  let group_tt = Alcotest.(check (list (list (list string)))) in
  group_tt "" [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]] (group ["a";"b";"c";"d"] [2;1]);;

let t_sort_lst_lst ()  =
  let sort_lst_lst_tt = Alcotest.(check (list (list string))) in
  sort_lst_lst_tt "length sort" [["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
 ["i"; "j"; "k"; "l"]] (length_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                                      ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ]);
  sort_lst_lst_tt "frequency sort" [["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
 ["d"; "e"]; ["m"; "n"]] (frequency_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                   ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ]);;
  
 
(* ------------------------------------------------------------------------------------ *)
  
let ts1 = [
    "last", t_last;
    "last_two", t_last_two;
    "at", t_at;
    "rev", t_rev;
    "length", t_length;
    "is_palindrome", t_is_palindrome;
    "flatten", t_flatten;
    "compress", t_compress;
    "pack", t_pack;
    "encode", t_encode;
    "encode_rle'", t_encode_rle';
    "decode", t_decode;
    "encode_rle", t_encode_rle;
    "duplicate", t_duplicate;
    "replicate", t_replicate;
    "drop", t_drop;
    "split", t_split;
    "slice", t_slice;
    "rotate", t_rotate;
    "remove_at", t_remove_at;
    "insert_at", t_insert_at;
    "range", t_range;
    "rand_select", t_rand_select;
    "lotto_select", t_lotto_select;
    "permutation", t_permutation;
    "extract", t_extract;
    "group", t_group;
    "sort_lst_lst", t_sort_lst_lst;
  ];;
