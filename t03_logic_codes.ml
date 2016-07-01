open P03_logic_codes;;
open Alcotest;;  

let alcotest_tuple3 (type a) (type b) (type c) (a:a testable) (b:b testable) (c:c testable): (a*b*c) testable =
  let module A = (val a) in
  let module B = (val b) in
  let module C = (val c) in
  (module struct
     type t = a * b * c
     let equal (a1, b1, c1) (a2, b2, c2) = A.equal a1 a2 && B.equal b1 b2 && C.equal c1 c2;;
     let pp ppf(a,b,c) = A.pp ppf a; Format.pp_print_cut ppf (); B.pp ppf b; C.pp ppf c;;                                                                                
end);;

let t_table2 () =
  let table2_tt = Alcotest.(check (list (alcotest_tuple3 bool bool bool))) in
  table2_tt "" [(true, true, true); (true, false, true); (false, true, false);
                (false, false, false)] (table2 "a" "b" (And(Var "a", Or(Var "a", Var "b"))));;

(* TODO test table *)

let t_gray ()  =
  let gray_tt = Alcotest.(check (list string)) in
  gray_tt "" ["0"; "1"] (gray 1);
  gray_tt "" ["00"; "01"; "11"; "10"] (gray 2);
  gray_tt "" ["000"; "001"; "011"; "010"; "110"; "111";  "101"; "100"] (gray 3);;

let t_huffman () =
  let huffman_tt =  Alcotest.(check (list (pair string string))) in
  huffman_tt "" [("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101"); ("d", "111")] (huffman [ ("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5) ]);
  huffman_tt "" [("d", "00"); ("a", "010"); ("b", "011"); ("e", "10"); ("c", "11")] (huffman ["a", 10;  "b", 15;  "c", 30;  "d", 16;  "e", 29]);;

let ts3 = [
    "table2", t_table2;
    "gray", t_gray;
    "huffman", t_huffman;
  ];;
