open P02_arithmetic;;

let t_is_prime () =
  let is_prime_tt = Alcotest.(check bool) in
  is_prime_tt "1" true (not (is_prime 1));
  is_prime_tt "2" true (is_prime 2);
  is_prime_tt "7" true (is_prime 7);;

let t_gcd () =
  let gcd_tt = Alcotest.(check int) in
  gcd_tt "" 1 (gcd 13 27);
  gcd_tt "" 2 (gcd 20536 7826);;

let t_coprime () =
  let coprime_tt = Alcotest.(check bool) in
  coprime_tt "" true (coprime 13 27);
  coprime_tt "" true (not (coprime 20536 7826));;

let t_phi () =
  let phi_tt = Alcotest.(check int) in
  phi_tt "1" 1 (phi 1);  
  phi_tt "prime 13" 12 (phi 13);  
  phi_tt "nonprime 10" 4 (phi 10);;

let t_factors () =
  let factors_tt = Alcotest.(check (list int)) in
  factors_tt "1" [] (factors 1);
  factors_tt "315" [3;3;5;7] (factors 315);;

let t_factors_mult () =
  let factors_mult_tt = Alcotest.(check (list (pair int int))) in
  factors_mult_tt "1" [] (factors_mult 1);
  factors_mult_tt "315" [(3,2); (5,1); (7,1)] (factors_mult 315);
  factors_mult_tt "125" [(5,3)] (factors_mult 125);;

let t_phi_improved () =
  let phi_improved_tt = Alcotest.(check int) in
  phi_improved_tt "1" 1 (phi_improved 1);
  phi_improved_tt "10" 4 (phi_improved 10);
  phi_improved_tt "13" 12 (phi_improved 13);;

let t_compare_phi () =
  let compare_phi_tt = Alcotest.(check bool) in
  compare_phi_tt "10090" true (let compare_res = compare_phi 10090 in
                              let f = fst compare_res and s = snd compare_res in
                              fst f = fst s && snd f = snd s);;

let t_all_primes () =
  let all_primes_tt = Alcotest.(check int) in
  let open P01_list in
  all_primes_tt "all_primes" 1000 (length (all_primes 2 7920));;

let t_goldbach () =
  let goldbach_tt = Alcotest.(check (pair int int)) in
  goldbach_tt "" (5, 23) (goldbach 28);;

let t_goldbach_composition () =
  let goldbach_composition_tt = Alcotest.(check (list (pair int (pair int int)))) in
  goldbach_composition_tt "goldbach_list" [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
 (20, (3, 17))] (goldbach_list 9 20);
  goldbach_composition_tt "goldbach_limit" [(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
 (1928, (61, 1867))] (goldbach_limit 1 2000 50);;
  
(* ------------------------------------------------------------------------------------ *)
  
let ts2 = [
    "is_prime", t_is_prime;
    "gcd", t_gcd;
    "coprime", t_coprime;
    "phi", t_phi;
    "factors", t_factors;
    "factors_mult", t_factors_mult;
    "phi_improved", t_phi_improved;
    "compare_phi", t_compare_phi;
    "all_primes", t_all_primes;
    "goldbach", t_goldbach;
    "goldbach_composition", t_goldbach_composition;
  ];;
