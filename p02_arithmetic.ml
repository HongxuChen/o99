open P01_list;;
  
(* 29 Determine whether a given integer number is prime *)
let is_prime n =
  let rec not_divisor d = d*d > n || n mod d <> 0 && (not_divisor (d+1)) in
  n <> 1 && not_divisor 2;;

(* 30 Determine the greatest common divisor of two positive integer numbers *)

let rec gcd m n = match m with
    | 0 -> n
    | _ -> gcd (n mod m) m;;  
  
(* 31 Determine whether two positive integer numbers are coprime *)

let rec coprime m n = (gcd m n) = 1;;  

(* 32 Calculate Euler's totient function φ(m) *)

let phi n =
  let rec count_coprime acc d =
    if d >= n then acc else count_coprime (if (coprime d n) then (acc+1) else acc) (d+1)
                                         in
                                         if n=1 then 1 else count_coprime 1 2;;

(* 33 Determine the prime factors of a given positive integer *)

let factors n =
  let rec aux d n =
    if n = 1 then [] else
      if n mod d = 0 then d:: (aux d (n/d)) else (aux (d+1) n) in
  aux 2 n;;  

(* 34 Determine the prime factors of a given positive integer (2) *)

let factors_mult n =
  let rec aux d n =
    if n = 1 then [] else
      if n mod d = 0 then
        match aux d (n/d) with
          | (h, n)::t when h=d -> (h, n+1)::t
          | l -> (d, 1)::l
      else aux (d+1) n
     in
     aux 2 n;;

(* 35 Calculate Euler's totient function φ(m) (improved) *)

let mypower a n =
  let rec aux acc a n = if n=0 then acc else aux (acc*a) a (n-1) in
  aux 1 a n;;

let phi_improved n =
  let rec aux lst acc = match lst with
      | [] -> acc
      | h::t -> let p = fst h and m = snd h in aux t (acc * (p-1) * (mypower p (m-1) ))
  in
  aux (factors_mult n) 1;;

(* 36 Compare the two methods of calculating Euler's totient function *)
let compare_phi n =
  let res f x = let t = Sys.time() in (f x, Sys.time()-.t) in
  (res phi n, res phi n);;
       
  
(* 37 A list of prime numbers *)

let all_primes lower upper =
  let rec aux cur acc_lst = if cur < upper then
    if is_prime cur then aux (cur+1) (cur::acc_lst) else aux (cur+1) acc_lst
      else acc_lst
    in aux lower [];;

(* 38 Goldbach's conjecture *)

let goldbach n =
  let rec aux cur =
    if is_prime cur && is_prime (n-cur) then (cur, n-cur) else aux (cur+2) in
  assert (n mod 2 = 0);
  aux 3;;

(* 39 A list of Goldbach compositions *)

let goldbach_list lower upper =
  let rec aux cur acc_list = if cur<=upper then
                               aux (cur+2) ((cur, goldbach(cur))::acc_list) else
                               acc_list in
  if lower mod 2 = 0 then rev (aux lower []) else rev (aux (lower+1) []);;

(* let goldbach_lb n lb = *)
(*   let rec aux cur = *)
(*     if is_prime cur && is_prime (n-cur) then (cur, n-cur) else aux (cur+2) in *)
(*   aux (if (lb mod 2 = 0) then (lb+1) else lb);; *)

(* let goldbach_limit lower upper limit = *)
(*   let rec goldbach_limit_rec cur acc_list = if cur<=upper then *)
(*                                goldbach_limit_rec (cur+2) ((cur, (goldbach_lb cur limit))::acc_list) else *)
(*                                acc_list in *)
(*   let lb = if (lower < limit) then if (limit mod 2 = 0) then limit else (limit+1) else if (lower mod 2 = 0) then lower else (lower+1) in *)
(*   List.rev (goldbach_limit_rec lb []);; *)

(* FIXME 994=53+941; 996=59+937 *)

let goldbach_limit lower upper limit =
  lst_filter (fun (_, (a, _)) -> a>limit) (goldbach_list lower upper);;
