  
(* 01 Write a function last : 'a list -> 'a option that returns the last element of a list *)
let rec last xs = match xs with
    | [] -> None
    | [x] -> Some x
    | _ :: t -> last t;;

let lst_hd xs = match xs with
  | [] -> failwith "hd"
  | h::t -> h;;

(* 02 Find the last but one (last and penultimate) elements of a list *)
let rec last_two xs = match xs with
  | [] -> None
  | [a] -> None
  | [a; b] -> Some(a, b)
  | hd::tail -> last_two tail;;

(* 03 Find the k'th element of a list *)
let rec at n xs = match xs with
  | [] -> None
  | hd::tail -> if n=1 then Some(hd) else at (n-1) tail;;

(* 04 Find the number of elements of a list *)
let length xs =
  let rec inner acc lst = match lst with
    | [] -> acc
    | _::tl -> inner (acc+1) tl
  in inner 0 xs ;;

(* 05 Reverse a list *)
let rev xs =
  let rec inner xs acc =
    match xs with
    | [] -> acc
    | head::tail -> inner tail (head::acc)
  in inner xs [] ;;

let lst_map f lst =
  let rec aux f lst acc = match lst with
    | [] -> acc
    | h::t -> aux f t ((f h)::acc) in
  aux f (rev lst) [];;

let lst_filter f lst =
  let rec aux f lst acc = match lst with
    | [] -> acc
    | h::t -> if (f h) then aux f t (h::acc) else aux f t acc
  in
  aux f (rev lst) [];;

let rec lst_forall f lst = match lst with
  | [] -> true
  | h::t -> if (f h) then lst_forall f t else false;;

(* 06 Find out whether a list is a palindrome *)
let rec is_palindrome xs = xs = (rev xs);;
  
(* 07 Flatten a nested list structure *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten xs =
  let rec inner xs acc =
    match xs with
       | [] -> acc
       | One x::t -> inner t (x::acc)
       | Many f::r -> inner r (inner f acc) in
  rev (inner xs []);;

(* 08 Eliminate consecutive duplicates of list elements *)
(* tailrec??? *)
let rec compress xs = match xs with
  | a::(b::_ as t) -> if a=b then (compress t) else a::(compress t)
  | _ -> xs;;

(* 09 Pack consecutive duplicates of list elements into sublists *)
let pack list =
  let rec aux cur acc l = match l with
    | [] -> []
    | [x] -> (x::cur)::acc
    | a::(b::_ as t) -> if a=b then (aux (a::cur) acc t) else (aux [] ((a::cur)::acc) t)
  in rev (aux [] [] list);;

(* 10 Run-length encoding of a list *)
let encode list =
  let rec aux count acc l = match l with
    | [] -> []
    | [x] -> (count+1, x)::acc
    | a::(b::_ as t) -> if a=b then aux (count+1) acc t
                        else aux 0 ((count+1, a)::acc) t
  in rev (aux 0 [] list);;

let encode' list = lst_map (fun l -> (length l, lst_hd l)) (pack list);;

(* 11 Modified run-length encoding *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let create_tuple n elem = match n with
  | 1 -> One elem
  | _ -> Many (n, elem);;

let encode_rle' ll = lst_map (fun l -> create_tuple (length l) (lst_hd l)) (pack ll);;
  
(* 12 Decode a run-length encoded list *)

let decode xs =
  let rec many acc n x =  match n with
      | 0 -> acc
      | _ -> many (x::acc) (n-1) x
  in
  let rec aux acc xs = match xs with
      | [] -> acc
      | (One x)::t -> aux (x::acc) t
      | (Many (c, e))::t -> aux (many acc c e) t
  in aux [] (rev xs);;

(* 13 Run-length encoding of a list (direct solution) *)

let encode_rle ll =
  let rec aux xs count acc = match xs with
    | [] -> []
    | [x] -> (create_tuple (count+1) x)::acc
    | a::(b::_ as t) -> if a=b then aux t (count+1) acc
                        else aux t 0 ((create_tuple (count+1) a)::acc)
  in rev (aux ll 0 []);;

(* 14 Duplicate the elements of a list *)

let rec duplicate xs = match xs with
  | [] -> []
  | h::t -> h::h::(duplicate t) ;;

(* 15 Replicate the elements of a list a given number of times *)

let rec replicate xs n =
    let rec prepend n acc x = match n with
      | 0 -> acc
      | _ -> prepend (n-1) (x::acc) x
    in
    let rec aux acc xs = match xs with
      | [] -> acc
      | h::t -> aux (prepend n acc h) t
    in aux [] (rev xs);;
    
(* 16 Drop every N'th element from a list *)

let drop xs n =
  let rec aux i xs = match xs with
    | [] -> []
    | h::t -> match i with
              | j when j=n  -> aux 1 t
              | _  -> h:: (aux (i+1) t)
  in aux 1 xs;;

(* 17 Split a list into two parts; the length of the first part is given *)

let split xs n =
    let rec aux l r i = match r with
        | [] -> (rev l, r)
        | h::t -> match i with
                    | 0 -> rev l, r
                    | _ -> aux (h::l) t (i-1)
    in aux [] xs n;;
                               
(* 18 Extract a slice from a list *)

let first_n xs n =
  let rec aux xs n acc = match xs with
    | [] -> rev acc
    | h::t -> if n=0 then (rev acc) else aux t (n-1) (h::acc)
  in
  aux xs n [];;  

let rec slice xs l r = match xs with
  | [] -> []
  | h::t -> if l=0 then (first_n xs (r+1)) else (slice t (l-1) (r-1));;

(* 19 Rotate a list N places to the left *)

let rec concat xs ys = match xs with
  | [] -> ys
  | h::t -> h::(concat t ys);;

let rotate xs n =
  let index = if n<0 then (length xs) + n else n in
  let rec aux lhs rhs i = match lhs with
    | [] -> rhs
    | h::t -> if i=0 then (concat lhs (rev rhs)) else aux t (h::rhs) (i-1)
  in
  aux xs [] index;;
  
  (* 20 Remove the K'th element from a list *)

let remove_at n xs =
  let rec aux lhs rhs i = match rhs with
    | [] -> lhs
    | h::t -> if i=0 then (concat (rev lhs) t) else (aux (h::lhs) t (i-1))
  in
  aux [] xs n;;
(* but since concat is not tailrec *)
let rec remove_at' n xs = match xs with
  | [] -> xs
  | h::t -> if n=0 then t else h::(remove_at' (n-1) t);;

  (* 21 Insert an element at a given position into a list *)

let rec insert_at v n xs = if n=0 then v::xs else match xs with
                                            | [] -> [v]
                                            | h::t -> h::(insert_at v (n-1) t);;
  
  (* 22 Create a list containing all integers within a given range *)

let range l r =
  let rec aux cur acc = if cur>r then (rev acc) else aux (cur+1) (cur::acc)
  in
  aux l [];;

  (* 23 Extract a given number of randomly selected elements from a list *)

let extract_rand list n =
  let rec aux acc list i = match list with
    | [] -> raise Not_found
    | h::t -> if i=0 then (h, (concat acc t)) else aux (h::acc) t (i-1)
  in
  aux [] list (Random.int n);;

let rand_select list n =
  let rec aux n acc list len =
    if n=0 then acc else
      let picked, rest = extract_rand list len in aux (n-1) (picked::acc) rest (len-1)
  in
  let len = length list in
  aux (min len n) [] list len;;

  (* 24 Lotto: Draw N different random numbers from the set 1..M *)

let lotto_select n m = rand_select (range 1 m) n;;

  (* 25 Generate a random permutation of the elements of a list *)

let permutation lst = rand_select lst (length lst);;

  (* 26 Generate the combinations of K distinct objects chosen from the N elements of a list *)

let extract n lst =
  let rec aux k acc emit lst = match lst with
    | [] -> acc
    | h::t -> if k=1 then aux k (emit [h] acc) emit t else
                let new_emit x = emit (h::x) in
                aux k (aux (k-1) acc new_emit t) emit t
  in
  let emit x acc = x::acc in
  aux n [] emit lst;;

  (* 27 Group the elements of a set into disjoint subsets *)

let group list sizes =
    let initial = lst_map (fun size -> size, []) sizes in
    let prepend p list =
      let emit l acc = l :: acc in
      let rec aux emit acc = function
        | [] -> emit [] acc
        | (n,l) as h :: t ->
           let acc = if n > 0 then emit ((n-1, p::l) :: t) acc
                     else acc in
           aux (fun l acc -> emit (h :: l) acc) acc t
      in
      aux emit [] list
    in
    let rec aux = function
      | [] -> [ initial ]
      | h :: t -> List.concat (lst_map (prepend h) (aux t))
    in
    let all = aux list in
    let complete = lst_filter (lst_forall (fun (x,_) -> x = 0)) all in
    lst_map (lst_map snd) complete;;  

  (* 28 Sorting a list of lists according to length of sublists *)

let rec insert cmp e lst = match lst with
  | [] -> [e]
  | h::t -> if (cmp e h <= 0) then e::lst else h::(insert cmp e t);;

let rec sort_with cmp lst = match lst with
  | [] -> []
  | h::t -> insert cmp h (sort_with cmp t);;

let length_sort lists =
  let length_lst_lst = lst_map (fun list -> length list, list) lists in
  let sorted_length_lst_lst = sort_with (fun a b -> compare (fst a) (fst b)) length_lst_lst in
  lst_map snd sorted_length_lst_lst;;
    
let rle_encode' list =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (x, count + 1) :: acc
    | a :: (b :: _ as t) ->
       if a = b then aux (count + 1) acc t
       else aux 0 ((a, count + 1) :: acc) t in
  aux 0 [] list

let frequency_sort lists =
  let lengths = lst_map length lists in
  let freq = rle_encode' (sort_with compare lengths) in
  let by_freq =
    lst_map (fun list -> List.assoc (length list) freq , list) lists in
  let sorted = sort_with (fun a b -> compare (fst a) (fst b)) by_freq in
  lst_map snd sorted;;  
