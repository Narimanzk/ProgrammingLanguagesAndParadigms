(* Question 1 *)


let rec find_map (f : 'a -> 'b option) (l : 'a list) : 'b option =
  match l with
  | [] -> None
  | h::t ->
      let r = f h in
      match r with
      | Some h -> Some h
      | None -> find_map f t
;;


let partition (p : 'a -> bool) (l : 'a list) : ('a list * 'a list) = 
  let list_one e l =
    if p e then e::l
    else l
  in
  let list_two e l =
    if p e then l
    else e::l
  in
  ( List.fold_right list_one l [] ,
    List.fold_right list_two l [] ) 
;;

(* Question 2 *)

let make_manager (masterpass : masterpass) : pass_manager = 
  let ref_list : (address * password) list ref = ref [] in
  let ref_master = ref masterpass in
  let counter = ref 0 in
  let failed = ref 0 in
  let verify input = 
    if input = !ref_master then counter := !counter + 1
    else raise WrongPassword; failed := !failed + 1
  in 
  let save m a p = 
    if !failed < 3 then (verify m; ref_list := (a , encrypt m p)::!ref_list)
    else  raise AccountLocked
  in
  let get_force m a = find_map
      (fun x -> let (y,z) = x in
        if a = y
        then Some(decrypt m z)
        else None) !ref_list  in
  let get m a = 
    if !failed < 3 then (verify m; get_force m a)
    else raise AccountLocked 
  in
  let update_master m1 m2 = verify m1;
    let rec update_pass l acc =
      match l with
      |[] -> acc
      |(add, pass)::t -> update_pass t (acc@[(add,encrypt m2 (decrypt m1 pass))])
    in
    ref_list := update_pass !ref_list [];
    ref_master := m2
  in
  let count_ops m = 
    if !failed < 3 then (verify m; !counter)
    else raise AccountLocked 
  in
  { save; get_force; get; update_master; count_ops}
    
;;

(* Question 3 *)

(* Counting values at same time *)
let catalan_count (n : int) : (int * int) = 
  let count_rec_calls = ref 0 in
  let rec catalan n =
    count_rec_calls := !count_rec_calls + 1;
    if n = 0 then 1
    else
      let rec aux i n acc =
        if i > n then acc
        else aux (i + 1) n (acc + catalan i * catalan (n-i))
      in 
      aux 0 (n-1) 0 in
  if n = 0 then 1 , 1
  else let cat_num = catalan n in (cat_num , !count_rec_calls)
;;

(* Memoization function *)
let memoize (f : ('a -> 'b) -> 'a -> 'b) (stats : stats) : 'a -> 'b =
  let hash = Hashtbl.create 1000 in
  let rec f' x = 
    match Hashtbl.find_opt hash x with
    |Some v -> stats.lkp := !(stats.lkp) + 1; v;
    |None -> 
        Hashtbl.add hash x (f f' x);
        stats.entries := !(stats.entries) + 1;
        let Some s = Hashtbl.find_opt hash x in 
        s
  in
  f'
;;

(* Version of catalan that can be memoized *)
let memo_cat (recf : int -> int) (n : int) : int = 
  if n = 0 then 1 
  else
    let rec aux i n acc =
      if i > n then acc
      else aux (i + 1) n (acc + recf i * recf (n-i))
    in
    aux 0 (n-1) 0
;;

let catalan_m (n : int) : int * stats = 
  let stat = { entries = ref 0; lkp = ref 0} in
  let cat = memoize memo_cat stat n in (cat, stat)
;;
