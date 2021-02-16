(* Section 1 : Lists *)

(* Question 1.1 : Most common element of *sorted* list *)

let mode_tests: (int list * int) list = [(1::2::3::3::4::[],3);
                                         (1::[],1);
                                         (3::2::2::1::1::[],1);
                                        ] ;;

let mode (l: 'a list) : 'a =
  let sorted_l = List.sort compare l in
  let rec aux l ((cur_el, cur_num) : 'a * int) ((max_el, max_num) : 'a * int) = 
    match l with
    | [] -> if (cur_num > max_num) then cur_el else max_el
    | x::t -> if x = cur_el then aux t (cur_el, cur_num+1) (max_el, max_num)
        else if cur_num > max_num then aux t (x,1) (cur_el, cur_num)
        else aux t (x,1) (max_el, max_num) in
  let x::t = sorted_l in 
  aux t (x,1) (x,1)
;;

(* Question 1.2 : Most common consecutive pairing *)

let pair_mode_tests: (int list * (int * int) ) list = [(1::2::[],(1,2));
                                                       (1::2::3::[],(1,2));
                                                       (1::2::3::4::1::2::[],(1,2));
                                                      ] ;;

let pair_mode (l: 'a list) : 'a * 'a = 
  let rec pair l acc =
    match l with
    | [] -> acc
    | _::[] -> acc
    | a::(b::_ as t) -> pair t ((a,b)::acc) in
  mode (pair l [])
;;


(* Section 2 : Custom data types *)

let convert_time ((from_unit, val_) : time_unit value) to_unit : time_unit value =
  match from_unit, to_unit with
  |Second, Second -> Second, val_
  |Hour, Hour -> Hour, val_
  |Second, Hour -> Hour, (val_ /. 3600.)
  |Hour, Second -> Second, (val_ *. 3600.)
;;

let convert_dist ((from_unit, val_) : dist_unit value) to_unit : dist_unit value =
  match from_unit, to_unit with
  |Foot, Foot -> Foot, val_
  |Meter, Meter -> Meter, val_
  |Mile, Mile -> Mile, val_
  |Foot, Meter -> Meter, (val_ *. 0.3048)
  |Meter, Foot -> Meter, (val_ /. 0.3048)
  |Foot, Mile -> Mile, (val_ /. 5280.)
  |Mile, Foot -> Foot, (val_ *. 5280.)
  |Meter, Mile -> Mile, (val_ /. 1609.344)
  |Mile, Meter -> Meter, (val_ *. 1609.344)
;;

let convert_speed ((from_unit, val_) : speed_unit value) to_unit : speed_unit value =
  let ((from_dist,from_time),(to_dist,to_time)) = (from_unit, to_unit) in
  let (dist, dist_val) = convert_dist (from_dist, val_) to_dist in
  let (time, time_val) = convert_time (from_time, 1.) to_time in
  (dist, time), (dist_val /. time_val)
;;

let add_speed (a : speed_unit value) ((b_unit, b_val) : speed_unit value) : speed_unit value = 
  let (a_unit,a_val) = a  in
  let (_ ,conv_val) = convert_speed (a_unit, a_val) b_unit in
  b_unit, ( conv_val +. b_val) 
;;

let dist_traveled time ((speed_unit, speed_val) : speed_unit value) : dist_unit value = 
  let (dist_unit, to_time) = speed_unit in
  let (from_time, from_time_val) = time in
  let (_ , time_val) = convert_time (from_time, from_time_val) to_time in
  dist_unit, (speed_val *. time_val)
;;

(* Section 3 : recursive data types/induction *)

let passes_da_vinci_tests : (tree * bool) list = [
  Leaf,true;
  Branch (2., []),true;
  Branch (2., [Branch(4., [])]), false;
  Branch (4., [Branch(2., [])]), true;
  Branch (5., [Branch(3., []); Branch(4., [])]), true;
  Branch (2., [Leaf; Leaf]), true;
  Branch (2., [Branch (3., [Leaf; Leaf; Leaf]);Leaf;Branch (4., [])]),false;
  Branch (2.3, [Branch(4.4, [Branch(3.2,[])])]), false;
  Branch (5., [Branch(4., [Branch(3.,[])])]), true;
] ;;
let sum_of_squares l =
  let rec sum l acc =
    match l with
    | [] -> acc
    | x::t -> sum t (acc +. x**2.) in
  sum l 0.
;;


let traverse_children l = 
  let rec traverse l l_width =
    match l with
    | [] -> sum_of_squares l_width
    | x::t -> match x with
      | Leaf -> traverse t (l_width @ [0.])
      | Branch (width, _) -> traverse t (l_width @ [width]) in
  traverse l []
;; 
  
let rec passes_da_vinci t =
  let rec helper_func t2 v =
    let rec helper l v =
      match l with
      | [] -> v
      | x::xs -> let y = helper_func x v in
          helper xs y in
    match t2 with
    | Leaf -> v
    | Branch(value ,children) -> let sqr_val = value**2. >= traverse_children children in
        match v with
        | true -> helper children sqr_val
        | false -> helper children false
  in
  match t with
  | Leaf -> true
  |Branch (_,_) -> helper_func t true
;;
