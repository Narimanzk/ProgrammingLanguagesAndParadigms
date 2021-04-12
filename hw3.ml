(* Question 1 *)

(*open MazeGen
 exception NotFound
 let solve_maze (maze : MazeGen.maze) : MazeGen.dir list = 
   let next_cell s d =
     let x, y, _ = s in
     match d with
     | West -> ((x-1), y, West)
     | North -> (x, (y-1), North)
     | East -> ((x+1), y, East)
     | South -> (x, (y+1), South)
   in
   let rec solve_maze' state fc ldir =
     let _, _, sdir = state in 
     let right, front, left = dirs_to_check sdir in
     let fc1 = fun () -> if not(has_wall maze (next_cell state left)) then
         solve_maze' (next_cell state left) fc (left::ldir) in
     let fc2 = fun () -> if not(has_wall maze (next_cell state front)) then
         solve_maze' (next_cell state front) fc1 (front::ldir) in
     if not(has_wall maze (next_cell state right)) then
       solve_maze' (next_cell state right) fc1 (right::ldir)
     else if not(has_wall maze (next_cell state front)) then
       solve_maze' (next_cell state front) fc2 (front::ldir)
     else if not(has_wall maze (next_cell state left)) then
       solve_maze' (next_cell state left) fc (left::ldir)
     else
       fc()
   in
   let width, height = dims maze in
   let start_state = ((width-1), (height-1), North) in
   solve_maze' start_state (fun () -> raise NotFound) []
  
    
 ;;

(* You can run this code to manually test your solutions. *)
 let test_maze width height =
   let maze = MazeGen.random width height in
   let path = solve_maze maze in
   print_string (MazeGen.string_of_maze ~path maze)
 ;;
*)
(* Question 2 *) 
    
module Range : RangeSig = struct
  open Seq

  let rec zip (seq1 : 'a seq) (seq2 : 'b seq) : ('a * 'b) seq =
    let force f = f () in
    let node1 = force seq1 in
    let node2 = force seq2 in 
    match node1, node2 with 
    | Cons(hd1, tl1) , Cons(hd2, tl2) -> 
        (fun () -> Cons((hd1, hd2), zip tl1 tl2)) 
    | _  -> fun () -> Nil
        
        
  let rec keep_until (p : 'a -> bool) (seq : 'a seq) : 'a seq =
    let force f = f () in
    let node = force seq in
    match node with
    | Cons(hd, tl) ->
        if p hd then
          (fun () -> Nil) 
        else
          (fun () -> Cons(hd, keep_until p tl))
    | Nil -> fun () -> Nil

  let to_list (seq : 'a seq) : 'a list =
    let rec to_list' s l =
      let force f = f () in
      let node = force s in
      match node with
      | Nil -> l
      | Cons (hd, tl) -> hd :: to_list' tl l
    in
    to_list' seq []
  
  exception Invalid_argument of string
  
  let range (start : int) (step : int) (stop : int) : int seq = 
    if (start < stop && step <= 0) || (start > stop && step >= 0)
    then raise (Invalid_argument "The range is not feasible")
    else
      let st = map (fun x -> x * step) nats in
      let seq = map ((+) start) st in
      let valid num =
        if start < stop then
          num >= stop
        else num <= stop
      in
      keep_until valid seq
end ;;



(* This function is just another example of the use of optional arguments. 
Removing or altering it should not change your grade, as the function Range.range
    is being tested. Our grader cannot, at the moment, handle optional arguments.
*)
let range ?(start : int = 0) ?(step : int = 1) (stop : int) : int seq =
  Range.range start step stop
;;

(* Examples of uses of optional arguments, lazy-evaluated *)
let example_ranges () = [
  range 10;
  range ~start:5 10;
  range ~step:2 20;
  range ~start:100 ~step:(~-2) 50
] ;;

(* Question 3 *)

module RationalField : (AlgField with type t = rational) = struct
  type t = rational
    
  let zero = {num = 0; den = 1}
  let one = {num = 1; den = 1}
  
  let equal a b = a.den <> 0 && b.den <> 0 && a.den * b.num = a.num * b.den

  let add a b = {num = ((a.num * b.den) + (b.num * a.den)) ; den = (a.den * b.den)}
  let mul a b = {num = (a.num * b.num) ; den = (a.den * b.den)}
                
  let neg a = {num = (-1 * a.num) ; den = a.den}
  let inv a = {num = a.den ; den = a.num}
end ;;

module BooleanField : (AlgField with type t = bool) = struct
  type t = bool
    
  let zero = false
  let one = true 

  let equal = (=)
              
  let add a b = 
    if (not a) && (not b) then false
    else not( a && b) 
        
  let mul = (&&)
                
  let neg a = a
  let inv a = a
end ;;

module EllipticCurves (F : AlgField) : sig
  type point = F.t * F.t 
  val easyECs : bool 
  val onCurve : F.t -> F.t -> point -> bool
end = struct 
  type point = F.t * F.t 
                       

  let easyECs =  
    let two = F.add F.one F.one in
    let three = F.add two F.one in
    let bool1 = not(F.equal three F.zero) in
    let bool2 = not(F.equal two F.zero) in
    bool1 && bool2 
      
  let onCurve p q xy =
    let x, y = xy in
    let y_sqr = F.mul y y in
    let x_sqr = F.mul x x in
    let x_cube = F.mul x x_sqr in
    let px = F.mul p x in
    let right1 = F.add x_cube px in
    let right = F.add right1 q in
    F.equal y_sqr right
                       
end

(* Elliptic curve definitions using functor *)
(* Do not remove these modules from your code: we need it for testing. *)
module Rational_EC = EllipticCurves(RationalField)
module Boolean_EC = EllipticCurves(BooleanField)
module Float_EC = EllipticCurves(FloatField)
(* 
As mentioned in prelude, you can test this in the Toplevel, just do not place it 
in your code below, or the grader will have a fit. 
                                                It has to do with weird dependencies in the grader. It's sad. 
module Complex_EC = EllipticCurves(ComplexField)
                    *)