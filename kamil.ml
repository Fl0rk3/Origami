(*
Autor: Kamil Pilkiewicz
Code Review: Florian Ficek 
*)
open List;;
(*typy*)
type point = float * float;;
type kartka = point -> int;;
(*margines bledu*)
let eps = 1e-9;;
(*funkcje pomocnicze*)
let square x = x *. x;;
let vector (a, b) (c, d) = (c -. a, d -. b);;
let add_vec (a, b) (c, d) = (a +. c, b +. d);;
let multi_vec (a, b) c = (a *. c, b *. c);;
let vector_product (a, b) (c, d) = (a *. d) -. (b *. c);;
let line_equation (x1, x2) (a, b) = (b, -1. *. a, (a *. x2) -. (b *. x1));; 
(*oblicza rownanie prostej przechodzacej przez x rownoleglej do wektora (a, b),
wynikiem jest krotka (c, d, e) taka, ze cx + dy + e = 0*)
let intersection_of_lines (a, b, c) (d, e, f) = 
  let x = ((b *. f) -. (c *. e)) /. ((a *. e) -. (b *. d)) 
  and y = ((a *. f) -. (c *. d)) /. ((b *. d) -. (a *. e)) in
  (x, y);;
let opposite x p1 p2 = 
  let (a, b) = vector p1 p2 in
  let (c, d) = (-1. *. b, a) in
  let vector_x_to_line_p1p2 = vector x (intersection_of_lines (line_equation p1 (a, b)) (line_equation x (c, d))) in
  add_vec x (multi_vec vector_x_to_line_p1p2 2.)
;;
(*konstruktory*)
let prostokat (ld1, ld2) (pg1, pg2) = 
  fun (p1, p2) -> if p1 -. pg1 <= eps 
  && ld1 -. p1 <= eps 
  && p2 -. pg2 <= eps 
  && ld2 -. p2  <= eps 
  then 1 else 0
;;
let kolko (o1, o2) r = 
  fun (p1, p2) -> if (square (p1 -. o1) +. square (p2 -. o2)) -. (r *. r) <= eps then 1 else 0
;;
(*modyfikatory*)
let zloz p1 p2 k = fun x ->
  let vec_pro = vector_product (vector p2 x) (vector p2 p1) in
  if vec_pro = 0. then k x else (*x p1 p2 collinear*)
  if vec_pro < 0. then 0 else (*x on right side*)
  (k x) + k (opposite x p1 p2)
;;
let skladaj lines k = fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) k lines;;
