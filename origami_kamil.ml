open List;;
(*typy*)
type point = float * float;;
type kartka = point -> int;;
(*funkcje pomocnicze*)
let square x = x *. x;;
let vector p1 p2 = (fst p2 -. fst p1, snd p2 -. snd p1);;
let add_vec (x, y) (a, b) = (x +. a, y +. b);;
let multi_vec (a, b) c = (a *. c, b *. c);;
let vector_product a b = (fst a *. snd b) -. (snd a *. fst b);;
let line_equation x (a, b) = (b, -1. *. a, (a *. snd x) -. (b *. fst x));; 
(*oblicza rownanie prostej p1 p2 przechodzacej przez x,
wynikiem jest krotka (c, d, e) taka, ze cx + dy + e = 0*)
let intersection_of_lines (a, b, c) (d, e, f) = 
  let x = ((b *. f) -. (c *. e)) /. ((a *. e) -. (b *. d)) 
  and y = ((a *. f) -. (c *. d)) /. ((b *. d) -. (a *. e)) in
  (x, y);;
let opposite x p1 p2 = 
  let (a, b) = vector p1 p2 in
  let (c, d) = (-1. *. b, a) in
  let vector_x_to_line_p1p2 = vector x (intersection_of_lines (line_equation p1 (a, b)) (line_equation x (c, d))) in
  add_vec x (multi_vec vector_x_to_line_p1p2 2.);;
(*konstruktory*)
let prostokat ld pg = fun p -> if fst p <= fst pg && fst p >= fst ld && snd p <= snd pg && snd p >= snd ld then 1 else 0;;
let kolko o r = fun p -> if sqrt (square (fst p -. fst o) +. square (snd p -. snd o)) <= r then 1 else 0;;
(*modyfikatory*)
let zloz p1 p2 k = fun x ->
  let vec_pro = vector_product (vector p1 x) (vector p1 p2) in
  if vec_pro = 0. then k x else (*x p1 p2 collinear*)
  if vec_pro > 0. then 0 else (*x on right side*)
  (+) (k x) (k (opposite x p1 p2));;
let skladaj k lines = fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) k lines;;
(*
let centr = (0., 0.);;
let a = prostokat centr (10., 10.);;
let a = zloz (5., 0.) (5., 377.) a;;
let a = zloz (5., 0.) (5., 1.) a;;
let b = zloz (-7., -7.) (300., 300.) a;;
let c = zloz (-6., -6.) (-6.1, -6.1) a;;
let d = zloz (9., 5.) (4., 2.) c;;
assert(d (10., 3.) = 2);;
*)
