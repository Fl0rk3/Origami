(*
    Autor: Florian Ficek
    Code Review: Kamil Pilkiewicz
*)

open List;;


(* ##### Deklaracja typów ###### *)
type point = float * float;;

type kartka = point -> int;;

    (* deklaracja typu prostej (A,B,C) *)
type prostaABC = float * float * float;;

    (* margines błędów działania na floatach *)
let eps = 1e-9;;


(* ##### FUNKCJE POMOCNICZE ##### *)
    (* funkcja wypisująca rónwanie prostej (A,B,C) otrzymanej z dwóch punktów (x1,y1),(x2,y2) *)
let prosta ((x1,y1):point) ((x2,y2):point) =
    let a = y1-.y2 and
    b = x2-.x1 and
    c = y2*.x1-.y1*.x2 in
    (a,b,c)
;;

    (* funkcja wypisująca obraz punktu (x,y) względem prostej (A,B,C) *)
let obraz ((x,y):point) ((a,b,c):prostaABC) = 
    let nx = (-.(a*.a-.b*.b)*.x-.2.*.a*.b*.y-.2.*.a*.c)/.(a*.a+.b*.b)
    and ny = ((a*.a-.b*.b)*.y-.2.*.a*.b*.x-.2.*.b*.c)/.(a*.a+.b*.b)
    in (nx,ny)
;;

    (* funkcja sprawdzająca po której stronie prostej tworzonej przez dwa punkty (x1,y1),(x2,y2)
        znajudje się punkt (p,q):
        LEWA gdy w<0
        ŚRODEK gdy w=0
        PRAWA gdy w>0
    *)
let strona ((x1,y1):point) ((x2,y2):point) ((p,q):point) = (* lewa, prawa, środek *)
    let w = (y2-.y1)*.(p-.x2)-.(q-.y2)*.(x2-.x1) in
    if w = 0. then 0
    else if w<0. then 1
    else -1
;;

(* ##### FUNKCJE PROGRAMU ##### *)
let prostokat ((x1,y1):point) ((x2,y2):point) =
    fun (x,y) ->
        if x1-.x<=eps && y1-.y<=eps && x-.x2<=eps && y-.y2<=eps then 1
        else 0
;;

let kolko ((xs,ys):point) (r:float) = 
    fun (x,y) ->
        if (x-.xs)*.(x-.xs)+.(y-.ys)*.(y-.ys)-.(r*.r)<=eps then 1
        else 0
;;

let zloz ((x1,y1):point) ((x2,y2):point) (k:kartka) = 
    let (a,b,c) = prosta (x1,y1) (x2,y2) in
    fun (x,y) -> 
        let check = strona (x1,y1) (x2,y2) (x,y) in
        if check = 0 then (k (x,y)) 
        else if check = 1 then 
            (k (x,y)) + k ((obraz (x,y) (a,b,c)))
        else 0
;;

let skladaj (lista: (point*point) list ) (k:kartka) = 
    List.fold_left (fun a (x,y) -> zloz x y a) k lista
;;