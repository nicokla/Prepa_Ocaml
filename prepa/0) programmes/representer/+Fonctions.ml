
include "biblio/utile/Graphique.ml";;
include "biblio/utile/Temps.ml";;

(*---------------*)
(*echelles !*)


let drawnormal (tailx, taily) (infx, supx) (infy, supy) f =
  open2 (tailx, taily); clear_graph();
  moveto 0 (*(intof (flo (tailx) *. (!a -. infx) /. (supx -. infx)))*)
  (intof (flo (taily) *. (f (infx) -. infy) /. (supy -. infy)));
  let pasx = (supx -. infx) /. flo (tailx)
  and i = ref 0 and a = ref infx in
    while !a < supx do
      let k = f (!a) in
        lineto !i (*(intof (flo (tailx) *. (!a -. infx) /. (supx -. infx)))*)
        (intof (flo (taily) *. (k -. infy) /. (supy -. infy)));
        i := !i + 1;
        a := !a +. pasx done;
    dessineraxes (tailx, taily) (infx, supx) (infy, supy);;


drawnormal (200,200) (-2.,2.) (0.,1.) (fun x -> x*.x);;


let drawparam (tailx, taily) (infx, supx) (infy, supy) (inft, supt) past fx fy =
  open2 (tailx, taily); clear_graph();
  let a = ref inft in
    moveto (intof (flo (tailx) *. (fx (!a) -. infx) /. (supx -. infx)))
    (intof (flo (taily) *. (fy (!a) -. infy) /. (supy -. infy)));
    while !a < supt do
      let k = fx (!a) and m = fy (!a) in
        lineto (intof (flo (tailx) *. (k -. infx) /. (supx -. infx)))
        (intof (flo (taily) *. (m -. infy) /. (supy -. infy)));
        a := !a +. past done;
    dessineraxes (tailx, taily) (infx, supx) (infy, supy);;

drawparam (400, 400) (-. 4., 4.) (-. 4., 4.) (0., 6.28) 0.1
(fun x -> 2. *. cos x +. cos (2. *. x)) (fun x -> 2. *. sin x -. sin (2. *. x));;


let drawpol (tailx, taily) (infx, supx) (infy, supy) (inft, supt) past f=
drawparam (tailx, taily) (infx, supx) (infy, supy) (inft, supt) past
(fun t->cos t *. f(t)) (fun t->sin t *. f(t));;

drawpol (400, 400) (-. 4., 4.) (-. 4., 4.) (0., 3.) 0.05 (fun x->x);; 

(*dérivée approximée*)
(*tangente en x*)
(*fonction 3d avec couleurs dans le plan (avec couleur -=vert,+=rouge)*)

let lissajou a b =
drawparam (500, 500) (-. 1.2, 1.2) (-. 1.2, 1.2) (0., 2.*.pi) 0.005
(fun x-> cos (a*.x)) (fun x-> sin (b*.x)) ;;

lissajou 3. 5. ;;

let drawparamdyna (tailx, taily) (infx, supx) (infy, supy) (inft, supt) past fx fy n dt =
  for i = 0 to n do
    drawparam (tailx, taily) (infx, supx) (infy, supy) (inft, supt) past (fx i) (fy i);
    wait dt;
    clear_graph() done;;

drawparamdyna (500, 500) (-. 1.2, 1.2) (-. 1.2, 1.2) (0., 2. *. pi) 0.005
(fun i -> (fun x -> cos (x))) (fun i -> (fun x -> sin (flo i *. x))) 10 0.5;;

let anim a =
drawparamdyna (500, 500) (-. 1.2, 1.2) (-. 1.2, 1.2) (0., 2. *. pi) 0.005
(fun i -> (fun x -> cos (x))) 
(fun i -> (fun x -> sin ((a *. x)+. (flo i)/.10.))) 40 0.1;;

anim 3.5;;

clear_graph();;

(*fenetre s'ajuste auto*)
