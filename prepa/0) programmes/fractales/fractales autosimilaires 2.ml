(*------------------------------------*)
(*---------Fractales auto-similaires---------*)

#open "graphics";;
include "biblio\utile\complexes.ml";;
include "biblio\utile\Graphique.ml";;
include "biblio\utile\Temps.ml";;
include "biblio/utile/bmp.ml";;

let intof=int_of_float;;
let intof2 (a,b) = (intof a, intof b);;
let flo2 (a,b) = (flo a, flo b);;
let norm2 (x, y) = (x *. x +. y *. y);;


let coord (a,b) (c,d) m th = 
let e = cos th and f = sin th in
let g = m*.(c-.a) and h = m*.(d-.b) in
let t = (e*.g -.f*.h,e*.h+.f*.g) in
t;;

let anti_coord (a, b) (c, d) (t1, t2) (*attention angle, sur pi!*) =
  let u = (c -. a, d -. b) and v = (t1 -. a, t2 -. b) in
    let h = norm u and i = norm v in
      let m = i /. h and
      th = asin ((antiscal u v) /. (h *. i)) /. pi
      in if scal u v >= 0. then
          (m, th) else (m, 1.-.th);;

let coord2 (a,b) (c,d) x y  =
let k = (x*.(c-.a),x*.(d-.b)) 
and l = (y*.(b-.d),y*.(c-.a)) in
add k l;;

let anti_coord2 (a, b) (c, d) (t1, t2) =
  let u = (c -. a, d -. b) and v = (b-.d,c-.a) and
w = (t1 -. a, t2 -. b) in
    let k = norm2 u in
      let x = (scal u w) /. k and y = (scal v w) /. k in
        (x, y);;

(*----------------------*)

let rec faire k l =
match l with
[]->()
|a::b-> k(a); faire k b;;

let boudou nato opt2 seglist (a, b) (c, d) =
  let transfcoord = opt2 (a, b) (c, d) in
    let transftout= map (fun k -> add (a, b) (transfcoord (fst k) (snd k))) in
      let rec boudou1 l = match l with
          [] -> () | [a] -> ()
          | a :: b -> nato (a) (hd b);
              boudou1 b in
        faire boudou1 (map transftout seglist);;


let rec autosimilgen mincar (*taille + ptite*)
opt1 (*dessin pendant?*)
opt2 (*cart=coord2 ou pol=coord*)
seglist (*si ab=00,10*)
(a, b) (c, d) =
  if norm2 (c -. a, d -. b) <. mincar then
    begin
      moveto (intof a) (intof b);
      lineto (intof c) (intof d)
    end
  else begin
      if opt1 then begin moveto (intof a) (intof b);
          lineto (intof c) (intof d) end;
      let k = (autosimilgen mincar opt1 opt2 seglist) in
        boudou k opt2 seglist (a, b) (c, d) end;;

let rec autosimilgen2 n (*etapes*)
opt1 (*dessin pendant?*)
opt2 (*cart=coord2 ou pol=coord*)
seglist (*si ab=00,10*)
(a, b) (c, d) =
  if n=0 then
    begin
      moveto (intof a) (intof b);
      lineto (intof c) (intof d)
    end
  else begin
      if opt1 then begin moveto (intof a) (intof b);
          lineto (intof c) (intof d) end;
      let k = (autosimilgen2 (n-1) opt1 opt2 seglist) in
        boudou k opt2 seglist (a, b) (c, d) end;;

let afficher p q truc=
  open2 (600, 600); clear_graph();
  truc p q;;

let rec faire_foug l =
  match l with [] -> []
    | a :: b -> let m, th = a in
          let k = [(1., 0.); (1. +. m *. cos (pi *. th), m *. sin (pi *. th))] in
            k :: (faire_foug b);;

let foug l n (a, b) (c, d) = let v = faire_foug l in
    afficher (a, b) (c, d) (autosimilgen2 n true coord2 v) ;;

let p1= (300.,0.) and q1 =(300.,200.);;
foug [(0.25,0.4);(0.25,-.0.4);(0.85,0.016)] 10 p1 q1;;
foug [(0.5,0.5);(0.5,0.);(0.5,-.0.5)] 8 p1 q1;;
foug [(0.5,1./.3.);(0.5,-.1./.3.);(0.5,0.)] 8 p1 q1;;

let kochgen l n (a, b) (c, d) = 
    afficher (a, b) (c, d) (autosimilgen2 n false coord2 
[(0.,0.)::l]) ;;

let p2 = (5.,5.) and q2 =(500.,5.);;
let k = 1. /. 3. in let m = 2. *. k in
    let v3 = [(k, 0.); (0.5, sqrt (3.) /. 6.); (m, 0.); (1., 0.)]
    in kochgen v3 8 p2 q2;;

let toto x k (*de ]0.25;0.5[*) =
  let v = [(x, 0.); (0.5, sqrt (x ** 2. -. (0.5 -. x) ** 2.)); (1. -. x, 0.); (1., 0.)]
  in kochgen v k p2 q2;;
toto 0.4 6;;
toto 0.48 6;;


let k = 1. /. 3. in let j = 2. *. k in
   let v1 =  [(k, 0.); (k, k); (j, k); (j, 0.); (1., 0.)]
in kochgen v1 8 p2 q2;;

let p0 = (200., 200.) and q0 = (400., 200.);;
let v4 = [0.5,0.5;1.,0.] in
kochgen v4 12 p0 q0;;

let v5 = [[0.,0.; 1./.3.,0.];[2./.3.,0.;1.,0.]];;
let yoman n= autosimilgen2 n false coord2 v5;;
visu (fun i->afficher p2 q2 (yoman i)) 1 6 0.5;;

(*-------------------*)

(*let kochquadra2 = autosimilgen2 2 false coord2 v1;;
afficher kochquadra2;;*)
(*visu (fun i -> afficher p0 q0 (autosimilgen2 i false coord2 v4)) 1 15 0.5;;*)

(*--------------*)
(*interactif feuille*)

let faire_arbre n p q =
  open_graph "600x700"; clear_graph();
  moveto (fst (intof2 p)) (snd (intof2 p));
  lineto (fst (intof2 q)) (snd (intof2 q));
  let points = ref [] and point = ref (0., 0.) and c = ref true in
    while (!c) do
      let e = wait_next_event [Button_down; Key_pressed] in
        if e.button then begin let k = mouse_pos() in
              lineto (fst k) (snd k);
              moveto (fst (intof2 q)) (snd (intof2 q));
              point := anti_coord p q (add (flo2 k) (add p (moins q)));
              points := !point :: !points end
        else if e.key = `a` then foug (!points) n p q
        else if e.key = `z` then begin if !points <> [] then points := tl !points end
        else if e.key = ` ` then c := false
    done;
    !points;;

let p4= (300.,0.) and q4 =(300.,200.) and a4 =(300.,100.) ;;
faire_arbre 9 p4 q4;;

(*size_x();;*)
copie_ecran (dossier_im^"autosimil/9.bmp");;
copie_ecran_gen (dossier_im^"alakoch/8.bmp") (250) 350 500 500 ;;

(*-----*)

let faire_kochou n p q =
  open_graph "1000x800"; clear_graph();
  moveto (fst (intof2 p)) (snd (intof2 p));
  lineto (fst (intof2 q)) (snd (intof2 q));
  let points = ref [(1., 0.)] and point = ref (0., 0.) and c = ref true in
    while (!c) do
      let e = wait_next_event [Button_down; Key_pressed] in
        if e.button then begin let k = mouse_pos() in
              lineto (fst k) (snd k);
              point := anti_coord2 p q (flo2 k);
              points := !point :: !points end
        else if e.key = `a` then kochgen (!points) n p q
        else if e.key = ` ` then c := false
    done;
    (0., 0.) :: (!points);;

let p5= (300.,400.) and q5 =(700.,400.);;
faire_kochou 4 p5 q5;;

(*faire fourmi, dragon*)



