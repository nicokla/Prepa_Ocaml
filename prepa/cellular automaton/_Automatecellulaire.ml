include "biblio/utile/Graphique.ml";;

let int_of_bool b=match b with
|true->1
|false->0;;
let float_of_bool b=match b with
|true->1.
|false->0.;;

type regle={v:color vect;tour:color vect vect -> int -> int -> color };;

let str = string_of_int;;

(*let repmat2 mat n l =
    for i = 0 to (n - 1) do
      for j = 0 to (n - 1) do
        set_color mat.(i).(j);
        bloc l j i;
      done; done;;*)

let repmat mat n l =
let im = make_image() in
draw_image im 0 0;;

(*---------------------------------------*)

let rec compte a v=
let b=ref 0 in
for i=0 to (vect_length v -1) do
if v.(i)=a then b:=!b+1; done; !b;;

let voisinage1 m p q i j =
  let p = vect_length m
  and q = vect_length (m.(0))
  and u = make_vect 8 (- 1)
  and d = [|1; 1; 1; 0; - 1; - 1; - 1; 0|]
  and a = ref 0 and b = ref 0 in
    for k = 0 to 7 do
      let k2 = ((k + 6) mod 8) in
        a := i + d.(k2); b := j + d.(k);
        if !a >= 0 && !a < p && !b >= 0 && !b < q then
          u.(k) <- (m.(!a).(!b));
    done;
    u;;

let voisinage2 m a i j =
let (x,y) = mod3 (a,b) (i,j) in

let tour_vie m i j =
  match (m.(i).(j), voisinage2 m h l black i j)with
    | (white, a) when a = 3 -> black
    | (black, a) when (a > 3 || a<2) -> white
    | _ -> m.(i).(j);;

let regle_vie={v=[|black;white|] ;tour=tour_vie};;

let vie m tour =
  let n = vect_length m in
    let m2 = make_matrix n n black in
      for i = 0 to (n - 1) do
        for j = 0 to (n - 1) do
          m2.(i).(j) <- tour m i j
        done; done;
      m2;;

let jeu_vie M k regleg =
  let m = ref M and
  n = vect_length M in
  open2 (k*h) (k*l);
  repmat (!m) k;
  let a = ref true in
  while (!a) do
    while (button_down()) do
      clear_graph(); m := (vie (!m) (regleg.tour));
      repmat (!m) k done;
  done;;


let mat =make_matrix 5 5 white;;
mat.(1).(1)<-black;
mat.(2).(1)<-black;
mat.(3).(1)<-black;
mat.(3).(2)<-black;
mat.(2).(3)<-black;;
jeu_vie mat 15 regle_vie;;

jeu_vie_al 45 10 regle_vie;;

(*--------------------------------------*)

let tour_incendie2 p1 p2 m i j = (*p1:résistance, reste en feu, p2: attaque, prend feu, prop. aux arbres autour aussi *)
  match (m.(i).(j), (compte red (voisinage m i j))) with
    | (a, _) when a = black -> black
    | (a, _) when a = red -> let r = (random__float 1.) in if r <. p1 then red
          else black
    | (a, x) when x > 0 && a = green -> let r = (random__float 1.) in if r <. (p2 *. (flo x) (*/.8.*)) then red
          else green
    | (green, 0) -> green;;


let regle_incendie2 p1 p2={v=[|red;green;black|]; tour=(tour_incendie2 p1 p2)};;

let mat2=make_matrix 80 80 green;;
mat2.(10).(10)<-red;
mat2.(11).(12)<-red;
mat2.(30).(42)<-red;
mat2.(70).(22)<-red;;
jeu_vie mat2 4 (regle_incendie2 0.8 0.1);;

(*---------------------------------*)

let calculer_p d h g b v = (* d+h+g+b <= 4 *)
let r a=float_of_bool (a=red)
and w= make_vect 8 0. in
for i = 0 to 7 do if ((i mod 2)=0) then w.(i)<-(r v.(i))/.2. else w.(i)<-(r v.(i)); done;
let p=0.125*.((d*.(w.(0)+.w.(1)+.w.(2))) +. (h*.(w.(2)+.w.(3)+.w.(4)))+. (g*.(w.(4)+.w.(5)+.w.(6))) +. (b*.(w.(6)+.w.(7)+.w.(0)))) in
p;;

let tour_incendie4 d h g b m i j= (*là d'où ca vient*)
let v=voisinage m i j in
match (m.(i).(j),v) with
 | (a, _) when a=black -> black
 | (a, _) when a=red-> black
|(a,x) when a=green && (compte red v)=0->green
 | (a, x) when a=green-> let p= calculer_p d h g b v and
r = (random__float 1.) in if r<p then red else green;;


let regle_incendie4 d h g b={v=[|red;green;black|]; tour=(tour_incendie4 d h g b)};;

let mat3=make_matrix 80 80 green;;
mat3.(10).(10)<-red;
mat3.(11).(12)<-red;
mat3.(30).(42)<-red;
mat3.(70).(22)<-red;;
 
jeu_vie mat3 4 (regle_incendie4 3.5 1.5 1. 0. ) ;;

