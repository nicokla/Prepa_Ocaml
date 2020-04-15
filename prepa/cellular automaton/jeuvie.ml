
#open "graphics";;
open_graph "600*600";;
clear_graph();;

let bloc l x2 y2=
fill_rect (l*x2) ((l*y2)-1) l l;;


type regle={v:color vect;tour:color vect vect -> int -> int -> int -> color };;

let random_matrix v n =
  let m = make_matrix n n white and 
 p=vect_length v and k = ref 0 in
    for i = 0 to (n - 1) do
      for j = 0 to (n - 1) do
        k := random__int p;
        m.(i).(j) <- v.(!k);
      done;
    done;
    m;;

let str = string_of_int;;

let repmat mat l =
  let n = vect_length mat in
    (*open_graph ((str (n * l)) ^ "*" ^ (str (n * l)));*)
    clear_graph();
    for i = 0 to (n - 1) do
      for j = 0 to (n - 1) do
        set_color mat.(i).(j);
        bloc l i j;
      done; done;;

(*---------------------------------------*)

let rec compte x l = match l with
    | [] -> 0
    | a :: q -> if a = x then 1 + (compte x q)
        else compte x q;;

let voisinage a m n i j =
  match (i, j) with
    | (0, 0) -> compte a [m.(0).(1); m.(1).(1); m.(1).(0)]
    | (a, b) when (a, b) = (n - 1, n - 1) -> compte a [m.(n - 2).(n - 1); m.(n - 2).(n - 2); m.(n - 1).(n - 2)]
    | (a, b) when (a, b) = (0, n - 1) -> compte a [m.(0).(n - 2); m.(1).(n - 2); m.(1).(n - 1)]
    | (a, b) when (a, b) = (n - 1, 0) -> compte a [m.(n - 2).(0); m.(n - 2).(1); m.(n - 1).(1)]
    | (0, _) -> compte a [m.(0).(j - 1); m.(0).(j + 1); m.(1).(j - 1); m.(1).(j); m.(1).(j + 1)]
    | (_, 0) -> compte a [m.(i - 1).(0); m.(i + 1).(0); m.(i - 1).(1); m.(i).(1); m.(i + 1).(1)]
    | (a, _) when a = n - 1 -> compte a [m.(n - 1).(j + 1); m.(n - 1).(j - 1); m.(n - 2).(j - 1); m.(n - 2).(j); m.(n - 2).(j + 1)]
    | (_, b) when b = n - 1 -> compte a [m.(i + 1).(n - 1); m.(i - 1).(n - 1); m.(i - 1).(n - 2); m.(i).(n - 2); m.(i + 1).(n - 2)]
    | (_, _) -> compte a [m.(i - 1).(j - 1); m.(i - 1).(j); m.(i - 1).(j + 1);
          m.(i + 1).(j - 1); m.(i + 1).(j); m.(i + 1).(j + 1); m.(i).(j - 1); m.(i).(j + 1)];;

let tour_vie m n i j =
  match (m.(i).(j), (voisinage black m n i j)) with
    | (white, a) when a = 3 -> black
    | (black, a) when (a > 3 || a<2) -> white
    | _ -> m.(i).(j);;

let regle_vie={v=[|black;white|] ;tour=tour_vie};;

let vie m tour =
  let n = vect_length m in
    let m2 = make_matrix n n black in
      for i = 0 to (n - 1) do
        for j = 0 to (n - 1) do
          m2.(i).(j) <- tour m n i j
        done; done;
      m2;;

let jeu_vie M k regleg =
  let m = ref M and
  n = vect_length M in
  open_graph ((str (k * n)) ^ "*" ^ (str (k * n)));
  repmat (!m) k;
  let a = ref true in
  while (!a) do
    while (button_down()) do
      clear_graph(); m := (vie (!m) (regleg.tour));
      repmat (!m) k done;
  done;;


let jeu_vie_al n k regleg =
  open_graph ((str (k * n)) ^ "*" ^ (str (k * n)));
  let m = ref (random_matrix regleg.v n ) in
  repmat (!m) k;
  let a = ref true in
  while (!a) do
    while (button_down()) do
      clear_graph(); m := (vie (!m) regleg.tour);
      repmat (!m) k done;
  done;;


let mat =make_matrix 5 5 white;;
mat.(1).(1)<-black;
mat.(2).(1)<-black;
mat.(3).(1)<-black;
mat.(3).(2)<-black;
mat.(2).(3)<-black;;
jeu_vie mat 15 regle_vie;;

jeu_vie_al 150 5 regle_vie;;

(*--------------------------------------*)

let tour_incendie p m n i j =
  match (m.(i).(j), (voisinage red m n i j)) with
    | (a, _) when a=red -> black
    | (a, _) when a=black-> black
    | (a, x) when x > 0 && a=green -> let r=(random__float 1.) in if r<.p then red 
else green
    | (green,0) -> green;;


let regle_incendie p={v=[|red;green;black|]; tour=(tour_incendie p)};;

jeu_vie_al 20 10 (regle_incendie 1.);;
let mat2=make_matrix 150 150 green;;
mat2.(10).(10)<-red;
mat2.(11).(12)<-red;
mat2.(30).(42)<-red;
mat2.(70).(22)<-red;;
jeu_vie mat2 3 (regle_incendie 0.28);;

