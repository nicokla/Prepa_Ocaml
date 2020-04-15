include "biblio\utile\3d fonctions + couleurs 2d.ml";;

(*potentiel electromagnetique*)
let potbase1 (q, (a,b)) (x,y) =
if (x,y) = (a,b) then 0.
else let k = norm(a-.x,b-.y) in
q/.(k);;

let plot_float x y =
let intof = int_of_float in
plot (intof x) (intof y);;

let rec pot_discret potbase l (x,y) =
match l with
|[]->0.
|p::q-> (potbase p (x,y)) +. 
(pot_discret potbase q (x,y));;

let affichepot potbase l =
let f = pot_discret potbase l in
draw_couleur f transf1 (500,500) (-.1.,1.) 
(-.1.,1.) (-.10.,10.);;

let l1 = [(-.1.,(0.,0.));(1.,(0.5,0.))];;
affichepot potbase1 l1;;

(*----------------------*)
(*champ*)
let multsca x (a,b) = (x*.a,x*.b);;
let add (a,b) (c,d) = (a+.c,b+.d);;

let champbase1 (q, (a, b)) (x, y) =
  let p = (x -. a, y -. b) in
    let r = q /. ((norm p) ** 3.) in
      multsca r p;;

let rec champ_discret champbase l (x,y) =
match l with
|[]->0.,0.
|p::q-> add (champbase p (x,y))
(champ_discret champbase q (x,y));;

let champ_discret2 champbase v (x, y) =
  let n = vect_length v in
    let m = ref (0., 0.) in
      for i = 0 to n - 1 do
        m := add (!m) (champbase v.(i) (x, y)) done;
      !m;;

let maxaxa x k (a, b) =
  if k <= x then (a, b) else multsca (x /. k) (a, b);;

let affiche_vect bout où quoi opt (*true=>long, ou pas*) =
  let k = norm quoi in let h = transf3 0. 10. k in
      set_color (h);
      let unit = ref (maxaxa 30. k quoi)
      and g (a, b) = (intof a, intof b) in
        if not opt then unit := multsca (12. /. k) quoi;
        let segment2 (u, v) (w, y) =
          segment (u, v) (u + w, v + y) in
          segment2 où (g !unit);
          if bout = true then begin set_line_width 3; let z = current_point() in
                lineto (fst z) (snd z);
                set_line_width 1 end;;

let affichechamp champbase (*l*) v =
  open2 (500, 500);
  let t = coord_of_pixel (500, 500) (-. 1., 1.) (-. 1., 1.) in
    let f = (*champ_discret champbase l*)
      champ_discret champbase2 v in
      for i = 0 to 49 do
        for j = 0 to 49 do
          let p = t (10 * i, 10 * j) in
            affiche_vect false (10 * i, 10 * j) (f p) false
        done; done;;

let l1 = [(1.,(0.,0.));(1.,(0.5,0.));(-.2.,(0.25,0.5))];;
affichepot potbase1 l1;;
affichechamp champbase1 l1;;

(*------------------*)
(*champ magnetostatique*)

let champbase2 (i, (a, b)) (x, y) =
  let p = (b -. y, x -. a) in
    let r = i /. ((norm p) ** 2.) in
      multsca r p;;
clear_graph();;
affichechamp champbase2 l1;;

(**)
let champ_interactif champbase =
  open2 (500, 500);
  let g = coord_of_pixel (500, 500) (-. 1., 1.) (-. 1., 1.) in
    let c = ref true and l = ref [] in
      while (!c) do
        let e = wait_next_event [Button_down; Key_pressed] in
          if e.button then begin
              let k = mouse_pos() in
                let (u, j) = g (k) in
                  l := (1., (u, j)) :: (!l);
                  clear_graph();
                  affichechamp champbase (!l); end
          else if e.key = ` ` then c := false
      done;;
champ_interactif champbase1;;
