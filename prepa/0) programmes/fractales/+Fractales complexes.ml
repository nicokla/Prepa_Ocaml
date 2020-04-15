(*Nicolas Klarsfeld*)
(*------------Fractales complexes, non divergence-----------------*)

#open "graphics";;
include "biblio/utile/complexes.ml";;
include "biblio/utile/3d fonctions + couleurs 2d.ml";;

let absre x = if x>=0. then x else -.x;;
let comp (a,b) (x,y) =(a,b);;
let julia f (x, y) = fun
(x0, y0) (x1, y1) -> f (x, y) (x1, y1);;

let test a f ray n (x, y) (*ray=le rayon de conv au carré,souvent=2*2=4*)=
  let i = ref 0
  and m = a (x, y) in let f0 = f m in
    let x1 = ref (fst m)
    and y1 = ref (snd m) in
      let b = ref true in
        while (!i < n) && !b do
          let k = f0 (!x1, !y1) in
            x1 := fst k;
            y1 := snd k;
            if (!x1 *. !x1 +. !y1 *. !y1) >. ray then b := false else
              i := !i + 1;
        done;
        !i, (!x1,!y1);;

let frac r a f ray n (*règlecoul intér/ext, transfogeom, fonc, iter*)=
  open_graph "400*400";
  for i = 0 to 400 do
    for j = 0 to 400 do
      let x = float_of_int (i - 200) /. 100. in
        let y = float_of_int (j - 200) /. 100. in
          let m = (test a f ray n (x, y)) in
            if (fst m) = n then (fst r) m else (snd r) m;
            plot i j;
    done; done;;


let frac_zoom r a f ray n (tailx, taily) (debx, finx)(deby, finy) =
open2 (tailx,taily);
  let coord = coord_of_pixel (tailx, taily) (debx, finx) (deby, finy) in
  for i = 0 to tailx do
    for j = 0 to taily do
      let k = coord (i,j) in
         let m = (test a f ray n k) in
            if (fst m) = n then (fst r) m else (snd r) m;
            plot i j;
    done; done;;


(*--------------------------*)
(*colorations*)

let couleur n =
	match (n mod 4) with
		| 0 -> (rgb 255 0 0)
		| 1 -> (rgb 255 128 0)
		| 2 -> (rgb 255 255 0)
		| _ -> (rgb 255 128 0);;

let changecol x = [|(x/65536); ((x/256)mod 256); (x mod 256)|];;


let r_black (k, (x, y)) = set_color black;;

let r_white (k, (x, y)) = set_color white;;

let r_basic (k, (x, y)) = set_color (couleur k);;

let r_arg (k, (x, y)) =
    let p = changecol (int_of_float (16777216. *. (arg (x, y)+.pi) /. (2. *. pi))) in
      set_color (rgb p.(0) p.(1) p.(2));;

let r_norm (k, (x, y)) = let p = changecol (int_of_float (16777216. *. (norm (x, y)) /. (2.))) in
      set_color (rgb p.(0) p.(1) p.(2));;

let règle1 = (r_black,r_basic);;
let règle2 = (r_arg,r_black);;
let règle3 = (r_norm,r_black);;
let règle4 = (r_norm,r_basic);;
let règle5 = (r_arg,r_basic);;
let règle6 = (r_black,r_white);;

(*------------------------*)
(*fonctions*)

let f_mand (x,y) (x1,y1) = add (carre (x1,y1)) (x,y);;
let mand n= frac règle6 Id f_mand 4. n;;
mand 100;; set_color black;; dessineraxes (400, 400) (-.2.,2.)(-.2.,2.);; plot 100 300;; set_color red;;
(*si f0=f (x,y) let mand2 (a,b) n = frac règle1 (comp (a,b)) f_mand 4. n;;
mand2 (0.,1.) 50;;*)

(*frac_zoom règle1 Id f_mand 4. 50 (600,600) (-1.,2.) (-2.,1.);;*)

let f_jul = julia f_mand;;
let juliaa (x,y) n = frac règle2 Id (f_jul (x,y)) 4. n;;
juliaa (-.0.75,0.145) 100;;
juliaa (-.0.62,0.43) 100;;
juliaa (-.0.15,0.75) 100;;
juliaa (-.1.17, 0.0) 100;;
juliaa (0.3, 0.54) 100;;
juliaa (-.0.05, -.0.69) 100;;

let frac_inv n =
frac règle1 inv f_mand 4. n;;
frac_inv 100;;

let f_multibrot m (x,y) (x1,y1) (*puiss m pas 1 -1 ou 0*)=
 add (puiss (x1,y1) m) (x,y);;
let multibrot m n= frac règle1 Id (f_multibrot m) 4. n;;
multibrot (5) 100;;

let f_mandelbar m (x,y) (x1,y1) = add (puiss (x1,-.y1) m) (x,y);;
let mandelbar m n= frac (r_black,r_basic) Id (f_mandelbar m) 4. n;;
mandelbar 2 100;;

let f_bidule (x,y) (x1,y1) = prod (x1,y1) (x1-.1.,y1);;
let bidule n = frac règle4 Id f_bidule 4. n;;
bidule 50;;

let f_burnship (x,y) (x1,y1) = add (carre(absre x1,absre y1)) (x,y) ;;
let burnship n = frac règle1 Id f_burnship 4. n;;
burnship 50;;

(*---------------------------------------*)

let navigateur_fra (tailx, taily) r a f ray n (*stop si espace*)=
  let debx = ref (-. 2.) and finx = ref 2. and
  deby = ref (-. 2.) and finy = ref 2. and
  c = ref true and d = ref true in
    let baba = (frac_zoom r a f ray n) and
    coord = ref (coord_of_pixel (tailx, taily) (!debx, !finx)
        (!deby, !finy)) in
      baba (tailx, taily) (!debx, !finx) (!deby, !finy);
      while (!c) do
        let e = wait_next_event [Button_down; Key_pressed] in
          if e.button then begin let t = mouse_pos() in
                let tt = (!coord) t in
                  if !d then begin debx := fst tt; deby := snd tt;
                      d := not (!d) end
                  else begin finx := fst tt; finy := snd tt;
                      baba (tailx, taily) (!debx, !finx) (!deby, !finy);
                      coord := (coord_of_pixel (tailx, taily) (!debx, !finx)
                        (!deby, !finy)); d := not (!d) end end
          else if e.key = `-` then begin
              let a1 = !debx and a2 = !finx
              and a3 = !deby and a4 = !finy in
                debx := (3. *. a1 -. a2) /. 2.; deby := (3. *. a3 -. a4) /. 2.;
                finx := (3. *. a2 -. a1) /. 2.; finy := (3. *. a4 -. a3) /. 2.;
                baba (tailx, taily) (!debx, !finx) (!deby, !finy);
                coord := (coord_of_pixel (tailx, taily) (!debx, !finx)
                  (!deby, !finy)); d := true end
          else if e.key = ` ` then c := false
      done;;

navigateur_fra (400,400) règle1 Id f_mand 4. 40;;

(*-----------------*)

let navigateur_fra2 (tailx, taily) r a f ray n =
  let debx = ref (-. 2.) and finx = ref 2. and
  deby = ref (-. 2.) and finy = ref 2. and
  c = ref true and point = ref (0, 0) and
  baba = (frac_zoom r a f ray n) in
    let coord = ref (coord_of_pixel (tailx, taily) (!debx, !finx)
        (!deby, !finy)) in
      let reactualise r = begin
          debx := fst (fst r); deby := fst (snd r);
          finx := snd (fst r); finy := snd (snd r);
          baba (tailx, taily) (!debx, !finx) (!deby, !finy);
          coord := (coord_of_pixel (tailx, taily) (!debx, !finx)
            (!deby, !finy)) end; in
        baba (tailx, taily) (!debx, !finx) (!deby, !finy);
        while (!c) do
          let e = wait_next_event [Button_down; Key_pressed] in
            if e.button then point := mouse_pos()
            else if e.key = `-` then
              let r = zoom 4. (tailx, taily) (!debx, !finx) (!deby, !finy) (!point)
              in reactualise r
            else if e.key = `+` then
              let r = zoom 0.25 (tailx, taily) (!debx, !finx) (!deby, !finy) (!point)
              in reactualise r
            else if e.key = ` ` then c := false
        done;;

navigateur_fra2 (400,400) règle1 Id f_mand 4. 40;;

(*----------*)

let interactif_julia r a f ray n (debx, finx) (deby, finy) =
  open_graph "400x400";
  frac r a f ray n; set_color white;
  dessineraxes (400, 400) (debx, finx) (deby, finy);
  let point = ref (0., 0.) in
    try while true do
        let e = wait_next_event [Button_down; Key_pressed; Poll] in
          if e.button then let k = mouse_pos() in
              point := coord_of_pixel (400, 400) (debx, finx) (deby, finy) k;
              frac_zoom r a (julia f !point) ray n (100, 100) (debx, finx) (deby, finy)
      done; !point;
    with | Graphic_failure "graphic screen not opened" -> !point;;


interactif_julia règle1 Id f_mand 4. 50 (-. 2., 2.) (-. 2., 2.);;
interactif_julia règle1 inv2 f_mand 4. 50 (-. 4., 4.) (-. 4., 4.);;
interactif_julia règle2 Id (f_mandelbar 2) 4. 50 (-. 2., 2.) (-. 2., 2.);;
frac règle4 Id (julia (f_mandelbar 2) (-.0.78, 0.08)) 4. 50;;

(*copie_ecran (dossier_im^"juliabar.bmp");;*)
(*faire newton*)




