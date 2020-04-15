(*---------------------------*)
(*fonctions (x,y)->z plan couleur*)
include "bibliothèques/Graphique.ml";;
include "bibliothèques/-Vecteurs.ml";;
include "bibliothèques/complexes.ml";;
include "bibliothèques/bmp.ml";;

let transf1 inf max x = 
if x>=max then black else if x<inf then white else
let k= intof(255.*. ((x-.inf)/.(max-.inf))) in
rgb (255-k) (255-k) (255-k);;

let transf2 inf max x =
let k =intof(16777216.*. ((x-.inf)/.(max-.inf)))
in rgb (k/65536) ((k/256)mod 256) (k mod 256);;

let transf3 inf max x = 
let k= intof(256.*. ((x-.inf)/.(max-.inf)))
in if k>=256 then (rgb 255 0 0) else if k<=0 then (rgb 255 255 255)else
rgb 255 (255-k) (255-k);;
let orange = rgb 255 128 0;;

let draw_couleur f tr (tailx, taily) (debx, finx) (deby, finy) (inf, sup) =
  open2 (tailx, taily);
  let coord = coord_of_pixel (tailx, taily) (debx, finx) (deby, finy)
  and transf = tr inf sup in
    for i = 0 to tailx - 1 do
      for j = 0 to taily - 1 do
          set_color (transf (f (coord (i, j))));
          plot i j;
      done; done;;

let navigateur_2D f tr (tailx, taily) (inf, sup) =
  let debx = ref (-. 2.) and finx = ref 2. and
  deby = ref (-. 2.) and finy = ref 2. and
  c = ref true and d = ref true in
    let baba = draw_couleur f tr (tailx, taily) and
    coord = ref (coord_of_pixel (tailx, taily) (!debx, !finx)
        (!deby, !finy)) in
      baba (!debx, !finx) (!deby, !finy) (inf, sup);
      while (!c) do
        let e = wait_next_event [Button_down; Key_pressed] in
          if e.button then begin let t = mouse_pos() in
                let tt = (!coord) t in
                  if !d then begin debx := fst tt; deby := snd tt;
                      d := not (!d) end
                  else begin finx := fst tt; finy := snd tt;
                      baba (!debx, !finx) (!deby, !finy) (inf, sup);
                      coord := (coord_of_pixel (tailx, taily) (!debx, !finx)
                        (!deby, !finy)); d := not (!d) end end
          else if e.key = `-` then begin
              let a1 = !debx and a2 = !finx
              and a3 = !deby and a4 = !finy in
                debx := (3. *. a1 -. a2) /. 2.; deby := (3. *. a3 -. a4) /. 2.;
                finx := (3. *. a2 -. a1) /. 2.; finy := (3. *. a4 -. a3) /. 2.;
                baba (!debx, !finx) (!deby, !finy) (inf, sup);
                coord := (coord_of_pixel (tailx, taily) (!debx, !finx)
                  (!deby, !finy)); d := true end
          else if e.keypressed then c := false
      done;;


(*--------------------------------*)
(*3D mode nuage points (faire traits,surf) *)
(*pour simplifier on prend 40*40, avec 10 pix chacun,0.1,
 de -2 a 2*)

let draw_3d_points f phi th echz=
  for i = 0 to 60 do
    for j = 0 to 60 do
      let k = (flo (i - 30)) /. 10. and
      l = (flo (j - 30)) /. 10. in
        let m = (10. *. (f (k, l))) in
          let n = [|10. *. (flo (i - 30)); 10. *. (flo (j - 30));echz*. m|] in
            let o = (rota phi th n) in
              plot (300 + intof o.(0)) (300 + intof o.(1))
    done; done;;

let draw_3d_lignes f phi th echz=
  let M = make_matrix 60 60 (0, 0) in
    for i = 0 to 59 do
      for j = 0 to 59 do
        let k = (flo (i - 30)) /. 10. and
        l = (flo (j - 30)) /. 10. in
          let m = (10. *. (f (k, l))) in
            let n = [|10. *. (flo (i - 30)); 10. *. (flo (j - 30)); echz*.m|] in
              let o = (rota phi th n) in
                M.(i).(j) <- ((300 + intof o.(0)), (300 + intof o.(1)))
      done; done;
    for i = 0 to 58 do
      for j = 0 to 58 do
        let p = M.(i).(j) and r = M.(i + 1).(j)
        and s = M.(i).(j + 1) in
          segment p s;
          segment p r
      done;
    done;;

let tracertruc () =
moveto 0 100;
lineto 200 100;
moveto 100 0;
lineto 100 200;;

let attend3d ledraw f echz =
  open2 (600, 600);
  ledraw f 0. 0. echz;
  let c = ref true in
    while (!c) do
      let e = wait_next_event [Button_down; Key_pressed;Poll] in
        if e.button then begin let t = mouse_pos() in
              if zone 0 0 200 200 (fst t) (snd t) then begin
                  clear_graph();
                  tracertruc();
                  ledraw f (flo ((fst t) - 100) /. 200. *. 2. *. pi)
                  (flo ((snd t) - 100) /. 200. *. 2. *. pi) echz end end
        else if e.key = ` ` then c := false
    done;;
(**)

let f1 (x, y) = (x*.y);;
let f2 (x,y)=sqrt( x*.x+.y*.y );;
let f3 (x,y) = 1./.f2(x,y);;
let f4 (x,y) = sin x +. sin y;;

(*attend3d draw_3d_lignes f2 10.;;

draw_couleur f4 transf1 (500,500) (-.4.,4.) (-.4.,4.) (-2.0,2.0);;

navigateur_2D f4 transf3 (500,500) (-2.0,2.0);;

faire_bitmap "C:/Program Files/WinCaml3_1/WinCaml/bibliothèques/images/0.bmp";;*)

(*--------------------*)

(*let représenter4D*)