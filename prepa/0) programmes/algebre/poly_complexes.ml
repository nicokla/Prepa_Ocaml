(*polynômes complexes*)
include "biblio/utile/3d fonctions + couleurs 2d.ml";;

type comp == float*float;;
type polc == comp vect;;

let comp_of_float (x:float) = (x,0.);;
let vel = vect_length;;

let fonc_of_polc p x = let n = vect_length p in
    if x = (0.,0.) then p.(0) else
      let k = ref p.(n - 1) in for i = (n - 2) downto 0 do
          k := add (prod !k x) p.(i) done;
        !k;;

let multiplierc p1 p2 =
  let a = vel p1 and b = vel p2 in
    if a <> 0 && b <> 0 then begin
        let p = make_vect (a + b - 1) (0.,0.) in
          for i = 0 to a - 1 do
            for j = 0 to b - 1 do p.(i + j) <- p.(i + j)^+(p1.(i)^* p2.(j))
            done; done; p
      end
    else [||];;

let prefix ~* p1 p2 = multiplierc p1 p2 ;;
let rec prefix ~+ v1 v2 =
  let l1 = vel v1 and l2 = vel v2 in
    if l1 < l2 then v2 ~+ v1 else
      let w = (copy_vect v1) in
        for i = 0 to l2 - 1 do
          w.(i) <- v1.(i) ^+ v2.(i) done;
        w;;

let derivec v =
    let n = vect_length v in
      let p = make_vect (n - 1) (0.,0.) in
        for i = 0 to n - 2 do
          p.(i) <- (flo (i + 1))^& v.(i + 1) done; p;;

let rec poly_of_racinesc l=
match l with []->[|1.,0.|]
|(a,b)::c->multiplierc [|(-.a,-.b);(1.,0.)|] (poly_of_racinesc c);;

let prefix ~& a v= map_vect (fun x-> a^*x) v;; 

let polunitc v j =
  let k = ref [|(1.,0.)|] and n = vect_length v in
    for i = 0 to j - 1 do
      k := (!k)~* ((inv(v.(j) ^- v.(i))) ~& [|moins v.(i); (1.,0.)|])
    done;
    for i = j + 1 to n - 1 do
      k := (!k)~* ((inv(v.(j) ^- v.(i))) ~& [|moins v.(i);(1.,0.)|])
    done; !k;;

let lagrangec v w (*pts, images*) =
  let n = vect_length v in
    if n = vect_length w then begin
        let p = ref (make_vect n (0., 0.)) in
          for i = 0 to n - 1 do
            p := (!p) ~+ ((w.(i) ~& (polunitc v i)))
          done; !p
      end
    else failwith "vecteurs de taille différentes";;


(**)
let transf1 infx maxx infy maxy (x, y) =
  let k = ref 0 and l = ref 0 in
    if x < infx then k := 0
    else if x >= maxx then k := 255
    else k := intof (256. *. ((x -. infx) /. (maxx -. infx)));
    if y < infy then l := 0
    else if y >= maxy then l := 255
    else l := intof (256. *. ((y -. infy) /. (maxy -. infy)));
      rgb !k !l 0;;


let draw_couleurc (*fonc complexe*) 
f tr (tailx, taily) (debx, finx) (deby, finy) 
(infx, supx) (infy,supy) =
  open2 (tailx, taily);
  let coord = coord_of_pixel (tailx, taily) (debx, finx) (deby, finy)
  and transf = tr infx supx infy supy in
    for i = 0 to tailx - 1 do
      for j = 0 to taily - 1 do
          set_color (transf (f (coord (i, j)))); 
          plot i j
      done; 
done;;

let drawpolc p (*pol coeff reels*)=
draw_couleurc (fonc_of_polc (map_vect comp_of_float p)) transf1 
(400,400) (-1.,1.) (-1.,1.) (-2.,2.) (-2.,2.);;

drawpolc [|0.;0.;1.|];;

(*idée anto avec quadrillage*)



