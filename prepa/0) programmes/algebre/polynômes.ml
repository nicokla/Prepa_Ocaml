
include "biblio\programmes\algebre\-Vecteurs.ml";;
include "biblio/programmes/representer/+Fonctions.ml";;
(*-----------------------*)
(*polynômes*)

type pol == float vect;;

let fonc_of_pol p x = let n = vect_length p in
    if x = 0. then p.(0) else
      let k = ref p.(n - 1) in for i = (n - 2) downto 0 do
          k := !k *. x +. p.(i) done;
        !k;;

let rec multiplier p1 p2 =
  let a = vel p1 and b = vel p2 in
    if a <> 0 && b <> 0 then begin
        let p = make_vect (a + b - 1) 0. in
          for i = 0 to a - 1 do
            for j = 0 to b - 1 do p.(i + j) <- p.(i + j) +. p1.(i) *. p2.(j)
            done; done; p
      end
    else [||];;

let prefix @* p1 p2 = multiplier p1 p2;;

let deg p = (vect_length p)-1;;


let rec multiplierli l =
match l with
|[a]->a
|a::(b::c)->multiplierli ((a @* b)::c)
|_->[||];;

multiplier [||] [|1.|];;

let polunit v j =
  let k = ref [|1.|] and n = vect_length v in
    for i = 0 to j - 1 do
      k := multiplier (!k) ((1. /. (v.(j) -. v.(i))) $& [|-. v.(i); 1.|])
    done;
    for i = j + 1 to n - 1 do
      k := multiplier (!k) ((1. /. (v.(j) -. v.(i))) $& [|-. v.(i); 1.|])
    done; !k;;


let lagrange (v: float vect) (w: float vect) (*pts, images*) =
  let n = vect_length v in
    if n = vect_length w then begin
        let p = ref (make_vect n 0.) in
          for i = 0 to n - 1 do
            p := (!p) $+ ((w.(i) $& (polunit v i)))
          done; !p
      end
    else failwith "vecteurs de taille différentes";;

let rep_lagrange v w (infx, supx) (infy, supy) =
  (*let infx = petit v -. 1. and
  supx = gros v +. 1. in*)
  drawnormal (500, 500) (infx, supx) (infy, supy)
  (fonc_of_pol (lagrange v w));
  set_color red;
  let pixpix = pixel_of_coord (500, 500) (infx, supx) (infy, supy) in
    for i = 0 to (vect_length v) - 1 do
      let p = (pixpix (v.(i), w.(i)))
      in fill_rect (fst p) (snd p) 2 2; done;
    set_color black;;

rep_lagrange [|- 1.; 0.; 3.;2.|] [|1.; 0.; 1.;1.|] 
(- 4.,4.)(- 4.,4.);;

let app_presque u a v =(*renvoie le premier rg proche ou -1 sinon*)
  let n=vect_length v in if n = 0 then -1 else
    let c = ref (-1) and k = ref 0 in while !c=(-1) && !k<n do
        if abs_float (v.(!k) -. a)<= u then c:=(!k) else k := (!k) + 1 done; !c;;

let lagrange_interactif (debx, finx) (deby, finy) =
  open2 (500, 500);
  let g = coord_of_pixel (500, 500) (debx, finx) (deby, finy) in
    let c = ref true
    and v = ref [||] and w = ref [||] in
      while (!c) do
        let e = wait_next_event [Button_down; Key_pressed;Poll] in
          if e.button then begin let k = mouse_pos() in
                let (u, j) = g (k) in let o = app_presque 0.2 u !v in
                    if o = (- 1) then begin
                        v := (concat_vect [|u|] !v);
                        w := (concat_vect [|j|] !w);
                        set_line_width 1;
                        rep_lagrange (!v) (!w) (debx, finx) (deby, finy) end
                    else if abs_float ((!w).(o) -. j) <= 0.2 then while button_down() do
                        let (a, b) = g (mouse_pos()) in !v.(o) <- a; !w.(o) <- b;
                          rep_lagrange (!v) (!w) (debx, finx) (deby, finy) done;
            end
          else if e.key = ` ` then c := false
      done;
      lagrange !v !w;;
lagrange_interactif (- 4.,4.) (- 4.,4.);;

(**)

let rec puiss_poly v n =
  match n with
    | 0 -> [|1.|]
    | 1 -> v
    | _ -> match n mod 2 with
          | 0 -> puiss_poly (multiplier v v) (n / 2);
          | _ -> multiplier v (puiss_poly (multiplier v v) (n / 2));;

let rendre_unitaire v =
  let n = vect_length v in
    let k = (1. /. v.(n - 1)) in
      k $$ v;;

let derive v =
    let n = vect_length v in
      let p = make_vect (n - 1) 0. in
        for i = 0 to n - 2 do
          p.(i) <- flo (i + 1) *. v.(i + 1) done; p;;

let rec fairen f n x = match n with 0 -> x
    |_-> f (fairen f (n - 1) x);;

let derive_nieme n = fairen derive n;;


(*let racines_reelles v =*)
let rec dicho f min max eps =
  let fmin = f min and fmax = f max in
    if fmin *. fmax > 0.
    then failwith "Aucune racine"
    else if max -. min < eps then (min, max) (* retourne un intervalle *)
    else let mil = (min +. max) /. 2. in
      if (f mil) *. fmin < 0.
      then dicho f min mil eps
      else dicho f mil max eps ;;
  (* Approximation de la racine carrée de 2 *)
  dicho (fun x -> x *. x -. 2.) 0. 10. 0.000000001;;

let multp a p =map_vect (fun x->x*.a) p;;

let compose_polynome p1 p2=
let n = vect_length p1 in
      let k = ref [|p1.(n - 1)|] in for i = (n - 2) downto 0 do
          k :=(multiplier !k  p2) $+ [|p1.(i)|] done;
        !k;;

let prefix @ p1 p2 =compose_polynome p1 p2;;

[|0.;0.;0.;1.|]@[|1.;1.|];;

let rec binom k = (*coeff généralisé*)
  let v = make_vect k [|0.; 1.|] in
    for i = 0 to k - 1 do
      let k = flo i in
        v.(i).(0) <- (-. k);
        v.(i) <- (multp (1. /. (k+.1.)) v.(i))
    done;
    let l = list_of_vect v in
      multiplierli l;;

let binonome k n =
if k = 0 || k = n then 1
else if k < 0 || k>n then 0 else
intof(fonc_of_pol (binom k) (flo n));;

let rec poly_of_racines l=
match l with []->[|1.|]
|a::b->multiplier [|-.a;1.|] (poly_of_racines b);;

(**)
(*poly caracteristique d'une matrice*)

let comat2 m a b (*mat ac lign et col en -*)=
  if vérifier m = false then failwith "chat va pas"
  else let l = ligne m and c = colonne m in
      if (a >= l or b >= c) then failwith "mec, t'es ouf ou koi ?!"
      else let n = make_matrix (l - 1) (c - 1) [|0.|] in
          for i = 0 to l - 1 do
            if i < a then begin
                for j = 0 to b - 1 do n.(i).(j) <- m.(i).(j) done;
                for j = b + 1 to c - 1 do n.(i).(j - 1) <- m.(i).(j) done; end
            else if i > a then begin
                for j = 0 to b - 1 do n.(i - 1).(j) <- m.(i).(j) done;
                for j = b + 1 to c - 1 do n.(i - 1).(j - 1) <- m.(i).(j) done; end
          done;
          n;;

let hello m =
  let n = vect_length m in
    let a = make_matrix n n [||] in
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          if i = j then a.(i).(j) <- [|m.(i).(j); -. 1.|]
          else a.(i).(j) <- [|m.(i).(j)|] done done;
a;;

let det2 m2 =
  if vérifier_carré m2 = false then failwith "aie" else
    let coucou = hello m2 in
      let rec deter m n =
        match n with
          | 0 -> [|1.|]
          | 1 -> m.(0).(0)
          | _ -> let k = ref [|0.|] and j = ref 1. in
                for i = 0 to (n - 1) do
                  k := !k $+ ((!j) $& m.(i).(0)) @* deter (comat2 m i 0) (n - 1);
                  j := (-. (!j)) done; !k
      in deter coucou (ligne m2);;

det2 [|[|1.;1.|];[|0.;1.|]|];;

(*eval2(avec les racines et coef dominant)
decomposer en fonctions...
legendre poly,integrale nulle en?,
division eucl, bezout;;*)

let simplifie v = if v = [||] then [||] else
let i = ref (vect_length v - 1) in
while v.(!i)<>0. do ;;

let divis a b = (*renvoie (quot,rest) de a/b des poly*)
let q = ref [||] and r = ref a in
while deg a >= deg ;;

let rec pgcd a b = match a, b with
    | [|0|], _ -> b
    | _, [|0|]-> a
    | _ -> pgcd b (reste a b);;


(*-----------------------*)
(*fractions rationnelles*)

type fr == (pol*pol);;

(*
decompose_rat;;
eval_rat;;*)

(*trigo*)
