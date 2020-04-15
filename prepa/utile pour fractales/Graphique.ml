#open "sys";;

let wait t (*en s*) =
let k = time() in
while time() < k+.t do () done;;

#open "graphics";;

let flo = float_of_int and intof = int_of_float
and stri = string_of_int;;
let Pi = 3.141592654;;
let pi = Pi;;
let e = 2.718281828;;

let bloc l x2 y2=
fill_rect (l*x2) ((l*y2)-1) l l;;

let open2 (tailx,taily) = 
let tailx1 = stri tailx and taily1 = stri taily in
 open_graph (tailx1^"x"^taily1) ;;

let zone x y lx ly a b =
  match a, b with
    | a, b when (a >= x) && (a < (x + lx)) && (b >= y) && (b < (y + ly)) -> button_down();
    | _ -> false;;

let deplacer droite haut=
let k = current_point() in
moveto (fst k + droite) (snd k + haut);;

let deplacer2 droite haut =
let k = current_point() in
lineto (fst k + droite) (snd k + haut);;

let segment (a,b) (c,d) =
moveto a b;
lineto c d;;

let pixel_of_coord (tailx, taily) (debx,finx) (deby,finy) (x,y)=
let a = intof((flo tailx) *. (x-.debx)/.(finx-.debx))
and b = intof((flo taily) *. (y-.deby)/.(finy-.deby)) in
(a,b);;

let coord_of_pixel (tailx, taily) (debx, finx) (deby, finy) (x, y) =
 (* if not (x <= tailx && x >= 0 && y <= taily && y >= 0) 
  then failwith "inutile"*)
  let coox = debx +. (flo (x) /. flo (tailx)) *. (finx -. debx)
    and cooy = deby +. (flo (y) /. flo (taily)) *. (finy -. deby)
    in (coox, cooy);;


let visu afficher i j dt =
  let taille = (j - i + 1) in
    afficher (i);
    let tailx = size_x() and taily = size_y() in
      let a = ref (get_image 0 0 tailx taily) in
        let v = make_vect taille !a in
          for k = i + 1 to j do afficher k; a := get_image 0 0 tailx taily;
            v.(k - i) <- !a
          done;
          for b = 0 to taille - 1 do draw_image v.(b) 0 0;
            wait dt done;;


let zoom k (*k<.1. alors zoom...*) (tailx, taily) (infx, supx)
(infy, supy) (x, y) =
  let (x2, y2) = coord_of_pixel (tailx, taily) (infx, supx) (infy, supy) (x, y) in
    let infx2 = x2 +. (k *. (infx -. x2)) and
    supx2 = x2 +. (k *. (supx -. x2)) and
    infy2 = y2 +. (k *. (infy -. y2)) and
    supy2 = y2 +. (k *. (supy -. y2)) in
      (infx2, supx2), (infy2, supy2);;


let dessineraxes (tailx, taily) (infx, supx) (infy, supy) (*(unx,uny)*) =
  let a = intof (flo (tailx) *. (-. infx /. (supx -. infx))) and
  b = intof (flo (taily) *. (-. infy /. (supy -. infy))) in
    if a < tailx && a >= 0 then
      segment (a, 0) (a, tailx - 1);
    if b < taily && b >= 0 then
      segment (0, b) (taily - 1, b);;

let mod2 a b = if b = - 1 then (a - 1)
  else if a = b then 0 else b;;
let mod3 (a,b) (c,d) = (mod2 a c,mod2 b d);;

let rec appartient a ens =
  match ens with
    | [] -> false
    | x :: y -> (x = a) || appartient a y;;


let frontiere (a, b) (c, d) =
  let coucoucou a c =
    match c with 0 -> - 1
      | k when k = a -> 1
      | _ -> 0 in
    (coucoucou a c, coucoucou b d);;

