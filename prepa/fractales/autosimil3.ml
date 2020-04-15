include "biblio\utile\poly_complexes.ml";;


let intof=int_of_float;;
let intof2 (a,b) = (intof a, intof b);;
let flo2 (a,b) = (flo a, flo b);;
let norm2 (x, y) = (x *. x +. y *. y);;
let norm3 (x, y) = max (abs (intof x)) (abs (intof y));;


let coord (a,b) (c,d) m th = 
let e = cos th and f = sin th in
let g = m*.(c-.a) and h = m*.(d-.b) in
let t = (e*.g -.f*.h,e*.h+.f*.g) in
t;;

let anti_coord (a, b) (c, d) (t1, t2) =
  let u = (c -. a, d -. b) and v = (t1 -. a, t2 -. b) in
    let h = norm u and i = norm v in
      let m = i /. h and
      th = asin ((antiscal u v) /. (h *. i))
      in if scal u v >= 0. then
          (m, th) else (m, pi-.th);;

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

(**)

let autosimil avant condstop final pendant trans param0 =
  avant();
  let rec autosimil_rec param =
    if condstop param then final param (*un param-> unit*)
    else begin pendant param; (*un param-> unit*)
        trans autosimil_rec param end; (*on fait quoi*) in
    autosimil_rec param0;;

type type_param1 ={a1:float*float;a2:float*float;a3:int};;
let param1_0 ={a1=5.,5.;a2=500.,5.;a3=7};;
let param1_1 = {a1=(300.,0.);a2=(300.,200.);a3=7};;
let param1_2 = {a1 =(300.,400.) ;a2 =(700.,400.);a3=7};;

let avant1() = open2 (600,600);clear_graph();;

let condstop1_1 mincar (m:type_param1)= 
let (a,b) = m.a1 and (c,d) = m.a2 in
norm2 (c -. a, d -. b) <. mincar;;
let condstop1_2 (m:type_param1)= m.a3 = 0;;
let condstop1_3 mincar m = (condstop1_1 mincar m) || condstop1_2 m;;
let condstop1_4 min m = let (a,b) = m.a1 and (c,d) = m.a2 in
norm3 (c -. a, d -. b) < min;;
let condstop1_5 min m = (condstop1_4 min m) || condstop1_2 m;;


let final1 (m:type_param1) = 
let (a,b) = m.a1 and (c,d) = m.a2 in
      moveto (intof a) (intof b);
      lineto (intof c) (intof d);;

let pendant1 opt1 (m: type_param1) =
    if opt1 then final1 m;;

let trans2 h t v opt2 autosim m =
  let (a, b) = m.a1 and (c, d) = m.a2 and n = m.a3 in
    let transfcoord = opt2 (a, b) (c, d) in
      for i = 0 to h - 1 do
        let k = t.(i) and
        w = map_vect (fun s -> add (a, b) (transfcoord (fst s) (snd s))) v.(i) in
          for j = 0 to k - 2 do
            autosim {a1 = w.(j); a2 = w.(j + 1); a3 = n - 1}
          done; done;;

autosimil avant1 condstop1_2 final1 (pendant1 false) 
(trans2  1 [|5|] [|[|0.,0.;0.333333333333, 0.0; 0.5, 0.288675134595; 0.666666666667, 0.0; 1.0, 0.0|]|]
coord2)
param1_0;;

let prepare v =
  let p = vect_length v in
    let v2 = make_vect p 0 in
      for i = 0 to p - 1 do v2.(i) <- vect_length v.(i) done;
      p, v2;;

let prepare_arbre v = let n = vect_length v in
(n,
make_vect n 2,
(map_vect
(fun (m,th)->[|(1.,0.);(1. +. m *. cos (th), m *. sin (th))|]) v));;

let arbre v param0 n = if v <> [||] then
    let (a, b, c) = prepare_arbre v and
    param = {a1 = param0.a1; a2 = param0.a2; a3 = n} in
      autosimil avant1 (condstop1_4 8) final1 (pendant1 true)
      (trans2 a b c coord2)
      param;;

let prepare_koch v =
([|vect_length v + 2|],[|concat_vect [|(0.,0.)|] (concat_vect v [|(1.,0.)|])|]);;

let koch v param0 n = if v <> [||] then
    let a, b = prepare_koch v and
    param = {a1 = param0.a1; a2 = param0.a2; a3 = n} in
      autosimil avant1 (condstop1_4 8) (*cond_stop1_2*) final1 (pendant1 false)
      (trans2 1 a b coord2)
      param;;

let app_presque u a v =(*renvoie le premier rg proche ou -1 sinon*)
  let n=vect_length v in if n = 0 then -1 else
    let c = ref (-1) and k = ref 0 in while !c=(-1) && !k<n do
        if (dist4 (v.(!k)) a) <= u then c:=(!k) else k := (!k) + 1 done; !c;;

let enleve v t = if t <> (- 1) then begin
      let n = vect_length v in
        let w = make_vect (n - 1) v.(0) in
          for i = 0 to t - 1 do w.(i) <- v.(i) done;
          for j = t+1 to n-1 do w.(j - 1) <- v.(j) done;
          w end
  else v;;

let interactif option n =
  let f = (option = "arbre") in
    let param0 = ref param1_1 in
      if not f then param0 := param1_2;
      open_graph "1000x700"; clear_graph();
      let p = !param0.a1 and q = !param0.a2 in
        moveto (fst (intof2 p)) (snd (intof2 p));
        lineto (fst (intof2 q)) (snd (intof2 q));
        let points = ref [||] in let points2 = ref [||] (*entiers*)
          and point = ref (0., 0.) and c = ref true and r = ref (- 1) in
            while (!c) do
              let e = wait_next_event [Button_down; Key_pressed] in
                if e.button then begin let k = mouse_pos() in r := app_presque 2 k !points2;
                      if f then begin point := anti_coord p q (flo2 k ^+ (p ^- q)) end
                      else begin point := anti_coord2 p q (flo2 k) end;
                      if !r = - 1 then begin
                          points2 := concat_vect [|k|] !points2;
                          points := concat_vect [|!point|] !points
                        end
                      else begin while button_down() do
                            let g = mouse_pos() in begin
                                if f then begin point := anti_coord p q (flo2 g ^+ (p ^- q)) end
                                else begin point := anti_coord2 p q (flo2 g) end;
                                !points2.(!r) <- g;
                                !points.(!r) <- !point;
                                if f then arbre !points !param0 n
                                else koch !points !param0 n;
                              end done;
                        end; end
                else if e.key = `z` then begin points := enleve !points !r;
                    points2 := enleve !points2 !r;
                  end
                else if e.key = ` ` then c := false;
                if f then arbre !points !param0 n else koch !points !param0 n;
                set_color red;
                for i = 0 to (vect_length !points2) - 1 do let z = !points2.(i)
                  in fill_rect (fst z - 2) (snd z - 2) 5 5; done;
                set_color black;
            done;
            points;;

interactif "arbre" 9;;

(*-------------------*)

let autosimil avant condstop final pendant trans param0 =
  avant();
  let rec autosimil_rec param =
    if condstop param then final param (*un param-> unit*)
    else begin pendant param; (*un param-> unit*)
        trans autosimil_rec param end; (*on fait quoi*) in
    autosimil_rec param0;;

type type_param2 = {a1: (float * float) vect; a2: (float * float) vect; a3: int};;
let avant2 = avant1;;
let condstop2_0 n (m:type_param2)= m.a3 = n;;
let final2 (m: type_param2) = ();;

let pendant2 opt (m: type_param2) =draw_poly m.a1(*preciser coordonnees*);;

let trans3 f autosim m =
autosim (f m);;

(*mettre de l'aléatoire dans la propagation
faire volumes
faire poly lagrange recur
régler pb angle*)











