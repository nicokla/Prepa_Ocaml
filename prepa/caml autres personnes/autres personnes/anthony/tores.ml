let trirapide t =
let n = vect_length t in
let s = make_vect n 0 in 
let rec tri g d =
let i = ref g and j = ref d and x = ref (t.(s.((g+d)/2))) in
while i <= j do
  while t.(s.(!i)) <. !x && !i < d do i := !i + 1 done;
  while !x <. t.(s.(!j)) && !j > g do j := !j - 1 done;
  if !i <= !j then begin
    let temp = s.(!i) in (s.(!i) <- s.(!j); s.(!j) <-temp);
    i := !i + 1;
    if !j > 0 then j := !j - 1
  end
done;
if g < !j then tri g (!j);
if !i < d then tri (!i) d;
in
for k = 0 to n - 1 do s.(k) <- k done;
tri 0 (n - 1);
s
;;

let orx =150.0;;
let ory = 180.0;;
let ux =30.0;;
let uy =30.0;;

let perspective x y z =
(int_of_float (orx +. (y -. 0.5 *. x) *. ux) , int_of_float (ory +. (z -. 0.5 *. x) *. uy));; 

let surface f g h ui us vi vs nu nv =
let index = ref 0 and tabrect = make_matrix (nu * nv) 4 (0 , 0)
and tabdist = make_vect (nu*nv) 0.
and du = (us -. ui) /. float_of_int nu
and dv = (vs -. vi) /. float_of_int nv
and u = ref 0. and v = ref 0.
and x = ref 0. and y = ref 0. in
u := ui;
for i = 0 to nu - 1 do
  v := vi;
  for j = 0 to nv - 1 do
    x := !u +. du /. 2.;
    y := !v +. dv /. 2.;
    tabdist.(!index) <- f !x !y +. 0.5 *. (g !x !y +. h !x !y);
    tabrect.(!index).(0) <- perspective (f !u !v) (g !u !v) (h !u !v);
    u := !u +. du;
    tabrect.(!index).(1) <- perspective (f !u !v) (g !u !v) (h !u !v);
    v := !v +. dv;
    tabrect.(!index).(2) <- perspective (f !u !v) (g !u !v) (h !u !v);
    u := !u -. du;
    tabrect.(!index).(3) <- perspective (f !u !v) (g !u !v) (h !u !v);
    index := !index + 1;
  done;
  u := !u +. du;
done;
(tabrect , tabdist)
;;

#open "graphics";;

let drawPoly p =
let (x,y) = p.(0) in moveto x y;
for i = 1 to (vect_length p) - 1 do
	let (x,y) = p.(i) in lineto x y
done;
let (x,y) = p.(0) in lineto x y
;;

let dessine fillcolor linecolor tabrect tabdist =
set_line_width 0;
let perm = trirapide tabdist in
for i = 0 to vect_length tabdist -1 do
  set_color fillcolor;
  fill_poly (tabrect.(perm.(i)));
  set_color linecolor;
  drawPoly (tabrect.(perm.(i)));
done;;

let tores() =
let f u v = (3. +. cos(u)) *. cos(v)
and g u v = (3. +. cos(u)) *. sin(v)
and h u v = sin(u) in
let f1 u v = 3. +. f u v  
and pi = 4. *. atan 1. in
let (tabrect1 , tabdist1) = surface f g h (-.pi) pi (-.pi) pi 30 30
and (tabrect2 , tabdist2) = surface f1 h g (-.pi) pi (-.pi) pi 30 30 in
let n1 = vect_length tabdist1 in
let n = n1 + vect_length tabdist2 in 
let tr = make_matrix n 4 (0 , 0) and td = make_vect n 0. in
for i = 0 to n1 - 1 do
  tr.(i) <- tabrect1.(i);
  td.(i) <- tabdist1.(i);
done;
for i = n1 to n - 1 do
  tr.(i) <- tabrect2.(i - n1);
  td.(i) <- tabdist2.(i - n1);
done;
dessine blue yellow tr td;;

open_graph " 300x300";;
set_window_title "tores";;
tores();;
