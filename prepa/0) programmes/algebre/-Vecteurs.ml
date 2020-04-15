(*Nicolas Klarsfeld*)
(*ALGEBRE*)
(*-----------------------*)
(*vecteurs*)
let flo = float_of_int and int=int_of_float;;
exception trouvé;;

let vel= vect_length;;

let petit v =
  let n = vect_length v in if n <> 0 then begin
        let k = ref v.(0) in
          for i = 1 to n - 1 do
            if v.(i) <. (!k) then k := v.(i) done; !k end
    else 0.;;

let gros v =
  let n = vect_length v in if n <> 0 then begin
        let k = ref v.(0) in
          for i = 1 to n - 1 do
            if v.(i) >. (!k) then k := v.(i) done; !k end
    else 0.;;

let rec prefix $+ v1 v2 =
  let l1 = vel v1 and l2 = vel v2 in
    if l1 < l2 then v2 $+ v1 else
      let w = (copy_vect v1) in
        for i = 0 to l2 - 1 do
          w.(i) <- v1.(i) +. v2.(i) done;
        w;;

let moins_v v = map_vect (fun x -> -.x) v;;

let moins_m m = map_vect (moins_v) m;;

let prefix $& a v = map_vect (fun x -> a*.x) v;;

let prefix $$ a m = map_vect (fun v->a$&v) m;;
let prefix $- v1 v2 = v1 $+ (moins_v v2);;

let prefix $. v1 v2 = let l = min (vel v1) (vel v2) in
let s = ref 0. in
for i = 0 to l-1 do
s:= !s +. v1.(i)*.v2.(i) done; !s;;

(* ! ! mettre : prod vect ! ! *)

(*---------------------*)
(*matrices*)

let Id n = let M =
    make_matrix n n 0. in
    for i = 0 to (n - 1) do M.(i).(i) <- 1. done;
    M;;

let ligne m = vect_length m;;

let colonne m = match m with
    | x when x = [||] -> 0
    | _ -> vect_length (m.(0));;

let vérifier mat =
  let l = ligne mat and c = colonne mat in
    try for i = 0 to l - 1 do
        if vel mat.(i) <> c then raise trouvé done; true;
    with | _ -> false;;

let vérifier_carré mat =
  let l = ligne mat and c = colonne mat in
    try if c <> l then raise trouvé
      else if not (vérifier mat) then raise trouvé; true;
    with | _ -> false;;


let tran m =
  let c = colonne m and l = ligne m in
    let n = make_matrix c l 0. in
      for j = 0 to c - 1 do
        for i = 0 to l - 1 do
          n.(j).(i) <- m.(i).(j) done; done; n;;

let trace m = if not (vérifier_carré m) then failwith "pas carré"
  else let k = ref 0. in for i = 0 to vect_length m do
        k := (!k) +. m.(i).(i) done; !k;;


let prefix $* a b =
  let c1 = colonne a
  and l2 = ligne b in
    if c1 <> l2 then failwith "pas bon" else
      begin
        let c2 = colonne b
        and l1 = ligne a in
          let M = make_matrix l1 c2 0. in
            for i = 0 to l1 - 1 do
              for j = 0 to c2 - 1 do
                for h = 0 to c1 - 1 do
                  M.(i).(j) <- M.(i).(j) +. (a.(i).(h) *. b.(h).(j))
                done;
              done;
            done;
            M;
      end;;

let comat m a b (*mat ac lign et col en -*)=
  if vérifier m = false then failwith "chat va pas"
  else let l = ligne m and c = colonne m in
      if (a >= l or b >= c) then failwith "mec, t'es ouf ou koi ?!"
      else let n = make_matrix (l - 1) (c - 1) 0. in
          for i = 0 to l - 1 do
            if i < a then begin
                for j = 0 to b - 1 do n.(i).(j) <- m.(i).(j) done;
                for j = b + 1 to c - 1 do n.(i).(j - 1) <- m.(i).(j) done; end
            else if i > a then begin
                for j = 0 to b - 1 do n.(i - 1).(j) <- m.(i).(j) done;
                for j = b + 1 to c - 1 do n.(i - 1).(j - 1) <- m.(i).(j) done; end
          done;
          n;;

let det m =
  if vérifier_carré m = false then failwith "aie" else
    let rec deter m n =
      match n with
        | 0 -> 1.
        | 1 -> m.(0).(0)
        | 2 -> (m.(0).(0) *. m.(1).(1)) -. (m.(0).(1) *. m.(1).(0))
        | _ -> let k = ref 0. and j = ref 1. in
              for i = 0 to (n - 1) do k := !k +. (!j) *. m.(i).(0) *. deter (comat m i 0) (n - 1);
                j := (-. (!j)) done; !k
    in deter m (ligne m);;

let coco m (*mat des cofact*)=
  let l = ligne m in let p = make_matrix l l 0. in
      for i = 0 to l - 1 do
        for j = 0 to l - 1 do
          if (i + j) mod 2 = 0 then
            p.(i).(j) <- det (comat m i j)
          else p.(i).(j) <- (-. det (comat m i j))
        done; done; p;;

let prod_vect_gen w = let k = vect_length w in
    if k < 1 then failwith
      "pas assez de vecteurs ds prodvect" else
      let i0 = vect_length w.(0) in
        if i0 <> k + 1 then failwith "nb vect = dimension - 1 !" else
          for i = 1 to k - 1 do
            if (vel w.(i)) <> i0 then failwith "pas tous de même taille, pas coooool"
          done;
        let p = make_vect i0 0. in
          let M = concat_vect [|p|] w and j = ref 0 and l = ref 1. in
            while !j < i0 do
              p.(!j) <- (!l) *. det (comat M 0 (!j)); j := (!j) + 1; l := (-. (!l)) done;
            p;;


let inverse m = let k = det m in
    if k = 0. then failwith "oups pas inversibeul" else
      map_vect (fun v -> map_vect (fun x -> x /. k) v) (tran(coco m));;

let k = [|[|2.;1.;0.|];[|0.;1.;0.|];[|0.;0.;1.|]|]
in inverse k;;

let prefix *** m n =
  if vérifier_carré m = false then failwith "pas carre" else
    let k = ligne m in
      let rec puissance m n =
        match n with
          | k when k < 0 -> puissance (inverse m) (-n)
          | 0 -> Id k
          | 1 -> m
          | k when k mod 2 = 0 -> let j = (puissance m (n / 2))
              in j $* j
          | k when k mod 2 = 1 -> let j = (puissance m (n / 2))
              in (j $* j) $* m
      in puissance m n;;

let eval_vect m v = ([|v|]$*m).(0);;

eval_vect [|[|2.;1.;0.|];[|0.;1.;0.|];[|0.;0.;1.|]|] [|1.;2.;0.|];;

let new_coord b v = 
if det b = 0. then failwith "problème là... c pas une base ton truc"
  else eval_vect (inverse b) v;;

new_coord [|[|2.;1.;0.|];[|0.;1.;0.|];[|0.;0.;1.|]|] [|1.;0.;0.|];;

let change_base b m (*m dans la base rep par b*)=
  if det b = 0. then failwith "problème là... c pas une base ton truc"
  else let a = (inverse b) in (b $* m $* a);;

change_base [|[|1.;1.|];[|0.;1.|]|] [|[|2.;1.|];[|0.;1.|]|] ;;


(*----------------*)
(*normes*)

let abs_float a= if a<.0. then -.a else a;;

let norme_eucl v = sqrt(v$.v);;

let norme_infinie v = if v =[||] then 0. else
let k = ref v.(0) in for i=1 to vect_length v do
k:= max (!k) v.(i) done; !k;;

let norme p v = if p<.1. then failwith "pas content" else
if v =[||] then 0. 
else let k=ref ((abs_float v.(0))**p) and n = vect_length v in
for i = 1 to n-1 do k := !k +. ((abs_float v.(i))**p) done;
(!k)**(1./.p);;

let distance norme a b = norme(a$-b);;

(*------------*)
(*dimension3*)

let prod_vect a b =
let p = make_vect 3 0. in
let M =[|p;a;b|] and j = ref 0 and l = ref 1. in
while !j<3 do
p.(!j)<-(!l)*.det(comat M 0 (!j)); j:=(!j)+1; l:=(-.(!l)) done;
p;;

let rota phi th v (*en rad*)= let a1 = cos th and a2 = sin th 
and b1 = cos phi and b2 = sin phi in
([|v|]$*[|[|1.;0.;0.|];[|0.;a1;a2|];[|0.;-.a2;a1|]|]
$*[|[|b1;0.;-.b2|];[|0.;1.;0.|];[|b2;0.;b1|]|]).(0);;

let proj v= v.(0), v.(1);;



