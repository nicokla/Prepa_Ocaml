(*Aléatoire*)
#open "random";;

let piocher u v = u + random__int (v-u+1);;

let random_vect a b n =
  let v = make_vect n 0 in
    for i = 0 to n - 1 do
      v.(i) <- piocher a b done; v;;

let random_list a b n =
  let k = ref [] in
    for i = 1 to n do
      k := (piocher a b) :: (!k) done; 
!k;;

let hazard r = 
let k = random__int (2*r) in
if k<r then k-r else k-r+1;;
