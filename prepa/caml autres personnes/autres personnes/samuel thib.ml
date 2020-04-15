(*
  Copyright (C) 2000-2001 Samuel Thibault <samuel.thibault@ens-lyon.org>

  This program is free software : you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation ; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY ; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with the program ; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  This License should be available in the same directory as this program,
  in a file called COPYING. If not, you may ask the webmaster or ftpmaster
*)

#open "graphics";;

let puis2 n=1 lsl n;;

let rec lg=function
  1->0
 |n->1+lg (n / 2);;

let hadamard f=
  let g=copy_vect f in
  let m=lg (vect_length g) in
  for k=1 to m do
    for s=0 to puis2(m-k) - 1 do
      let lbloc=puis2(k-1) in 
      let debut=2*lbloc*s in
      for j=debut to debut + lbloc - 1 do
        let aux=g.(j) in
        g.(j)<- aux + g.(j+lbloc);
        g.(j+lbloc)<-aux - g.(j+lbloc)
      done
    done
  done;
  g;;

let hadamard2 f=
  let g=copy_vect f in
  let rec hadam j n=
    for k=0 to n-1 do
      let aux=g.(j+k) in
      g.(j+ k )<-aux + g.(j+k+n);
      g.(j+k+n)<-aux - g.(j+k+n)
    done;
    if n>=2 then (
      hadam j (n/2);
      hadam (j+n) (n/2)
    ) 
in
  hadam 0 (vect_length f / 2);
  g;;

let psi u=
  let m=vect_length u-1 in
  let res=make_vect (puis2 m) u.(0) in
  for i=1 to m do
    let puis2_i_1=puis2 (i-1) in
    for j=0 to puis2_i_1-1 do
      res.(j+puis2_i_1)<-(res.(j)+u.(i)) mod 2
    done
  done;
  res;;

let code=psi;;

exception Indéterminé;;

let F_of_f f=
  map_vect (function n-> -2*n+1) f;;

let cherche_max t=
  let n=vect_length t and k=ref 0 and dup=ref false in
  for j=1 to n-1 do
    if abs(t.(j))=abs(t.(!k)) then dup:=true else
    if abs(t.(j))>abs(t.(!k)) then (k:=j;dup:=false)
  done;
  if !dup then raise Indéterminé else !k;;

let vect_of_int2 i m=
  let res=make_vect (m+1) 0 and j=ref i and k=ref 1 in
  while !j>0 do
    res.(!k)<- !j mod 2;
    j:=!j / 2;
    incr k
  done;
  res;;

let decode f=
  let F=F_of_f f in
  let h2mF=hadamard F in
  let i=cherche_max h2mF
  and m=lg (vect_length h2mF) in
  let u=vect_of_int2 i m in
  if h2mF.(i)<0 then u.(0)<-1;
  u;;

let bruit f n=
  let g=copy_vect f
  and m=vect_length f in
  let errs=make_vect m false
  and n=min n m in
  let i=ref 1 in
  while (!i<=n) do
    let j=random__int m in
    if not errs.(j) then
    (
      g.(j)<-1-g.(j);
      errs.(j)<-true;
      incr i
    )
  done;
  g;;

let rayure f n=
  let g=copy_vect f
  and m=vect_length f in
  let j=random__int (m-n) in
  for i=1 to n do
    g.(i+j)<-1-g.(i+j)
  done;
  g;;

decode (bruit (code [|1;0;1;0;0;1|]) 4);;

exception Err of int;;
exception Indét of int;;

let u=[|1;0;1;0;0;1;0;1|];;
let m=1 lsl (vect_length u - 1);;
let a=code u;;

try
for j=0 to 100000 do
let b=bruit a j in
try if u <> decode b then raise (Err j)
with Indéterminé -> raise (Err j)
    | f-> raise f
done
with Err j -> print_float (100. *. float_of_int j /. float_of_int m);;

let vect_of_int i m=
  let res=make_vect m 0 and j=ref i and k=ref 0 in
  while !j>0 do
    res.(!k)<- !j mod 2;
    j:=!j / 2;
    incr k
  done;
  res;;

let int_of_vect u=
  let m=vect_length u in
  let j=ref u.(m-1) in
  for i=m-2 downto 0 do
    j:=2 * !j + u.(i)
  done;
  !j;;



let matrix_of_bmp txt=
	let a=open_in_bin txt and
	l=ref 0 and c=ref 0 in
	for i=1 to 19 do c:=input_byte a done;
	c:=!c+(input_byte a)*256;
	for i=1 to 3 do l:= input_byte a done;
	l:=!l+(input_byte a)*256;
	let m= (make_matrix !l !c 0) in
	for i=1 to 30 do input_byte a; () done;
	for i=0 to !l-1 do
	for j=0 to !c-1 do
		let c=(rgb (input_byte a) (input_byte a) (input_byte a)) in
		m.(!l-i-1).(j)<- c
		done;
	for i=1 to !c mod 4 do input_byte a; () done;
	done; close_in a;m;;

print_string "ouverture...";;
let a=matrix_of_bmp "mars.bmp";;
let xx=vect_length a.(0) and yy=vect_length a;;

let nbbits=16;;



(*
moveto 0 0;;
for i= 0 to 5 do
 lineto (xx*i/12) (yy-yy*i/12);
 lineto (xx-xx*i/12) (yy-yy*i/12);
 lineto (xx-xx*i/12) (yy*i/12);
 lineto (xx*(i+1)/12) (yy*i/12);
done;;
*)

print_string "aplanissage...";;
for i=0 to xx-1 do
  for j=0 to yy-1 do
    let col=(a.(j).(i) mod 256 + a.(j).(i) / 256 mod 256 + a.(j).(i) / 65536 mod 256) / 3 in
    a.(j).(i)<-rgb col col col
  done
done;;






let code_color ( n : color )=
  let trans n=
    let u=code (vect_of_int n 8)
    and res=make_vect 5 0 in
    for i=0 to 4 do
      res.(i)<-int_of_vect (sub_vect u (24*i) 24)
    done;
    res in
  let i=n mod 256 and j= (n / 256) mod 256 and k= (n / 65536) mod 256 in
    concat_vect (trans i) (concat_vect (trans j) (trans k));;

let decode_color v i=
try
  let detrans v j=
    let aux=make_vect 128 0 in
    for i=0 to 4 do
      blit_vect (vect_of_int v.(i+j) 24) 0 aux (24*i) 24
    done;
    int_of_vect (decode aux)
  in (detrans v i + 256 * detrans v (i+5) + 65536 * detrans v (i+10) : color)
with Indéterminé -> black;;

let code_mono ( n : color )=
  let u=code (vect_of_int (n mod 256 / 4)  6)
  and res=make_vect 2 0 in
  for i=0 to 1 do
    res.(i)<-int_of_vect (sub_vect u (nbbits*i) nbbits)
  done;
  res;;

let decode_mono v i=
try
  let aux=make_vect (2*nbbits) 0 in
  for j=0 to 1 do
    blit_vect (vect_of_int v.(i+j) nbbits) 0 aux (nbbits*j) nbbits
  done;
  let col=4 * int_of_vect (decode aux) in 
    rgb col col col
with Indéterminé -> black;;

(*let a=dump_image (get_image 0 0 xx yy);;*)
(*print_string "encodage...";;
let b=map_vect (fun v->
it_list concat_vect [||] (map_vect_list code_mono v)*)
(*it_list concat_vect [||] (map_vect_list code_color v)*)
(*) a;;*)

let bruitimage n v=
  let w=copy_vect v
  and m=vect_length v in
  for i=1 to (m*nbbits*n/100) do
    let j=random__int m
    and k=random__int nbbits in
    w.(j)<-w.(j) lxor (1 lsl k)
  done;
  w;;

let bruitimage64 n v=
  let w=copy_vect v
  and m=vect_length v in
  for i=1 to (m*6*n/100) do
    let j=random__int m
    and k=random__int 6 in
    let aux=4 * ((w.(j) mod 256 / 4) lxor (1 lsl k)) in
    w.(j)<-rgb aux aux aux
  done;
  w;;
print_string "brouillage...";;
let c=map_vect (bruitimage64 10) a;;
(*
let c=map_vect (bruitimage 10) b;;
*)

(*while not key_pressed () do done;
read_key ();;*)

(*let h=get_image 0 yy (xx*15) yy;;

let c=dump_image h;;*)
(*
print_string "décodage...";;
let d=map_vect (fun v->
let m=vect_length v / 2 in*)
(*let m=vect_length v / 15 in*)
(*let res=make_vect m 0 in
for i= 0 to m-1 do
res.(i)<- decode_mono v (2*i)*)
(*res.(i)<- decode_color v (15*i)*)
(*done;
res
)c;;*)


(*
try
for j=0 to 100000 do
let b=rayure a j in
try if u <> decode b then raise (Err j)
with Indéterminé -> raise (Indét j)
    | f-> raise f
done
with Err j -> print_float (100. *. float_of_int j /. float_of_int m)
  |Indét j -> print_string "dét: ";
              print_float (100. *. float_of_int j /. float_of_int m);;
*)

(*
let u=[|1;0;1;1;1;1;0;1|] in
for j=0 to 55 do
for i=1 to 20 do
  let l=code u in
  try if u<>(decode (bruit l j)) then printf__printf "err: %d\n" j
  with Indéterminé -> printf__printf "détecté: %d\n" j;
done;
done;;
*)

(*
let u=[|1;0;1;1;1;1;0;1|] in
for j=0 to 10000 do
  if u<>(decode (bruit (code u) 32)) then raise Indéterminé
done;;
*)

(*close_graph ();;*)
(*open_graph (":0.0 " ^ (string_of_int (xx*2)) ^ "x" ^ (string_of_int (yy*3)) ^ "+0+0" );;*)
(*open_graph (":0.0 " ^ (string_of_int (xx*15)) ^ "x" ^ (string_of_int (yy*4)));;*)

(*print_string "affichage...";;
draw_image (make_image a) 0 0;;
draw_image (make_image b) 0 yy;;
draw_image (make_image c) 0 (2*yy);;
draw_image (make_image d) xx 0;;*)
