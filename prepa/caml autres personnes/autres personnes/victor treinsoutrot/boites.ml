let eval s c =
let ((n1,f1),(n2,f2))=s in
 let i1_1=c n1 and i2_1 = c n2 in
let i1_2 = c (f1 i1_1) and i2_2 = c (f2 i2_1) in
(i1_2=1)&&(i2_2=2);;

let eval2 s c =
let ((n1,f1),(n2,f2))=s in
 let i1_1=c n1 and i2_1 = c n2 in
let i1_2 = c (f1 i1_1) and i2_2 = c (f2 i2_1) in
((i1_2=1)&&(i2_2=2),c 1,c 2,c 3, c 4, n1,i1_1,f1 i1_1,i1_2,n2,i2_1,f2 i2_1,i2_2);;

let f n1 n2 n3 n4 n =
match n with
|1->n1
|2->n2
|3->n3
|4->n4
|_->(print_int n1 ; print_int n2 ; print_int n3 ; print_int n4;print_int n;failwith "pb") ;;
let id = f 1 2 3 4;;
let allc = let v = make_vect 24 id in
  for i = 0 to 3 do
    for j = 0 to 2 do
        let n1 = i in
        let n2 = (i + j + 1) mod 4 in
        let n3 = if ((n2 + 1) mod 4) = n1 then (n1 + 1) mod 4
          else ((n2 + 1) mod 4) in
        let n4 = (6 - (n1 + n2 + n3) ) in
        v.(i * 6 + j * 2 + 0) <- f (n1 + 1) (n2 + 1) (n3 + 1) (n4 + 1);
        v.(i * 6 + j * 2 + 1) <- f (n1 + 1) (n2 + 1) (n4 + 1) (n3 + 1);
print_int n1;
print_int n2;
print_int n3;
print_int n4;
print_endline "";
    done;
  done;
  v;;
let allf = make_vect (131072) ((1,id),(2,id));;


for n1 = 1 to 1 do
  for a1 = 1 to 4 do
    for b1 = 1 to 4 do
      for c1 = 1 to 4 do
        for d1 = 1 to 4 do
          for n2 = 1 to 2 do
            for a2 = 1 to 4 do
              for b2 = 1 to 4 do
                for c2 = 1 to 4 do
                  for d2 = 1 to 4 do
                    allf.(a1 + b1 * 4 + c1 * 16 + d1 * 64 + n2 * 256 + a2 * 512 + b2 * 2048 + c2 * 8192 + d2 * 32768 - 43861)
                    <- ((n1, f a1 b1 c1 d1), (n2, f a2 b2 c2 d2));
                  done;
                done;
              done;
            done;
          done;
        done;
      done;
    done;
  done;
done;;



let mel =
  let pmax = ref 0 and p = ref 0 and smax = ref 0 and nbe = ref 0 and vd = make_vect 3 0 in
  for i = 0 to 131071 do
    p := 0;
    for j = 0 to 23 do
      if eval (allf.(i)) (allc.(j)) then
        incr (p)
      else ();
    done;
    if (!p) > (!pmax) then (pmax := !p; smax := i)
    else if (!p) = 10 then (vd.(!nbe) <- i;incr nbe) else ();
  done;
  (!pmax, !smax, !nbe,vd)
;;

let melprecis s=
let ((d1,f1),(d2,f2))=s in
let (f11,f12,f13,f14,f21,f22,f23,f24) =(f1 1,f1 2,f1 3,f1 4,f2 1,f2 2,f2 3,f2 4) in
print_int f11;
print_string " ";
print_int f12;
print_string " ";
print_int f13;
print_string " ";
print_int f14;
print_string " ";
print_int f21;
print_string " ";
print_int f22;
print_string " ";
print_int f23;
print_string " ";
print_int f24;
print_endline " ";
let p=ref 0 in
    for j = 0 to 23 do
let (b, n1,n2,n3,n4,e1,r1,e2,r2,e3,r3,e4,r4)=eval2 s (allc.(j)) in
      if b then
        incr (p)
      else ();
print_int n1;
print_string " ";
print_int n2;
print_string " ";
print_int n3;
print_string " ";
print_int n4;
print_string " ";
print_int e1;
print_string " ";
print_int r1;
print_string " ";
print_int e2;
print_string " ";
print_int r2;
print_string " - ";
print_int e3;
print_string " ";
print_int r3;
print_string " ";
print_int e4;
print_string " ";
print_int r4;
print_endline "";

    done;
(!p);;

melprecis (allf.(117220));;


