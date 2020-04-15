(*let compoconnexes1 v =
  let n = vect_length v in
    let m = mat_of_adj v in
      let m4 = ref m and b = ref true in
        let m6 = ref m in
          let multmat m1 m2 =
            let m3 = make_matrix n n 0 in
              for i = 0 to n - 1 do
                for j = 0 to n - 1 do
                  for k = 0 to n - 1 do
                    m3.(i).(j) <- (m1.(i).(k) * m2.(k).(j)) + m3.(i).(j)
                  done; done; done; m3
          in let add m1 m2 =
              let m3 = make_matrix n n 0 in
                for i = 0 to n - 1 do
                  for j = 0 to n - 1 do
                    m3.(i).(j) <- (m1.(i).(j) + m2.(i).(j)) done; done; m3
            in while !b do
                let m5 = (multmat !m6 m) in
                  if !m6 = m5 then b := false else
                    m6 := m5;
                    m4 := add (!m4) (m5) done; !m4;;

compoconnexes1 [|[1]; [0]; []|];;*)


let listadj v =
  let n = vect_length v in
let v2=make_vect n [] in
    for i = 0 to n - 1 do let a, b = v.(i) in
        v2.(a) <- b :: v2.(a) done;
    v2;;

let coadj v =
  let n = vect_length v in
    let v2 = make_vect n [] in
      for i = 0 to n - 1 do let l = v.(i) in
          let rec aux l2 = match l2 with [] -> ()
                  |(a, b) :: c -> v2.(b) <- (a :: (v2.(b)));
                  aux c in aux l done; v2;;

let mat_of_adj v =
  let n = vect_length v in
    let m = make_matrix n n false in
      for i = 0 to n - 1 do
        let rec aux l =
          match l with [] -> ()
            | b :: c -> m.(i).(b) <- true;
                aux c in
          aux v.(i) done; m;;

let compoco v =
  let n = vect_length v in
    let m = make_vect n (- 1) in
      let c = ref 0 in
        let rec explore k =
          if (m.(k)) = - 1 then
            begin
              m.(k) <- !c;
              let l = ref v.(k) in
                while !l <> [] do explore (hd (!l)); l := tl (!l); done; end;
        in for i = 0 to (n - 1) do
            if m.(i) = (- 1) then begin explore i; c := (!c) + 1 end;
          done; m;;

let compobis a = let n = vect_length a in
    let m = make_vect n false in
      let c = ref [] and res = ref [] in
        let rec explore k =
          if not (m.(k)) then
            begin
              m.(k) <- true;
              c := k :: (!c);
              let l = ref a.(k) in
                while !l <> [] do explore (hd (!l)); l := tl (!l); done; end;
        in for i = 0 to (n - 1) do
            if not m.(i) then begin explore i; res := (!c) :: (!res); c := []; end;
          done;
          !res;;

compoco [|[1]; [0]; []|];;

let warshall v =
let n = vect_length v in 
let m = mat_of_adj v in
for i = 0 to (n-1) do m.(i).(i)<- true done;
for k = 0 to (n-1) do
for i = 0 to (n-1) do
for j = 0 to (n-1) do
m.(i).(j)<- m.(i).(j) || (m.(i).(k) && m.(k).(j));
done;done;done; m;;

let warshall2 v =


