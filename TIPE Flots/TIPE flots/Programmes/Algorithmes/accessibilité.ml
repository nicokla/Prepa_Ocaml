
(*---------------------------------------------------*)
(*Algorithmes d'accessibilité,
 bout signifie 'jusqu'au bout' (on ne s'arrête pas à b)*)


let chemin v b = (*renvoie [a...b] si possible, [b] sinon*)
  let rec aux l =
    match l with
      | x :: y -> if v.(x) = (- 2) || v.(x) = (-1) then l
            (*<- y si on veut mettre [] au lieu de b et [a+1;...;b] sinon*)
          else aux (v.(x) :: l) | _ -> [] in aux [b];;

let chemin2 v h b = (*renvoie [a...b], puis la cap min, si possible, [b] et (ent 0) sinon*)
(*utiliser uniquement pour Bellman*)
  let rec aux l k=
    match l,k with
      | (x :: y),_ -> if v.(x) = (- 2) || v.(x) = (-1) then l,k
            (*<- y,k si on veut mettre [] au lieu de [b] et [a+1;...;b] sinon*)
          else aux (v.(x) :: l) (mini k h.(x)) | _ -> [], (ent 0) in aux [b] (h.(b));;

let PP_bout g n a =
  let v = make_vect n (- 2) and h = make_vect n omega in
v.(a)<-(-1);
    let p = ref [a] in
      while !p <> [] do let t = hd !p in p := tl !p;
          let l = ref g.(t) in
            while !l <> [] do
              let k = fst (hd !l) and r = (snd (hd !l)).(0) in l := tl !l;
                  if v.(k) = (- 2) then
                  begin v.(k) <- t; h.(k) <- mini h.(t) r;
                    p := k :: !p; end;
            done; done; v,h;;

let PP2 g n a b = let (v,h) = PP_bout g n a in (chemin v b, h.(b));;

let PL_bout g n a =
  let v = make_vect n (- 2) and h = make_vect n omega in
    v.(a) <- (- 1);
    let f = creer_file() in
      enfiler f a;
      while not est_vide_f f do
        let t = defiler f in
          let l = ref g.(t) in
            while !l <> [] do
              let k = fst (hd !l) and r = (snd (hd !l)).(0) in l := tl !l;
                if v.(k) = (- 2) then begin v.(k) <- t; h.(k) <- mini h.(t) r; enfiler f k end
            done done;v,h;;

let PL2 g n a b = let v,h=PL_bout g n a in (chemin v b, h.(b));;

(*---------------------------------*)

let djikstra_bout g n a =
  let v = make_vect n (- 2) and G = copy_vect g and
  w = make_vect n omega and v2 = make_vect n false and
  h = make_vect n omega and minimum = ref (ent 0) and k = ref 1 and
  t = ref (- 1) in w.(a) <- ent 0; v.(a) <- (- 1); v2.(a) <- true;
    
    while G.(a) <> [] do let c, d = hd G.(a) in G.(a) <- tl G.(a);
        w.(c) <- d.(1); h.(c) <- d.(0);v.(c)<-a done;
    
    while !minimum <> omega && !k < n do
      minimum := omega;
      t := (- 1);
      for i = 0 to n - 1 do
        if v2.(i) = false && w.(i) <& !minimum then begin
            t := i; minimum := w.(i) end
      done;
      
      if !t <> (- 1) then begin
          v2.(!t) <- true;
          while G.(!t) <> [] do let x, y = hd G.(!t) in G.(!t) <- tl G.(!t);
              if not v2.(x) then begin
                  if (y.(1) +& w.(!t)) <& w.(x) then begin
                      v.(x) <- !t;
                      w.(x) <- (y.(1) +& w.(!t));
                      h.(x) <- mini h.(x) h.(!t) end
              end;
          done;
        end;
        k := !k + 1;
    done;
    v, w, h;;

let djikstra2 g n a b =
let v,w,h = djikstra_bout g n a in (chemin v b, h.(b),w.(b));;

(*---------------------------------------------*)
(*pas jusqu'au bout, on s'arrête à b:*)


let PP g n a b (*version itérative avec pile*) = (*a<>b*)
  let v = make_vect n (- 2) and h = make_vect n omega in
    v.(a) <- (- 1);
    let p = ref [a] in
      while !p <> [] do let t = hd !p in p := tl !p;
          (*qui n'est pas b car a<>b et on stop dès que b est devant aperçu*)
          let l = ref g.(t) in
            while !l <> [] do
              let k = fst (hd !l) and r = (snd (hd !l)).(0) in l := tl !l;
                if k = b then begin l := []; v.(b) <- t; h.(b) <- mini (h.(t)) r; p := [] end
                else if v.(k) = (- 2) (*si pas encore vu, marquer*) then
                  begin v.(k) <- t; h.(k) <- mini h.(t) r;
                    p := k :: !p; end;
            done; done; chemin v b, h.(b);;
(*d'abord le chemin puis le débit dispo*)

let PL g n a b (*parcours en largeur, avec file*) =
  (*en particulier nombre d'arêtes minimum*)
  (*on aurait aussi pu utiliser djikstra avec des distances de 1*)
  let v = make_vect n (- 2) and h = make_vect n omega in
v.(a)<-(-1);
    let f = creer_file() in
      enfiler f a;
      while not est_vide_f f do
        let t = defiler f in
          let l = ref g.(t) in
            while !l <> [] do
              let k = fst (hd !l) and r = (snd (hd !l)).(0) in l := tl !l;
                if k = b then begin l := []; v.(b) <- t; h.(b) <- mini (h.(t)) r; f.a <- []; f.b <- [] end
                else if v.(k) = (- 2) then begin v.(k) <- t; h.(k) <- mini h.(t) r; enfiler f k end
            done done; chemin v b, h.(b);;

let djikstra g n a b =
  let v = make_vect n (- 2) and G = copy_vect g and
  w = make_vect n omega and v2 = make_vect n false and
  h = make_vect n omega and minimum = ref (ent 0) and k = ref 1 and
  t = ref (- 1) in w.(a) <- ent 0; v.(a) <- (- 1); v2.(a) <- true;
    
    while G.(a) <> [] do let c, d = hd G.(a) in G.(a) <- tl G.(a);
        w.(c) <- d.(1); h.(c) <- d.(0);v.(c)<-a done;
    
    while !minimum <> omega && !k < n && !t<> b do
      minimum := omega;
      t := (- 1);
      for i = 0 to n - 1 do
        if v2.(i) = false && w.(i) <& !minimum then begin
            t := i; minimum := w.(i) end
      done;
      
      if !t <> (- 1) then begin
          v2.(!t) <- true;
          while G.(!t) <> [] do let x, y = hd G.(!t) in G.(!t) <- tl G.(!t);
              if not v2.(x) then begin
                  if (y.(1) +& w.(!t)) <& w.(x) then begin
                      v.(x) <- !t;
                      w.(x) <- (y.(1) +& w.(!t));
                      h.(x) <- mini h.(x) h.(!t) end
              end;
          done;
        end;
        k := !k + 1;
    done;
    chemin v b, h.(b), w.(b);;

let bellman g n a b =
  let v = make_vect n (- 2) in(*D'où viennent les noeuds dans le chemin*) v.(b)<-(-1);
  let w = make_vect n omega in w.(b) <- ent 0;(*distances*)
  let h = make_vect n omega in
   let k = ref 1 (*nb de sommets rencontrés pour l'instant au minimum, au depart juste b donc 1*)
    and stabilité = ref false in
    (*on calcule le chemin le plus court pour aller jusqu'à b en k pas*)
      while not (!stabilité || !k = n) do
        stabilité := true;
        for i = 0 to n - 1 do
          let l = ref g.(i) in
            while !l <> [] do
              let r, s = hd !l in l := tl !l;
                if w.(r) +& s.(1) <& w.(i) then begin
                    w.(i) <- w.(r) +& s.(1);
                    v.(i) <- r;
                    h.(i)<- s.(0);
                    stabilité := false end done done;
        k := !k + 1;
      done; let dist = w.(a) in 
 let (chem1,cap)= chemin2 v h a in (*car bellman a été fait en partant de b*)
 let chem = rev chem1 in if chem <>[a] then
 (chem, cap, dist) else ([b],cap,dist);;

