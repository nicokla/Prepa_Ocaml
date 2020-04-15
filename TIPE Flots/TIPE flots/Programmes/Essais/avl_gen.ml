(*avl_gen*)

include "biblio2/TIPE/programmes/types/def_nombre";;

type balance == int;;
type 'a avl = V | N of (('a * balance) * ('a avl) * ('a avl));;
type avl_ici == (int * (nombre vect)) avl ;; 

let inférieur a b = fst a<fst b;;
let pareil a b = fst a = fst b ;;
let supérieur a b = fst a>fst b ;;

let recherche_avl a k =(*k est le numéro du snd sommet, plus le vect des capa et des dist*)
  let rec aux a =
    match a with V -> false
      | N (u, w, x) -> if pareil k (fst u) then true
          else if supérieur k (fst u) then aux x
          else aux w in aux a;;

let rot_d z = match z with N (q, N (p, a, b), c) -> N (p, a, N (q, b, c))
    | _ -> V;;

let rot_g z =match z with N (q, c, N (p, a, b)) -> N (p, N (q, c, a), b)
    | _ -> V;;

let rot_gd z=match z with N(a,b,c)->rot_d(N(a,rot_g b,c))
|_->V;;

let rot_dg z= match z with N(a,b,c)->rot_g(N(a,b,rot_d c))
|_->V;;

let equil_d a =
match a with N ((b, 2), d, e) ->begin
match d with
| N ((f, 1), h, i) -> (rot_d (N ((b, 0), N ((f, 0), h, i), e))), (- 1)
| N ((f, 0), h, i) -> (rot_d (N ((b, 1), N ((f, (- 1)), h, i), e))), 0
| N ((f, - 1), h, i) ->
begin
match i with
| N ((j, - 1), l, m) -> (rot_gd (N ((b, 0), N ((f, 1), h, N ((j, 0), l, m)), e))), (- 1)
| N ((j, 0), l, m) -> (rot_gd (N ((b, 0), N ((f, 0), h, N ((j, 0), l, m)), e))), (- 1)
| N ((j, 1), l, m) -> (rot_gd (N ((b, -1), N ((f, 0), h, N ((j, 0), l, m)), e))), (- 1)
end
end;;

let equil_g a =
  match a with N ((b, -2), d, e) ->
        begin
          match e with
            | N ((f, -1), h, i) -> (rot_g (N ((b, 0), d, N ((f, 0),     h, i)))), (- 1)
            | N ((f, 0), h, i) ->  (rot_g (N ((b, 1), d, N ((f, (- 1)), h, i)))), 0
            | N ((f, 1), h, i) ->
                begin
                  match h with
                    | N ((j, 1), l, m) ->(rot_dg (N ((b, 0), d, N ((f,-1), N ((j, 0), l, m),i)))), (- 1)
                    | N ((j, 0), l, m) ->(rot_dg (N ((b, 0), d, N ((f, 0), N ((j, 0), l, m),i)))), (- 1)
                    | N ((j,-1), l, m) ->(rot_dg (N ((b, 1), d, N ((f, 0), N ((j, 0), l, m),i)))), (- 1)
                end
        end;;

(*------------------------------*)

let rec cherche_petit a =
  match a with V -> failwith "arbre vide"
    | N (b, V, d) -> (d, fst b, - 1)
    | N (b, c, d) -> let arb, el, varprof = cherche_petit c in
          let e = snd b + varprof in
            let b2 = fst b, e in
              if varprof = - 1 && snd b = - 1 then
                let f, g = equil_g (N (b2, arb, d)) in (f, el, g)
              else if varprof = - 1 && snd b = 1 then ((N (b2, arb, d)), el, - 1)
              else ((N (b2, arb, d)), el, 0);;

let rec enleve_aux_avl a b =
  match a with 
V -> V, 0,(-1,[||])
| N (c, z, V) when pareil b (fst c) -> z, - 1,(fst c)
| N (d, e, f) when inférieur b (fst d) ->
   let g, h,z = enleve_aux_avl e b in
   let i = snd d + h in
   let d2 = fst d, i in
     if snd d = 1 && h = - 1 then N (d2, g, f), - 1, z
     else if i > - 2 then N (d2, g, f), 0, z
     else (*if i =-2 then*) let u,v= equil_g (N (d2, g, f)) in u,v,z
| N (d, e, f) when supérieur b (fst d) -> 
   let g, h,z = enleve_aux_avl f b in
   let i = snd d - h in
   let d2 = fst d, i in
     if snd d = - 1 && h = - 1 then N (d2, e, g), - 1,z
     else if i < 2 then N (d2, e, g), 0,z
     else (*if i =2 then*)let u,v = equil_d (N (d2, e, g)) in u,v,z
| N (d, e, f) when pareil b (fst d) ->
   let arb, el, varprof = cherche_petit f in
   let j = snd d - varprof in
   let d2 = el, j in
     if j < 2 && snd d >= 0 then N (d2, e, arb), 0,(fst d)
     else if j < 2 && snd d = - 1 then N (d2, e, arb), varprof,(fst d)
     else (*if j = 2 then*) let u,v = equil_d (N (d2, e, arb)) in u,v,fst d;;

let enleve_avl a b = let c,d,e = enleve_aux_avl a b in c,e;;

(*-------------------------------------*)

let rec aux_insere_avl a k = (*renvoie l'arbre, et la différence de profondeur avec avant*)
  match a with
| V -> N ((k, 0), V, V), 1
| N (b, c, d) -> 
if pareil k (fst b) then begin 
   let cap = (snd (fst b)).(0) +& (snd k).(0) in
   let b2 = ((fst (fst b), [|cap; (snd (fst b)).(1)|]), snd b) in
     if cap >& ent 0 then (N (b2, c, d), 0)
     else let z,y,x = enleve_aux_avl a (fst b) in z,y end
else if inférieur k (fst b) then begin
   let (t, s) = aux_insere_avl c k (*s=1 ou 0*) in
   let h = (snd b) + s in let b2 = (fst b, h) in
     if snd b >= 0 then begin if h < 2 then ((N (b2, t, d)), s)
     else let (p, q) = (equil_d (N (b2, t, d))) in (p, q + s) end
     else N (b2, t, d), 0
end 
else begin
   let (t, s) = aux_insere_avl d k in
   let h = (snd b - s) in let b2 = (fst b, h) in
     if snd b <= 0 then begin
       if h > (- 2) then ((N (b2, c, t)), s)
       else let (p, q) = (equil_g (N (b2, c, t))) in (p, q + s) end
     else N (b2, c, t), 0 end;;

let rec insere_avl a k = let (p,m) = aux_insere_avl a k in p;;

(*---------------------------------*)
(*tests*)
#open "graphics";;

let rec mettre l a =
match l with
[]->a
|_-> mettre (tl l) (insere_avl a ((hd l),[|ent 1 ;ent 1|]));;

let rec representer_avl1 a k = match a with V -> ()
    | N (p, q, s) -> let r = current_point () in fill_circle (fst r) (snd r) 3;
          draw_string (string_of_int (fst (fst p)));
          moveto (fst r) (snd r); 
          if q <> V then begin lineto ((fst r) - k) ((snd r) - 50) end;
          representer_avl1 q (k / 2);
          moveto (fst r) (snd r);
          if s <> V then begin lineto ((fst r) + k) ((snd r) - 50) end;
          representer_avl1 s (k / 2);;

let representer_avl a = open_graph "1000x800"; clear_graph();moveto 500 750;
representer_avl1 a 250;;
(*
representer_avl ((mettre [3;6;9;12;15;18;21;24;27;30;33;36;77;11;2;1;0] V));;
*)


