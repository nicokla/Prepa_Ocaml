
(*
include "biblio2/TIPE/programmes/types/def_nombre";;
include "biblio2/TIPE/Programmes/autres types et implémentations 2/listes";;
include "biblio2/TIPE/programmes/autres types et implémentations 2/2_multi_avl1";;
*)

let rec enleve_tout_aux2 a k = match a with V -> V, 0, (ent (-1),indefini)
    | N (((nb, l), eq), z, V) when nb = k -> z, - 1, l
    | N (((nb, l), eq), e, f) when k < nb ->
        let g, h,l2 = enleve_tout_aux2 e k in let i = eq + h in
            let d2 = (nb, l), i in
              if eq = 1 && h = - 1 then N (d2, g, f), - 1,l2
              else if i > - 2 then N (d2, g, f), 0,l2
              else let u,v=equil_g (N (d2, g, f)) in u,v,l2
    | N (((nb, l), eq), e, f) when k > nb ->
        let g, h,l2 = enleve_tout_aux2 f k in let i = eq - h in
            let d2 = (nb, l), i in if eq = - 1 && h = - 1 then N (d2, e, g), - 1,l2
              else if i < 2 then ((N (d2, e, g)), 0,l2)
              else let u,v = equil_d (N (d2, e, g)) in u,v,l2
   | N (((nb, l), eq), e, f) when k = nb ->
        begin let arb, el, varprof = cherche_petit f in
            let j = eq - varprof in
              let d2 = el, j in if j < 2 && eq >= 0 then N (d2, e, arb), 0, l
                else if j < 2 && eq = (- 1) then N (d2, e, arb), varprof,l
                else let r,s = equil_d (N (d2, e, arb)) in r,s,l end;;

let enleve_tout3 a k = let arbre, eq, elt = enleve_tout_aux2 a k in
    arbre, elt;;

let rec aux_insere_avl4 a k (cap, cout) =
  match a with V -> N (((k, (cap, cout)), 0), V, V), 1
    | N (((nb, l), eq), c, d) when k = nb ->
        if cout = snd l then
          if fst l +& cap >& ent 0 then (N (((nb, (fst l +& cap, snd l)), eq), c, d)), 0
          else let p, q, r = enleve_tout_aux2 a k in p, q
        else N (((nb, (cap, cout)), eq), c, d), 0
    | N (((nb, l), eq), c, d) when k < nb ->
        begin let (t, s) = aux_insere_avl4 c k (cap, cout) in
            let h = eq + s in let b2 = ((nb, l), h) in
                if eq >= 0 then begin if h < 2 then ((N (b2, t, d)), s)
                    else let (p, q) = (equil_d (N (b2, t, d))) in (p, q + s) end
                else N (b2, t, d), 0 end
    | N (((nb, l), eq), c, d) -> begin let (t, s) = aux_insere_avl4 d k (cap, cout) in
            let h = eq - s in let b2 = ((nb, l), h) in
                if eq <= 0 then begin if h > - 2 then ((N (b2, t, d)), s)
                    else let (p, q) = (equil_g (N (b2, t, d))) in (p, q + s) end
                else N (b2, t, d), 0 end;;

let rec insere_avl4 a k (cap,cout) =
let (arbre,eq) = aux_insere_avl4 a k (cap,cout) in arbre;;

let enleve_tout6 a k =
let arb, eq = enleve_tout3 a k in (enleve_tout2 arb k);;
