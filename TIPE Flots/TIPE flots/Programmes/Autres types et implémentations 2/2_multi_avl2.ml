
let rec enleve_tout_aux1 a k = match a with V -> V, 0
    | N (((nb, l), eq), z, V) when nb = k -> z, - 1
    | N (((nb, l), eq), e, f) when k < nb ->
        let g, h = enleve_tout_aux1 e k in let i = eq + h in
            let d2 = (nb, l), i in
              if eq = 1 && h = - 1 then N (d2, g, f), - 1
              else if i > - 2 then N (d2, g, f), 0
              else equil_g (N (d2, g, f))
    | N (((nb, l), eq), e, f) when k > nb ->
        let g, h = enleve_tout_aux1 f k in let i = eq - h in
            let d2 = (nb, l), i in if eq = - 1 && h = - 1 then N (d2, e, g), - 1
              else if i < 2 then ((N (d2, e, g)), 0)
              else equil_d (N (d2, e, g))
   | N (((nb, l), eq), e, f) when k = nb ->
        begin let arb, el, varprof = cherche_petit f in
            let j = eq - varprof in
              let d2 = el, j in if j < 2 && eq >= 0 then N (d2, e, arb), 0
                else if j < 2 && eq = (- 1) then N (d2, e, arb), varprof
                else equil_d (N (d2, e, arb)) end;;


let rec enleve_tout2 a k=match a with |V->V
|(N(((nb,l),eq),c,d)) when nb>k->(N(((nb-1,l),eq),(enleve_tout2 c k),(enleve_tout2 d k)))
|(N(((nb,l),eq),c,d))->(N(((nb,l),eq),c,(enleve_tout2 d k)));;

let enleve_tout1 a k = let arbre = fst(enleve_tout_aux1 a k)
in enleve_tout2 arbre k;;

let rec aux_insere_avl a k (cap, cout) =
  match a with V -> N (((k, [(cap, cout)]), 0), V, V), 1
    | N (((nb, l), eq), c, d) ->
        if k = nb then let l2 = ajout_1 l (cap, cout) in
            if l2 <> [] then ((N (((nb, l2), eq), c, d)), 0)
            else enleve_tout_aux1 a k
        else if k < nb then begin let (t, s) = aux_insere_avl c k (cap, cout) in
              let h = eq + s in let b2 = ((nb, l), h) in
                  if eq >= 0 then begin if h < 2 then ((N (b2, t, d)), s)
                      else let (p, q) = (equil_d (N (b2, t, d))) in (p, q + s) end
                  else N (b2, t, d), 0 end
        else begin let (t, s) = aux_insere_avl d k (cap, cout) in
              let h = eq - s in let b2 = ((nb, l), h) in
                  if eq <= 0 then begin if h > - 2 then ((N (b2, t, d)), s)
                      else let (p, q) = (equil_g (N (b2, t, d))) in (p, q + s) end
                  else N (b2, t, d), 0 end;;

let insere_avl a k (cap,cout) =
let (arbre,eq) = aux_insere_avl a k (cap,cout) in arbre;;