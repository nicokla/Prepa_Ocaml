let rec insere_1_multi_avl_aux a k nb =
  match a with V -> N (((k, [nb]), 0), V, V), 1
    | N (((nb2, l), eq), c, d) ->
        if k = nb then ((N (((nb2, nb::l), eq), c, d)), 0)
        else if k < nb2 then begin let (t, s) = insere_1_multi_avl_aux c k nb in
              let h = eq + s in let b2 = ((nb, l), h) in
                  if eq >= 0 then begin if h < 2 then ((N (b2, t, d)), s)
                      else let (p, q) = (equil_d (N (b2, t, d))) in (p, q + s) end
                  else N (b2, t, d), 0 end
        else begin let (t, s) = insere_1_multi_avl_aux d k nb in
              let h = eq - s in let b2 = ((nb, l), h) in
                  if eq <= 0 then begin if h > - 2 then ((N (b2, t, d)), s)
                      else let (p, q) = (equil_g (N (b2, t, d))) in (p, q + s) end
                  else N (b2, t, d), 0 end;;

let insere_1_multi_avl a k nb =
let (arbre,eq) = insere_1_multi_avl_aux a k nb in arbre;;

let rec enleve_aux_avl_2 fonc a k =
  match a with V -> V, 0, (ent (- 1), omega)
    | N (((nb, l), eq), z, V) when nb = k ->
        let l2, p = fonc l in if l2 = [] then z, - 1, p
          else (N (((nb, l2), eq), z, V)), 0, p
    | N (((nb, l), eq), e, f) when k < nb ->
        let g, h, z = enleve_aux_avl fonc e k in
          let i = eq + h in
            let d2 = (nb, l), i in
              if eq = 1 && h = - 1 then N (d2, g, f), - 1, z
              else if i > - 2 then N (d2, g, f), 0, z
              else let u, v = equil_g (N (d2, g, f)) in u, v, z
    | N (((nb, l), eq), e, f) when k > nb ->
        let g, h, z = enleve_aux_avl fonc f k in
          let i = eq - h in let d2 = (nb, l), i in
              if eq = - 1 && h = - 1 then N (d2, e, g), - 1, z
              else if i < 2 then N (d2, e, g), 0, z
              else let u, v = equil_d (N (d2, e, g)) in u, v, z
    | N (((nb, l), eq), e, f) when k = nb ->
        let l2, p = plus_court_1 l in
          if l2 <> [] then (N (((nb, l2), eq), e, f)), 0, (p, q) else begin
              let arb, el, varprof = cherche_petit f in
                let j = eq - varprof in let d2 = el, j in
                    if j < 2 && eq >= 0 then N (d2, e, arb), 0, p
                    else if j < 2 && eq = - 1 then N (d2, e, arb), varprof, p
                    else let u, v = equil_d (N (d2, e, arb))
                      in u, v, (p, q) end;;

let enleve_aux_avl_plus_petit_2 a k =
let arb,eq,(p,q)=enleve_aux_avl plus_court_1 a k in arb,(p,q);;

let enleve_aux_avl_plus_long_2 a k=
let arb,eq,(p,q)=enleve_aux_avl plus_long_1 a k in arb,(p,q);;