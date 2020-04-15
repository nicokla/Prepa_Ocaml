type 'a avl = V | N of (('a * int) * ('a avl) * ('a avl));;

type avl1 == (int*((nombre*nombre)list)) avl;;

let recherche_avl a k =
let rec aux a = match a with V -> false
|N (((num,l),eq), w, x) -> if k = num then true
else if k > num then aux x
else aux w in aux a;;

let rot_d z = match z with N (q, N (p, a, b), c) -> N (p, a, N (q, b, c))
|_ -> V;;

let rot_g z =match z with N (q, c, N (p, a, b)) -> N (p, N (q, c, a), b)
|_ -> V;;

let rot_gd z=match z with N(a,b,c)->rot_d(N(a,rot_g b,c))
|_->V;;

let rot_dg z= match z with N(a,b,c)->rot_g(N(a,b,rot_d c))
|_->V;;

let equil_d a =
match a with N ((b, 2), d, e) -> begin
match d with
|N ((f, 1), h, i) -> (rot_d (N ((b, 0), N ((f, 0), h, i), e))), (-1)
|N ((f, 0), h, i) -> (rot_d (N ((b, 1), N ((f, (- 1)), h, i), e))), 0
|N ((f, - 1), h, i) ->
begin match i with
|N ((j, - 1), l, m) -> (rot_gd (N ((b, 0), N ((f, 1), h, N ((j, 0), l, m)), e))), (-1)
|N ((j, 0), l, m) -> (rot_gd (N ((b, 0), N ((f, 0), h, N ((j, 0), l, m)), e))), (-1)
|N ((j, 1), l, m) -> (rot_gd (N ((b, - 1), N ((f, 0), h, N ((j, 0), l, m)), e))), (-1)
end end;;

let equil_g a =
match a with N ((b, -2), d, e) ->
begin
match e with
|N ((f, -1), h, i) -> (rot_g (N ((b, 0), d, N ((f, 0),h, i)))), (- 1)
|N ((f, 0), h, i) ->  (rot_g (N ((b, 1), d, N ((f, (- 1)), h, i)))), 0
|N ((f, 1), h, i) ->
begin
match h with
|N ((j, 1), l, m) ->(rot_dg (N ((b, 0), d, N ((f,-1), N ((j, 0), l, m),i)))), (- 1)
|N ((j, 0), l, m) ->(rot_dg (N ((b, 0), d, N ((f, 0), N ((j, 0), l, m),i)))), (- 1)
|N ((j,-1), l, m) ->(rot_dg (N ((b, 1), d, N ((f, 0), N ((j, 0), l, m),i)))), (- 1)
end end;;

let rec cherche_petit a = match a with V-> failwith "arbre vide"
|N ((a,eq), V, d) -> (d, a , - 1)
|N (b, c, d) -> let arb, el, varprof = cherche_petit c in
let e = snd b + varprof in
let b2 = fst b, e in
if varprof = - 1 && snd b = - 1 then
let f, g = equil_g (N (b2, arb, d)) in (f, el, g)
else if varprof = - 1 && snd b = 1 then ((N (b2, arb, d)), el, - 1)
else ((N (b2, arb, d)), el, 0);;

let rec enleve_aux_avl fonc a k =
match a with V -> V, 0, (ent (- 1), omega)
|N (((nb, l), eq), z, V) when nb = k->
let l2, (p, q) = fonc l in if l2 = [] then z, - 1, (p, q)
else (N (((nb, l2), eq), z, V)), 0, (p, q)
|N (((nb, l), eq), e, f) when k < nb ->
let g, h, z = enleve_aux_avl fonc e k in
let i = eq + h in
let d2 = (nb, l), i in
if eq = 1 && h = - 1 then N (d2, g, f), - 1, z
else if i > - 2 then N (d2, g, f), 0, z
else let u, v = equil_g (N (d2, g, f)) in u, v, z
|N (((nb, l), eq), e, f) when k > nb->
let g, h, z = enleve_aux_avl fonc f k in
let i = eq - h in let d2 = (nb, l), i in
if eq = - 1 && h = - 1 then N (d2, e, g), - 1, z
else if i < 2 then N (d2, e, g), 0, z
else let u, v = equil_d (N (d2, e, g)) in u, v, z
|N (((nb, l), eq), e, f) when k = nb ->
let l2, (p, q) = plus_court_1 l in
if l2 <> [] then (N (((nb, l2), eq), e, f)), 0, (p, q) else begin
let arb, el, varprof = cherche_petit f in
let j = eq - varprof in let d2 = el, j in
if j < 2 && eq >= 0 then N (d2, e, arb), 0, (p, q)
else if j < 2 && eq = - 1 then N (d2, e, arb), varprof, (p, q)
else let u, v = equil_d (N (d2, e, arb))
in u, v, (p, q) end;;

let enleve_aux_avl_plus_petit a k =
let arb,eq,(p,q)=enleve_aux_avl plus_court_1 a k in arb,(p,q);;

let enleve_aux_avl_plus_long a k=
let arb,eq,(p,q)=enleve_aux_avl plus_long_1 a k in arb,(p,q);;



