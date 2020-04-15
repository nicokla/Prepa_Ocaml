(*def_nombre*)

(*attention, pour éviter des complications,
ne pas utiliser nombre dans le cas où une structure de données
impose des entiers (indice de vecteurs par exemple), mais juste des int.*)

let intof = int_of_float and floatof = float_of_int;;

type nombre = ent of int |flo of float |omega |omega_m |indefini;;
(*|fra of (int*int);; qu'on utilise pas pour éviter les complications, dues au risque de 
division par zéro. L'avantage n'est pas vraiment suffisant par rapport à la complication*)
(*Alternative à omega et omega_m : omega of bool, si true -> plus l'infini, sinon moins*)

let est_nul a = match a with 
|ent 0->true|flo 0.->true |_->false;;
let est_infini a = a = omega || a=omega_m;;

let rec prefix +& a b = match a,b with
|ent(i),ent(j)->ent(i+j)
|ent i,flo j-> flo(floatof i +. j)
|flo i,flo j-> flo(i+.j)
|omega,omega_m->indefini
|omega_m,omega->indefini
|_,omega-> omega
|_,omega_m-> omega_m
|_,indefini->indefini
|_->b+&a;;

let moins_nombre a = match a with
|ent a -> ent (-a)
|flo a -> flo (0.-.a)
|omega-> omega_m
|omega_m->omega
|indefini->indefini;;
let prefix -& a b = a+&(moins_nombre b);;

let positif a = match a with
ent k->k>=0
|flo k->k>=0.
|omega->true
|omega_m->false
|indefini->false(*ou true ?*);;
let prefix >=& a b = positif (a-&b);;
let prefix <=& a b = positif (b-&a);;
let prefix >& a b = if b = omega || a=omega_m then false else not a<=& b;; (*positif (a-&b) && not vaut_zero(a-&b)*)
let prefix <& a b = if a = omega || b = omega_m then false else not a>=& b;;

let est_environ_nul precision a = 
if a <& precision && a >& moins_nombre precision then true else false;;
let mini a b = if a<=&b then a else b;;
let maxi a b = if a>=&b then a else b;;

let rec prefix *& a b = match a,b with
|ent(i),ent(j)->ent(i*j)
|flo i,flo j-> flo(i*.j)
|ent i,flo j-> flo(floatof i *. j)
|j,k when est_nul j && est_infini k->indefini
|(u,k) when k>&ent 0 && est_infini u-> u
|u,k when k<&ent 0 && est_infini u-> moins_nombre u
|_,indefini->indefini
|_-> b*&a;;

let inverse a =
match a with
|ent a-> if a<>0 then flo (1./. floatof a) else indefini
|flo a-> if a<>0. then flo (1./.a) else indefini
|x when est_infini x-> ent 0 
|_->indefini;;
let prefix /& a b = a*&(inverse b);;


(*somme et produit associatif et commutatif ? 
grâce à indefini oui, toujours*)
(*par contre si on avait defini omega+omega_m = 0 il y aurait eu un pb,
de mm pour omega*0 = 1.*)
(*généralisation aux rationnels : cf def2_nombre, dans le repertoire essais.*)

