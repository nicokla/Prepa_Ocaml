(*def_ensemble*)
(*si un élément est plusieurs fois dans un ensemble, pas grave*)
(*file:///C:/Program%20Files/WinCaml3_1/
WinCaml/man-caml/node14.13.html*)
(*file:///C:/Program%20Files/WinCaml3_1/
WinCaml/man-caml/node15.16.html*)

(*
value subtract : 'a list -> 'a list -> 'a list
    subtract l1 l2 returns the list l1 where all elements structurally equal to one of the elements of l2 have been removed. 

value union : 'a list -> 'a list -> 'a list
    union l1 l2 appends before list l2 all the elements of list l1 that are not structurally equal to an element of l2. 

value intersect : 'a list -> 'a list -> 'a list
    intersect l1 l2 returns the list of the elements of l1 that are structurally equal to an element of l2. 

*)
#open "set";;
type ens_de_nb == int t;;
let ens_vide = empty eq__compare;;
let y = add 5 ens_vide;;
type 'a ensemble == 'a list;;

let rec appartient a e =
match e with []->false
|_->a = hd e || appartient a (tl e);;

let rec contient a b (*b ensemble et pas élément*)=
match b with [] -> true
| _-> appartient (hd b) a && contient a (tl b);;
let inclus a b = contient b a;;

let rec ajoute a l =(*laisse en place si a déjà dedans*)
match l with |[]->[a]
|_-> if hd l = a then l else hd l :: ajoute a (tl l);;

let rec union a b = match b with []->a
|c::d-> ajoute c (union a d);;

let intersection a b=
let rec aux l b1 =
match b1 with []->[]
|c::d-> if appartient c a then aux (c::l) d
else aux l d in
aux [] b;;