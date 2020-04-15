(*def_file*)
(*les fifo, ie piles, sont les ref list normales,
et les lifo sont les files*)

type 'a file ={mutable a: ('a list) ; mutable b: 'a list};;

let creer_file() ={a=[];b=[]};;
let est_vide_f f = f = {a=[];b=[]};;
let enfiler f r = f.b<-r::f.b;; 
let inverse l =
let rec aux acc l = match l with [] -> acc
|_-> aux ((hd l) :: acc) (tl l) in aux [] l;;
let reactu f = f.a<- (*a@*)inverse f.b;f.b<-[];;
(*push*)
let rec defiler f = match f.a,f.b with
|[],[]->failwith "file vide"
|[],_-> reactu f; defiler f
|_->let k = hd f.a in f.a<-tl (f.a); k;; 


(*-----------------*)

type 'a pile = {mutable c:'a list};;
(*plus pratique à utiliser que les ref list parfois ?*)

let creer_pile() = {c=[]};;
let est_vide_p p = p = {c=[]};;
let empiler p a = p.c<-a::p.c;;
let depiler p = if p.c = [] then failwith "pile vide"
else let k = hd p.c in p.c<-tl p.c;k;;