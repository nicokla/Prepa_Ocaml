
let rec insere e l =match l with
|[] -> [e]
|a::q -> if e < a then e::l else (a::insere e q );;

let rec tri l=match l with
|[]->[]
|a::q-> insere a (tri q);;

let rec iter f n a= match n with
|1-> f(a)
|_->f ( iter f (n-1) (a));;