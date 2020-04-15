

type 'a fifo = {mutable fifo : (('a list) * ('a list ))};;

let enfile t c (*met c en dernier*)=
 let (a,b) = t.fifo in t.fifo<-(a,c::b);;

let rec avance t (*que si personne dans l1*) =
 let (a, b) = t.fifo in
    if b <> [] then begin 
t.fifo <- (hd b :: a, tl b); avance t end;;

let rec defile t (*celui en premier s'en va, on le voit*) =
  let (a, b) = t.fifo in
    if a <> [] then
      begin let k = hd a in t.fifo <- (tl a, b); k; end
    else if b = [] then failwith "fifo vide"
    else begin avance t; defile t end;;

let est_vide t = t.fifo = ([],[]);;

let rec premier t (*dernier faudrait defiler a lenvers donc non*) =
  if est_vide t then failwith "fifo vide" else begin
      if fst (t.fifo) = [] then avance t; hd (fst t.fifo) end;;

let cree_file() = {fifo= ([], []);};;

(*------------*)


