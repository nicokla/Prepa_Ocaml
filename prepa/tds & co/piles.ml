
type 'a pile=  {mutable pile : 'a list};;

let empile c t = let l = t.pile in t.pile<-(c::l);;

let depile t =
  if t.pile<> [] then
    let (a :: b) = t.pile in begin t.pile <- b; a end
  else failwith "file vide";; 

let cree_pile() = {pile=[]};;

let est_vide t = t.pile= [];;

(*-----------------*)

type operateurs = plus | moins | mult;;
type fonctions = carre;;
type lexeme = var of int | fon of fonctions | op of operateurs;;

let evalue formule =
  let p = cree_pile() in
    let traite_var x = empile x p
    and traite_fon f = let x = depile p in empile (x * x) p
    and traite_op f2 =
      let x = depile p in
        let y = depile p in
          match f2 with
            | plus -> empile (x + y) p
            | moins -> empile (y - x) p
            | mult -> empile (x * y) p
    in
      let traite_lex l = match l with
          | var x -> traite_var x
          | fon f -> traite_fon f
          | op f2 -> traite_op f2
      in
        let rec parcours l = match l with
            | [] -> depile p
            | lex :: q -> traite_lex lex; parcours q in
          parcours formule;;