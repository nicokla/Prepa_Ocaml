
(*-------trucs de base---------*)
type etat == int;;
type etats == etat list;;

let rec cardinal (ens: etats) =
  match ens with
    | [] -> 0
    | a :: b -> cardinal b + 1;;

let rec appartient a ens =
  match ens with
    | [] -> false
    | x :: y -> (x = a) || appartient a y;;

let ajout v e =
  if appartient v e then e else v :: e;;

let egalite e1 e2 =
  let rec contient e1 e2 =
    match e1 with
      | [] -> true
      | a :: b -> (appartient a e2) && contient b e2
  in contient e1 e2 && contient e2 e1;;

let rec union e1 e2 =
  match e1 with [] -> e2
    | a :: b -> union b (ajout a e2);;

let rec plein el ens =
 match ens with
    | [] -> true
    | x :: y -> (x = el) && plein el y;;

let filtre e = union e [];;

let intersection e1 e2 =
  let rec intersectionr e1 e2 acc =
    match e1 with
      | [] -> acc
      | a :: b -> if appartient a e2 then intersectionr b e2 (a :: acc)
          else intersectionr b e2 acc
  in intersectionr e1 e2 [];;

(*--------------*)
let rec ajout e lst_ens=match lst_ens with
|[]->[]
|ens::q->(e::ens) :: (ajout e q);;

let rec parties ens = match ens with
|[]->[[]]
|e::sEns-> let partiesQ=parties sEns in
(partiesQ)@(ajout e partiesQ);;

let rec combinaisons p ens= match p,ens with
|0,_-> [[]]
|_,[]->[]
|p,e::sEns-> (combinaisons p sEns) @ (ajout e (combinaisons (p-1) sEns));;

(*surjections cf dm an dernier*)