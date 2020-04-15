

#open "graphics";;
open_graph "";;

let moinsPremier = function
  [] -> (0, 0)
  | (x, y) :: _ -> (- x, - y);;

let plusPremier = function
  [] -> (0, 0)
  | (x, y) :: _ -> (x, y);;

let translateP = function (a, b) ->
    function (x, y) -> (x + a, y + b);;

let translateL = function (liste, (a, b)) -> map (translateP (a, b)) liste;;

let tourneL = function (liste) -> map (function (x, y) -> (- y, x)) liste;;

(* A partir du motif d'une itération, donne le motif complémentaire *)
let transformeL = function (liste) ->
    let p = plusPremier (liste) and m = moinsPremier (liste) in
    translateL (tourneL (translateL (liste, m)), p)
;;

let saufPremier = function
  [] -> []
  | x :: liste -> liste;;

let rec rev = function
  [] -> []
  | x :: l -> rev (l) @ [x];;

(* passe d'une itération a la suivante  *)
let nouveauMotif = function (liste) ->
    rev (saufPremier (transformeL (liste))) @ liste;;

let rec nemeIteration = function (liste, n) ->
    if n = 0 then liste
    else nemeIteration (nouveauMotif (liste), n - 1);;

(* trace une itération *)
let rec traceListe = function
  [] -> (moveto 0 0)
  | (x, y) :: liste -> (lineto x y); traceListe (liste);;

let traceIteration = function (liste) ->
    let (a, b) = plusPremier (liste) in
    begin
      clear_graph();
      (set_color red);
      (moveto a b);
      (traceListe liste)
    end
;;


let fractal = function (n) ->
    traceIteration (nemeIteration ([450, 200; 450, 204], n));;

(* appel principal *)
fractal (12);;