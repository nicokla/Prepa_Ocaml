(*mincost*)
(*Flot de coût minimum *)

include "biblio2/TIPE/Programmes/Types/def_pile";;
include "biblio2/TIPE/Programmes/Types/def_graphe";;
include "biblio2/TIPE/Programmes/Algorithmes/accessibilité";;
include "biblio2/TIPE/Programmes/Algorithmes/fonctions basiques";;

let maxflow (*fonc*) g a b (*e&k si fonc = PL, f&f si fonc = PP*) =
  let n = vect_length g in
    let G = copy_vect g and c = ref true in
      while !c do
        let (chem, cap) = (*fonc*) PL G n a b in if chem <> [b] && cap <> omega then
            begin tout_changer G chem cap; end
          else if chem <> [b] && cap = omega then begin
              failwith ("Flot infini passant par " ^ (string_of_list chem)) end;
          c := chem <> [b] done;simplifier G g n a b;;

let maxflow_mincost g a b =
  let n = vect_length g in
    let G = copy_vect g and c = ref true and cost = ref (ent 0) in
      while !c do
        let (chem, cap, dist) = bellman G n a b in
          if chem <> [b] && cap <> omega then
            begin tout_changer3 G chem cap; cost := !cost +& (cap *& dist); end
          else if chem <> [b] && cap = omega then begin
              failwith ("Flot infini passant par " ^ (string_of_list chem)); end;
          c := chem <> [b] done;
      !cost, (simplifier G g n a b);;

let mincost_flot_fixé (*fonc*) g a b flot =(*pour un flot fixé*)
  let n = vect_length g in
    let G = copy_vect g and c = ref true and f = ref (ent 0) and cost = ref (ent 0) in
      while !c do
        let (chem, cap, dist) = (*fonc*) bellman G n a b in
          if chem <> [b] && !f<&flot then begin
                  let augm = mini cap (flot -& !f) in
                  f := !f +& augm; tout_changer3 G chem augm;
                  cost := !cost +& (augm *& dist);
                   end
          else c:=false;
      done;
      (!cost, simplifier G g n a b);;

let mincost_coût_fixé g a b cout =
  let n = vect_length g in
    let G = copy_vect g and c = ref true and f = ref (ent 0) and cost = ref (ent 0) in
      while !c do
        let (chem, cap, dist) = (*fonc*) bellman G n a b in
          if chem <> [b] && !cost <& cout then begin
              let augm = mini (cap*&dist) (cout -& !cost) in
                f := !f +& (augm/&dist); tout_changer3 G chem (augm/&dist);
                cost := !cost +& (augm);
            end
          else c := false;
      done;
      (!cost, simplifier G g n a b);;