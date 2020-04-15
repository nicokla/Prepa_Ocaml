
      	(* Projet de programmation: outils de compression.
           fichier adapt.ml : codage adaptatif de Huffman  *)


   (* Description d'ensemble du programme:
      J'inscris tout d'abord la longueur (sur 4 octets) du fichier à
      compresser dans le futur fichier compressé.

      Puis, je traite, à chaque caractere lu en entrée, deux listes:
      !l_v et !l_p, de (int * int), le premier int symbolisant
      l'occurence partielle du caractère numéroté par le 2ème int.

      Dans !l_p, un couple particulier (0,256), sert à symboliser la
      sous-liste des caractères non encore utilisés.

      Dans !l_v, les caractères ont des occurences 1, afin de produire un
      arbre complet , mais la somme est éliminée lorsque l'on intègre ce
      sous-arbre à l'arbre principal.

      De plus, lorsque l_v est vide (ie quand tous les caractères ont été
      utilisés), on retire (0,256) de l_p, qui ne sert plus à rien.

      Donc, à chaque étape, la fonction "code" trouve la liste de 0/1
      corespondant au caractère traité, qui est envoyée dans un tampon,
      qui est vidé par paquets de 8 bits. *)

  

let ch_e   = ref std_in      (* pour le canal d'entrée *)
and ch_s   = ref std_out     (* pour le canal de sortie *)
and l_v    = ref [(0,0)]     (* pour les caractères non encore utilisés *)
and l_p    = ref [(0,0)]     (* pour les autres et 256, symbolisant l'arbre
       	       	       	       	des caractères non encore utilisés *)
and tampon = ref [0];;   (* contiendra la suite de 0/1 à regrouper en octets *)


    (* Produit la liste: [(1,255);(1,254);...;(1,0)]. *)

let rec fait_liste_vide =
  fun 0 -> [(1,0)]
    | i -> (1,i)::(fait_liste_vide (i-1));;


    (* Regarde si le caractère n est absent de la liste donnée décroissante. *)

let rec pas_element n =
  fun [] -> true
    | ((a,m)::l) -> if m=n
                    then false
                    else if m<n then true
               	    else pas_element n l;;


    (* Retire le caractere vide (0,256) de l_p, lorsque l_v est vide. *)

let test_tout_plein =
  fun [] ((0,256)::lp) -> ([],lp)
    | lv lp -> (lv,lp);;


    (* Retire le caractere n de la liste. *)
  
let rec retire n =
  fun ((a,m)::l) -> if m=n
                    then l
                    else (a,m)::(retire n l);;


    (* Insère un élément dans une liste de paires triées *)
    (* par ordre croissant du premier élément.           *)

let rec insere a =
  fun []     -> [a]
    | (b::l) -> if fst a <= fst b then a::b::l
                                  else b::(insere a l);;


    (* Retrie la liste (l_p) en incrémentant l'occurence de n.         *)
    (* Cette liste n'est jamais vide, donc pas de problème de matching *)

let rec modifie n =
  fun ((occ,m)::l) -> if m=n then insere (occ+1,n) l
      	       	       	     else (occ,m)::(modifie n l);;


    (* Reçoit une liste triée de (occ,car), et un car, et rend le chemin    *)
    (* dans l'arbre de Huffman corespondant, sous forme d'une liste de 0/1. *)

let code m ll = rev (code_inv m ll)
  where rec code_inv n =
    fun ((occ_a,a)::(occ_b,b)::l) ->
              if      n=a then  0::(code_inv n (insere (occ_a+occ_b,n) l))
              else if n=b then  1::(code_inv n (insere (occ_a+occ_b,n) l))
      	       	          else  code_inv n (insere (occ_a+occ_b,a) l)
      | _ -> [];;


      (* Rend l'octet corespondant aux 8 1ers éléments de !tampon. *)

let valeur () =
  let accu = ref 0
  in
    begin
      accu:=0;
      for j = 1 to 8 do
      	match !tampon
	with (tete::queue) ->  ( accu := 2 * !accu + tete; tampon := queue )
      done;
      !accu
    end;;


      (* Inscrit, dans le fichier compréssé, l'octet corespondant aux 8      *)
      (* premiers éléments du tampon, tant qu'il y a suffisamment d'éléments.*)

let vide_tampon () =
  while (list_length !tampon >= 8)
  do
    (output_byte !ch_s (valeur()))
  done;;


    (* Rajoute à la fin de tampon la direction list du caractère *)
    (* qui vient d'être traité, et le vide, si possible.         *) 

let tamponne l = (tampon := !tampon @ l); vide_tampon ();;


    (* Reçoit le nouveau caractère. Et modifie l_p et l_v en conséquence. *)

let suivant car =
  if (pas_element car !l_v)
  then ( tamponne (code car !l_p);
         l_p := modifie car !l_p )
  else ( tamponne ((code 256 !l_p) @ (code car !l_v));
         l_v := retire car !l_v;
         match test_tout_plein !l_v !l_p
         with  (l1,l2) -> (l_v:= l1; l_p:=l2);
         l_p := insere (1,car) !l_p );;


      (* S'occupe d'ouvrir les fichiers. *)

let ouvre nom =
  begin
    print_newline();
    print_string ("Compression en cours de: " ^ nom);
    print_newline ();
    ch_e := (open_in_bin nom);
    ch_s := (open_out_bin (nom ^ ".zzz"));
    seek_out !ch_s 0;
    seek_in !ch_e 0
  end;;


      (* S'occupe de fermer les fichiers. *)

let ferme nom =
  begin
    print_newline();
    print_string ("Compression de " ^ nom ^ " terminée.");
    print_newline();
    close_in !ch_e;
    close_out !ch_s
  end;;


  (* Prend "nom_du_fichier_à_comprésser" en argument,    *)
  (* rend unit, en produisant le fichier compréssé .zzz. *)

let comprime nom_fich =
  begin

      (* Initialisation. *)

    tampon := [];
    l_v := fait_liste_vide 255;
    l_p := [(0,256)];
    ouvre nom_fich;

      (* Stocke la longueur totale du fichier à comprésser *)
      (* en tête du fichier compréssé.                     *)

    output_binary_int !ch_s (in_channel_length !ch_e);

      (* Passage sur le fichier à compresser, où chaque octet       *)
      (* est inseré dans le tampon sous forme de sa direction list. *)

    for i=1 to (in_channel_length !ch_e) do
      suivant(input_byte !ch_e)
    done;

      (* Pour finir l'octet en cours, et fermer les fichiers. *)

    tampon := !tampon @ [0;0;0;0;0;0;0];
    vide_tampon ();
    ferme nom_fich

  end;;

(* Fin.  Pour lancer: comprime "nom_du_fichier";; *)
(* Il faut 1 minute (sur ss5) pour comprésser 7392 octets de texte. *)
