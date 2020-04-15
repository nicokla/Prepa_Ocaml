
      	(* Projet de programmation: outils de compression.
           fichier adapt.ml : codage adaptatif de Huffman  *)


   (* Description d'ensemble du programme:
      J'inscris tout d'abord la longueur (sur 4 octets) du fichier �
      compresser dans le futur fichier compress�.

      Puis, je traite, � chaque caractere lu en entr�e, deux listes:
      !l_v et !l_p, de (int * int), le premier int symbolisant
      l'occurence partielle du caract�re num�rot� par le 2�me int.

      Dans !l_p, un couple particulier (0,256), sert � symboliser la
      sous-liste des caract�res non encore utilis�s.

      Dans !l_v, les caract�res ont des occurences 1, afin de produire un
      arbre complet , mais la somme est �limin�e lorsque l'on int�gre ce
      sous-arbre � l'arbre principal.

      De plus, lorsque l_v est vide (ie quand tous les caract�res ont �t�
      utilis�s), on retire (0,256) de l_p, qui ne sert plus � rien.

      Donc, � chaque �tape, la fonction "code" trouve la liste de 0/1
      corespondant au caract�re trait�, qui est envoy�e dans un tampon,
      qui est vid� par paquets de 8 bits. *)

  

let ch_e   = ref std_in      (* pour le canal d'entr�e *)
and ch_s   = ref std_out     (* pour le canal de sortie *)
and l_v    = ref [(0,0)]     (* pour les caract�res non encore utilis�s *)
and l_p    = ref [(0,0)]     (* pour les autres et 256, symbolisant l'arbre
       	       	       	       	des caract�res non encore utilis�s *)
and tampon = ref [0];;   (* contiendra la suite de 0/1 � regrouper en octets *)


    (* Produit la liste: [(1,255);(1,254);...;(1,0)]. *)

let rec fait_liste_vide =
  fun 0 -> [(1,0)]
    | i -> (1,i)::(fait_liste_vide (i-1));;


    (* Regarde si le caract�re n est absent de la liste donn�e d�croissante. *)

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


    (* Ins�re un �l�ment dans une liste de paires tri�es *)
    (* par ordre croissant du premier �l�ment.           *)

let rec insere a =
  fun []     -> [a]
    | (b::l) -> if fst a <= fst b then a::b::l
                                  else b::(insere a l);;


    (* Retrie la liste (l_p) en incr�mentant l'occurence de n.         *)
    (* Cette liste n'est jamais vide, donc pas de probl�me de matching *)

let rec modifie n =
  fun ((occ,m)::l) -> if m=n then insere (occ+1,n) l
      	       	       	     else (occ,m)::(modifie n l);;


    (* Re�oit une liste tri�e de (occ,car), et un car, et rend le chemin    *)
    (* dans l'arbre de Huffman corespondant, sous forme d'une liste de 0/1. *)

let code m ll = rev (code_inv m ll)
  where rec code_inv n =
    fun ((occ_a,a)::(occ_b,b)::l) ->
              if      n=a then  0::(code_inv n (insere (occ_a+occ_b,n) l))
              else if n=b then  1::(code_inv n (insere (occ_a+occ_b,n) l))
      	       	          else  code_inv n (insere (occ_a+occ_b,a) l)
      | _ -> [];;


      (* Rend l'octet corespondant aux 8 1ers �l�ments de !tampon. *)

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


      (* Inscrit, dans le fichier compr�ss�, l'octet corespondant aux 8      *)
      (* premiers �l�ments du tampon, tant qu'il y a suffisamment d'�l�ments.*)

let vide_tampon () =
  while (list_length !tampon >= 8)
  do
    (output_byte !ch_s (valeur()))
  done;;


    (* Rajoute � la fin de tampon la direction list du caract�re *)
    (* qui vient d'�tre trait�, et le vide, si possible.         *) 

let tamponne l = (tampon := !tampon @ l); vide_tampon ();;


    (* Re�oit le nouveau caract�re. Et modifie l_p et l_v en cons�quence. *)

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
    print_string ("Compression de " ^ nom ^ " termin�e.");
    print_newline();
    close_in !ch_e;
    close_out !ch_s
  end;;


  (* Prend "nom_du_fichier_�_compr�sser" en argument,    *)
  (* rend unit, en produisant le fichier compr�ss� .zzz. *)

let comprime nom_fich =
  begin

      (* Initialisation. *)

    tampon := [];
    l_v := fait_liste_vide 255;
    l_p := [(0,256)];
    ouvre nom_fich;

      (* Stocke la longueur totale du fichier � compr�sser *)
      (* en t�te du fichier compr�ss�.                     *)

    output_binary_int !ch_s (in_channel_length !ch_e);

      (* Passage sur le fichier � compresser, o� chaque octet       *)
      (* est inser� dans le tampon sous forme de sa direction list. *)

    for i=1 to (in_channel_length !ch_e) do
      suivant(input_byte !ch_e)
    done;

      (* Pour finir l'octet en cours, et fermer les fichiers. *)

    tampon := !tampon @ [0;0;0;0;0;0;0];
    vide_tampon ();
    ferme nom_fich

  end;;

(* Fin.  Pour lancer: comprime "nom_du_fichier";; *)
(* Il faut 1 minute (sur ss5) pour compr�sser 7392 octets de texte. *)
