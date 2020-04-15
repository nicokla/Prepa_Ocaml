
      	(* Projet de programmation: outils de compression.
              fichier comprime.ml : compression  *)


type 'a arbre  = Node of mutable ('a arbre * 'a arbre) | Leaf of mutable 'a
and  direction = Gauche | Droite;;


let ch_e       = ref std_in         (* contiendra le canal du fichier a comp *)
and ch_s       = ref std_out          (* contiendra le canal du fichier comp *)
and occurences = make_vect 256 0     (* les occurences des divers caracteres *)
and codage     = make_vect 256 ([Gauche])(* caractere -> chemin dans l'arbre *)
and tampon     = ref [Gauche]  (* contiendra la suite de G/D a coder en bits *)
and li         = ref [(0,0)];;                               (* accumulateur *)




  (* Insertion d'un element dans une liste de paires triee par ordre *)
  (* croissant du second element, en otant les occurences vides.     *)

let rec insert =
  fun (_,0) l1      -> l1
    |  a    []      -> [a]
    |  a    (b::l2) -> if (snd a) <= (snd b) then a::b::l2
                                             else b::(insert a l2);;



  (* Rend le 1er element d'une liste non vide *) 

let head =
  fun [] -> (Leaf 0,0)
    | l  -> hd l;;



  (* Parcours l'arbre de huffman, et rend la liste des paires *)
  (* composees des feuilles et de leurs direction list.       *)

let rec parcours_arbre =
  fun (Leaf a)     -> [(a,[])]
    | (Node (a,b)) -> (map (fun (x,l)-> (x,Gauche::l)) (parcours_arbre a))
                   @  (map (fun (x,l)-> (x,Droite::l)) (parcours_arbre b));;



  (* Rend [(a,_)], ou a est l'arbre de Huffman. *)

let rec construit_huff =
  fun []                -> []
    | [(p,q)]           -> [(p,q)]
    | ((p,q)::(r,s)::l) -> construit_huff (insert (Node (p,r),q+s) l);;



  (* Prend "nom_du_fichier_a_compresser" en argument,   *)
  (* rend unit, en produisant le fichier compresse .zz. *)

let comprime nom_fich =
  begin

      (* Remplit le tableau permettant d'obtenir *)
      (* la direction list d'un caractere.       *)

    let rec fait_codage =
      fun []           -> ()
        | ((x,l)::fin) -> begin
                            codage.(x) <- l;
                            fait_codage fin
                          end

      (* Clair. *)

    and change =
      fun Gauche -> 0
        | Droite -> 1


      (* S'occupe d'ouvrir les fichiers. *)

    and ouvre ()=
      begin
        print_newline();
        print_string ("Compression en cours de: " ^ nom_fich);
        ch_e := (open_in_bin nom_fich);
        ch_s := (open_out_bin (nom_fich ^ ".zz"))
      end


      (* S'occupe de fermer les fichiers. *)

    and ferme ()=
      begin
        print_newline();
	print_string ("Compression de " ^ nom_fich ^ " terminée.");
	print_newline();
	close_in !ch_e;
        close_out !ch_s
      end


      (* Calcul les occurences en lisant le fichier a compresser. *) 

    and remplit_occ ()=
      let carac = ref 0
      in
        for i = 1 to (in_channel_length !ch_e) do
          carac := (input_byte !ch_e);
          occurences.(!carac) <- (occurences.(!carac) + 1)
        done


      (* Produit la liste des paires (caracteres, occurences), *)
      (* triee par occurences croissantes.                     *)

    and liste_croissante ()=
      begin
        for i = 0 to 255 do li:= (insert (i,occurences.(i)) !li) done;
        !li
      end


      (* Inscrit, dans le fichier compresse, l'octet correspondant aux 8     *)
      (* premiers elements du tampon, tant qu'il y a suffisamment d'elements.*)

    and vide_tampon ()=
      while (list_length !tampon >= 8) do
        (output_byte !ch_s (valeur()))
      done


      (* Rend l'octet correspondant aux 8 1ers elements de tampon.*)

    and valeur ()=
      let accu = ref 0
      and accu2 = ref 0 in
      begin
        accu:=0;
        accu2:=128;
        for j = 1 to min 8 (list_length !tampon) do
          accu := !accu + !accu2 * (change (hd(!tampon)));
          accu2 := !accu2/2;
          tampon := tl(!tampon)
        done;
        !accu
      end


  (* Corps imperatif de la fonction comprime: string -> unit. *)

    in
    begin

        (* Initialisation des variables. *)

      li:=[];
      tampon:=[];
      for i=0 to 255 do
        occurences.(i) <- 0;
        (codage.(i) <- [])
      done;

        (* Premiere lecture du fichier a compresser. *)

      ouvre();
      remplit_occ();
      seek_out !ch_s 0;
      seek_in !ch_e 0;

        (* Je stocke les occurences dans le fichier compresse. *)

      for i=0 to 255 do
        output_binary_int !ch_s occurences.(i)
      done;

        (* Dans l'ordre: map (..) (liste_croissante()) fournit la liste   *)
        (* des paires (Leaf caractere, occurence). Ensuite, on construit  *)
        (* l'arbre de Huffman par construit_huff, qui donne [(a,_)], et   *)
        (* on rend a par fst( head...). Puis on parcours l'arbre, ce qui  *)
        (* rend la liste (carac, direction list), que l'on rentre enfin   *)
        (* dans le tableau codage (pour ne pas avoir a chercher le chemin *)
        (* d'un caractere a chaque fois.                                  *)

      fait_codage (
        parcours_arbre (
          fst (head (
            construit_huff (
              map
                (fun (l,i) -> (Leaf l,i))
                (liste_croissante()) ) ) ) ) );

        (* Deuxieme passage sur le fichier a compresser, ou chaque octet *)
        (* est insere dans le tampon sous forme de sa direction list.    *)

      for i = 1 to (in_channel_length !ch_e) do
        tampon := (!tampon @ (codage.(input_byte !ch_e)));
        vide_tampon ()
      done;

        (* Pour finir l'octet en cours, et fermer les deux fichiers. *)

      tampon := (!tampon @ [Gauche;Gauche;Gauche;Gauche;Gauche;Gauche;Gauche]);
      vide_tampon ();
      ferme ()

    end
  end;;


(* Code fini.

   Pour lancer: comprime "nom_de_fichier";;  

   Remarques: pour coder l'arbre dans le fichier, j'ai essaye avec output_value
    et ca n'est finalement preferable que dans de rares cas a un codage direct
    des occurences (taille fixe: 1 Ko). Le codage de l'arbre par output_value
    peut prendre plus de 2 Ko.
    Donc, je laisse l'inscription du tableau des occurences dans le fichier
    compresse.
    Finalement, le mieux reste de coder l'arbre "a la main", dans le fichier
    compresse (du fait qu'il a au plus 256 feuilles), ce qu'on peut faire en,
    au pire des cas, 671 octets.
      (un noeud   : 1 bit pour differencier des feuilles
                    2 * 9 bits pour "l'adresse" des fils
       une feuille: 1 bit ...
                    8 bits pour coder le caractere
       total max  : 256 feuilles, et 255 noeuds => 670.5 octets)      *)



