
TD de programmation No 2. (le 28 octobre 1994)




Exercice 1: Les listes;;


let rec length = fun 
                  []     -> 0
                | (a::l) -> 1+ length l;;

length [1;2;3];;


let rec append  = fun 
                      (c::l) l2 -> (c::append l l2)
                    |  []    l2 -> l2;;

append [1;2;3] [4;5;6];;


let rec reverse_lente = fun
                            []     -> []
                          | (a::l) -> append (reverse l) [a];;

let reverse l = rev (l,[])
                where rec rev = fun
                                   ([],l) -> l
                                   | ( (a::l),ll) -> rev (l,(a::ll));;

reverse [1;2;3;4;5;6;7;8;9;0];;


let rec sigma = fun
                    []     -> 0
                  | (a::l) -> a+sigma l;;

sigma [13;21;34];;


let rec accumuler addition zero = fun
                []     -> zero
              | (a::l) -> addition a (accumuler addition zero l);;

accumuler add_int 0 [1;2;3;4;5];;


let rec map f = fun
                    []     -> []
                  | (a::l) -> f(a)::(map f l);;

map (fun x -> x*x) [0;1;2;3;4;5;6;7;8;9];;


let rec flat = fun
                   []     -> []
                 | (a::l) -> append a (flat l);;

flat [[1;2];[];[3;4;5]];;


let rec insert inf = fun
                        [] x     -> [x]
                      | (a::l) x -> if inf(a,x) then a::(insert inf l x)
                                                else x::(a::l);;

let rec sort inf = fun
                      []     -> []
                    | (a::l) -> insert inf (sort inf l) a;;

sort (fun (a,b) -> a<b) [3;1;4;1;5;9;2;6;5;3;5];;

map (string_length) ["que";"j";"aime";"a";"faire";"apprendre";"ce";"nombre";
                     "utile";"aux";"sages"];;





Exercice 2: Le codage de Huffman;;


type direction = Gauche | Droite;;

type 'a arbre = Node of 'a arbre * 'a arbre | Leaf of 'a;;

Node (Leaf 1,Node (Leaf 3,Node (Leaf 4,Leaf 5)));;


let rec decode = fun  (Leaf l) dir_l           -> (l,dir_l)
                   |  (Node n) (dir::dir_lst)  ->
                      decode (if dir=Gauche then fst n  else snd n) dir_lst;;

let rec decode_list = fun _   []     -> []
                        | arb d_list -> [prem] @ (decode_list arb deuz)
                          where (prem,deuz)=(decode arb d_list);;

let coerce lst = map (fun (l,i) -> (Leaf l,i))
                     (sort (fun (a,b) -> snd a<snd b) lst);;

let make_huff lst = (fun [(p,q)] -> p) (construit_huff lst)
    where rec construit_huff = fun  []                -> []
                                 |  [(p,q)]           -> [(p,q)]
                                 |  ((p,q)::(r,s)::l) -> 
       construit_huff (insert (fun (a,b) -> snd a<snd b) l (Node (p,r),q+s));;

                         (Warning <> error);;


decode (Node (Leaf 1,Node (Leaf 3,Node (Leaf 4,Leaf 5))))
       [Droite;Droite;Gauche;Gauche];;

make_huff (coerce [("a",5);("b",2);("c",1);("d",1);("r",2)]);;

decode_list (make_huff (coerce [("a",5);("b",2);("c",1);("d",1);("r",2)])) 
        [Gauche;   Droite;Droite;Droite;   Droite;Gauche;   Gauche;
         Droite;Droite;Gauche;Gauche;   Gauche];;



let fct_codage a arb = if present then chemin
                                  else []
    where (chemin,present)=(codage arb)
    where rec codage = fun (Leaf l)       -> if l=a then ([],true)
                                                    else ([],false)
                         | (Node (Ag,Ad)) -> 
          if testd then (Droite::wayd,true)
                   else (if testg then (Gauche::wayg,true)
                                  else ([],false)
                         where (wayg,testg)=(codage Ag))
          where (wayd,testd)=(codage Ad);;



fct_codage "d" (make_huff (coerce [("a",5);("b",2);("c",1);("d",1);("r",2)]));;






Exercice 3: Les ensembles représentés par des listes;;



let rec insert inf = fun [] x     -> [x]
                       | (a::l) x -> if x=a then a::l
                     else (if inf(a,x) then a::(insert inf l x)
                                       else x::(a::l));;

let rec sort inf = fun []     -> []
                     | (a::l) -> insert inf (sort inf l) a;;

let rec member x = fun []     -> false
                     | (a::l) -> if x=a then true
                                        else (member x l);;
                     
let rec member_trie inf x = fun []     -> false
                              | (a::l) -> if a=x then true
                                    else (if inf(a,x) then (member_trie inf x l)
                                                      else false);;

member_trie (fun (x,y)->x<y) 4 [1;2;4;5;8];;



let rec union inf = fun [] l              -> l
                      | l  []             -> l
                      | (x1::l1) (x2::l2) -> if x1=x2 
                                then x1::(union inf l1 l2)
                                else (if inf(x1,x2) 
                                      then x1::(union inf l1 (x2::l2))
                                      else x2::(union inf (x1::l1) l2));;

union (fun (x,y)->x<y) [1;3;5;7] [2;5;6;8];;



let rec inter inf = fun [] l              -> []
                      | l  []             -> []
                      | (x1::l1) (x2::l2) -> if x1=x2 
                                then x1::(inter inf l1 l2)
                                else (if inf(x1,x2) 
                                      then inter inf l1 (x2::l2)
                                      else inter inf (x1::l1) l2);;

inter (fun (x,y)->x<y) [1;3;5;7] [2;4;6;8];;



let rec delta inf = fun [] l              -> []
                      | l  []             -> l
                      | (x1::l1) (x2::l2) -> if x1=x2 
                                then delta inf l1 l2
                                else (if inf(x1,x2) 
                                      then x1::(delta inf l1 (x2::l2))
                                      else delta inf (x1::l1) l2);;

delta (fun (x,y)->x<y) [1;3;5;7] [2;5;6;8];;



Fin.
