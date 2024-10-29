type dimension = int;; (*restreint aux entiers strictement positifs*)

type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)
type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)

type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
               | Libre 
               | Code of string (*une chaine restreinte a 3 caracteres*);;


type case_coloree = case * couleur;;

type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)
          
type coup = Du of case * case | Sm of case list;;


(*------------------------------------------------------------------------------------*)
let indice_valide (x:int) (dim:dimension) : bool =
  x >= -2*dim && x<= 2*dim;;

indice_valide 4 3 ;;
indice_valide 4 5 ;;
indice_valide 3 1 ;;



(*------------------------------------------------------------------------------------*)
let est_case ((i,j,k):case):bool=
 (i+j+k=0);;

est_case (0,0,0) ;;
est_case (0,0,9) ;;



(*------------------------------------------------------------------------------------*)

(* associe
   - Prend une clé 'a', une liste de paires clé-valeur ('a * 'b) list, et une valeur par défaut 'defaut'.
   - Recherche la clé dans la liste et renvoie la valeur associée s'il y a correspondance, sinon renvoie la valeur par défaut.
   Remarque : Un changement a été apporté dans la fonction associe, car elle renvoyait toujours la valeur par défaut (Libre),
             ce qui empêchait l'affichage correct de la configuration souhaitée.
*)
let rec associe (a:'a) (l:('a * 'b) list) (defaut:'b):'b = 
  match l with
  | [] -> defaut
  | (cord, value)::fin -> if cord = a then value else associe a fin defaut
;;
(*
(associe (0, 0, 0) [((−1, 0, 1), Jaune); ((0, 0, 0), Vert)] Libre) = Vert
(associe (1, −1, 0) [((−1, 0, 1), Jaune); ((0, 0, 0), Vert)] Libre) = Libre
*)

(*------------------------------------------------------------------------------------*)
(* Debut Partie I *)

(*Q2*)
(* - Profil : est_dans_losange : case -> dimension -> bool
    - Sémantique : La fonction est_dans_losange c dim retourne true 
  une case est dans le losange Nord-Sud . Sinon, elle retourne false.
    - Exmples : 
    est_dans_losange (2,0,-2) 3 = true
    est_dans_losange (3,3,-6) 3 = false
   *)
let est_dans_losange (c:case) (dim:dimension):bool = 
  let (i,j,k)=c in 
  (j<=dim && j>=(-dim)) && (k<=dim && k>=(-dim));;           


est_dans_losange (2,0,-2) 3 ;;
est_dans_losange (3,3,-6) 3 ;;


(*------------------------------------------------------------------------------------*)

(*Q3*
    - Profil : est_dans_etoile : case -> dimension -> bool
    - Sémantique : La fonction est_dans_etoile c dim retourne true 
    si les coordonnées de la case c sont situées dans une étoile délimitée
    par la dimension dim. Sinon, elle retourne false.
    -Exmples : 
    est_dans_etoile (-2,2,0) 3 = true
    est_dans_etoile (-2,6,0) 3 = false
  *)
let est_dans_etoile (c:case) (dim:dimension):bool = 
  let(i,j,k)=c in 
  ((i+j+k) = 0 ) && (est_dans_losange c dim = true) || (est_dans_losange ((i+j),(-i),(-j)) dim = true) || ((est_dans_losange (((k+i)),(-k),(k+j)))  dim = true)
  ;;

est_dans_etoile (-1,1,0) 3 ;;
est_dans_etoile (-2,6,3) 3 ;;



(*------------------------------------------------------------------------------------*)

(* Q4.*)
(* case_place | Fonction auxiliaire
    - Profil : case_place : case -> case
    - Sémantique :  La fonction case_place c prend une case c en argument et
     retourne une nouvelle case résultant après avoir fait tourner le plateau une fois 
    -Exemples :
    case_place (0,0,0) = (0,0,0)
    case_place (6,-3,-3) = (3,-6,3)
    *)
let case_place (c:case) :case = 
let (i,j,k)=c in 
let new_i = i+j in
let new_j = (-i) in
let new_k = (-j) in
(new_i,new_j,new_k);;


case_place (2,2,2) ;;
case_place (0,0,3) ;;

(*------------------------------------------------------------------------------------*)

(* tourner_case 
    - Profil : tourner_case : int -> case -> case
    - Sémantique :  La fonction tourner_case m c prend un entier m 
    et une case c en argument et retourne la case résultant de la
    rotation du plateau de m sixièmes de tour dans le sens anti-horaire.
    -Exemples :
    tourner_case 1 (6,-3,-3) = (3,-6,3)
    tourner_case 2 (1,-1,0) = (-1,0,1)    
*)
let rec tourner_case (m:int) (c:case) : case = 
match m with
|0 -> c
|m -> tourner_case (m-1) (case_place c) ;;


tourner_case 1 (6,1,0) ;;
tourner_case 1 (0,0,0) ;;
(*------------------------------------------------------------------------------------*)
(* Q5.*)
(* 
    - Profil : translate : case -> vecteur -> case
    - Sémantique : La fonction translate c v prend une case c et 
    un vecteur v en argument et retourne la case résultant de la translation 
    de la case c par le vecteur v.
    -Exemples : 
    translate (-4,1,3) (0,2,-2) = (-4,3,1)
*)
let translate (c:case) (v:vecteur):case = 
let (a,b,c) = c in 
let (e,f,g) = v in 
(a+e,b+f,c+g);;


translate (-4,1,3) (0,2,-2) ;;
translate (-2,0,2) (0,2,-2) ;;


(*------------------------------------------------------------------------------------*)

(* Q6. *)
   (*- Profile : diff_case : case -> case -> vecteur
   - Sémentique :  La fonction diff_case c1 c2 prend deux cases c1 et c2
  en argument et retourne le vecteur de translation nécessaire pour passer
  de la case c1 à la case c2.
  - Exemples : 
  diff_case (0,0,0) (0,-2,2) = (0,2,-2) *)
let diff_case (c1:case) (c2:case):vecteur = 
let (a,b,c) = c1 in 
let (e,f,g) = c2 in 
(a-e,b-f,c-g);;


diff_case (1,3,0) (0,-1,2) ;;
diff_case (0,1,-2) (2,3,0) ;;



(*------------------------------------------------------------------------------------*)

(* Q7. *)
(* - Profile : sont_cases_voisines : case -> case -> bool
   - Sémantique : La fonction sont_cases_voisines c1 c2 teste si deux cases c1 et c2 sont voisines.
   - Exemples : 
   sont_cases_voisines (1, 0, -1) (1, 1, -2) = true
   sont_cases_voisines (1, 0, -1) (2, 0, -2) = false
   *)
let sont_cases_voisines (c1:case) (c2:case):bool = 
let (i,j,k) = diff_case c1 c2 in
((i=0 || i=1 || i=(-1)) && (j=0 || j= 1 || j=(-1)) && ( k=0 || k=1 || k=(-1)));;


sont_cases_voisines (1, 0, -1) (1, 1, -2) ;;
sont_cases_voisines (1, 0, 1) (2, 0, -2) ;;



(*------------------------------------------------------------------------------------*)

(* Q8.*)

(* cord_align | Fonction auxiliaire 
    - Profile : cord_align : case -> case -> int 
    - Sémantique : La fonction cord_align prend deux cases c1 et c2 en argument 
    et retourne un entier représentant l'alignement des coordonnées entre les deux cases.
    Si les cases sont alignées sur l'axe x, y ou z, la fonction renvoie 0, 1 ou 2 respectivement.
    Si les cases ne sont alignées sur aucun axe, la fonction renvoie -1.
    -Exemples : 
    cord_align (1, 2, -3) (0, 3, -3) = 1 (alignement sur l'axe y).
    cord_align (1, 2, -3) (1, 0, -1) = 0 (alignement sur l'axe x).
    cord_align (1, 2, -3) (-6, 6, -3) = 2 (alignement sur l'axe z).
    cord_align (1, 2, -3) (0, 0, 0) = -1 (pas d'alignement).*)
let cord_align (c1: case) (c2: case) : int =
  let (i, j, k) = c1 in
  let (e, f, g) = c2 in
  if i = e then 0
  else if j = f then 1
  else if k = g then 2 
  else -1
;;

cord_align (1, 2, -3) (0, 0, 0) ;;
(*- : int = -1 *)
cord_align (1, 2, -3) (1, 0, -1) ;;
(* - : int = 0 *)


(*------------------------------------------------------------------------------------*)

(* espace_impair | Fonction auxiliaire 
    - Profile : espace_impair : case -> case -> bool
    - Sémantique : La fonction espace_impair c1 c2 vérifie si l'espace entre deux cases c1 et c2 est impair,
    en considérant l'axe sur lequel elles sont alignées.
    -Exemples : 
    espace_impair (2, 1, -3) (0, 1, -1) = true
    espace_impair (2, 1, -3) (-1, 1, 0) = false *)

let espace_impair (c1: case) (c2: case) : bool = 
  let (i, j, k) = c1 in
  let (e, f, g) = c2 in
  let alignment = cord_align c1 c2 in 
  if alignment = -1 then false
  else ((alignment = 0 && (abs(j+f)+1) mod 2 <> 0) || (alignment = 1 && (abs(i+e)+1) mod 2 <> 0) || (alignment = 2 && (abs(j+f)+1) mod 2 <> 0)) ;;

espace_impair (2, 1, -3) (0, 1, -1) ;;
(*- : bool = true*)
espace_impair (2, 1, -3) (0, 0, 0) ;;
(*- : bool = false*)


(*------------------------------------------------------------------------------------*)
  (* Fonction calcule_pivot 
    - Profile : calcul_pivot : case -> case -> case option
    - Sémantique : La fonction calcul_pivot c1 c2 calcule le pivot entre deux cases
    c1 et c2 dans le respect des règles du jeu.
    -Exemples : 
    calcul_pivot (1, 1, -2) (1, -2, 1) = None (espace pair)
    calcul_pivot (-1,-1,2) (3,3,-6) = None (Non aligner )
    calcul_pivot (1,1,-2) (1,-3,2) = Some(1,-1,0)  
    *)

let calcul_pivot (c1: case) (c2: case) : case option =
  let alignment = cord_align c1 c2 in
  let (i, j, k) = c1 in
  let (e, f, g) = c2 in
  if (espace_impair c1 c2 = false) then None
  else if alignment = 0 then
    let px = i in
    let py = (j + f) / 2 in
    let pz = (k + g) / 2 in
    Some (px, py, pz)
  else if alignment = 1 then
    let px = (i + e) / 2 in
    let py = j in
    let pz = (k + g) / 2 in
    Some (px, py, pz)
  else
    let px = (i + e) / 2 in
    let py = (j + f) / 2 in
    let pz = k in
    Some (px, py, pz)
;;


calcul_pivot (1, 1, -2) (1, -2, 1) ;;
(* - : case option = None *)
calcul_pivot (-1,-1,2) (3,3,-6) ;;
(* - : case option = None *)
calcul_pivot (1,1,-2) (1,-3,2) ;;
(* - : case option =  Some (1, -1, 0)*)

(*------------------------------------------------------------------------------------*)

(* Q9. 
    - Profile : vect_et_dist : case -> case -> vecteur * int
    - Sémantique :  La fonction vect_et_dist c1 c2 calcule le vecteur de déplacement unitaire 
    et la distance entre deux cases c1 et c2.
    -Exemples :
    vec_et_dist (-4,2,2) (-3,1,2) = ((-1, 1, 0), 1) 
    vec_et_dist (0,2,-2) (0,0,0) = ((0,1,-1),2)*)

let vec_et_dist (c1:case) (c2:case): vecteur*int = 
  let (i, j, k) = c1 in
  let (e, f, g) = c2 in
  let vi = if i>e then 1 else if i<e then -1 else 0 in
  let vj = if j>f then 1 else if j<f then -1 else 0 in 
  let vk = if k>g then 1 else if k<g then -1 else 0 in 
  let vect_unitaire = (vi,vj,vk) in
  let d = max (abs (i - e)) (max (abs (j - f)) (abs (k - g))) in
  (vect_unitaire,d) ;;

vec_et_dist (-4,2,2) (-3,1,2) ;;
(*- : vecteur * int = ((-1, 1, 0), 1) *)
vec_et_dist (0,2,-2) (0,0,0) ;;
(*- : vecteur * int = ((0, 1, -1), 2) *)


(* fin de Partie I *)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;

let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
    if m = (4 * dim) + 1 then " " (*fin de ligne*)
    else
      let c = transfo m n in
      if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
        "   "^ affiche_ligne n (m + 1) config
      else (*ceci est une case ou bien en dehors du plateau*)
       (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
    let rec affiche_aux n =
      if n = - 2 * dim - 1 then ()
      else
      begin
      print_endline (affiche_ligne n (-4*dim-1) config);
      print_endline "\n";
      affiche_aux (n - 1)
      end
    in
    affiche_aux (2*dim+1);;


let conf_1=([((0,0,0),Jaune)],[Jaune],2);;
affiche conf_1;;
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;
(* fin de Partie I *)


(*-----------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------*)

(* Partie 2 *)
(* Q10. 
    - Profil : tourner_list : 'a list -> 'a list
    - Sémantique :  La fonction tourner_list l nous donne une liste a partir de l en placant le 
    premier element a la fin de la list.
    -Exemples :
    tourner_list [e1;e2;..;en] = [e2;...;en;e1] 
    tourner_list [Vert;Jaune;Rouge] = [Jaune;Rouge;Vert] *)
let tourner_list (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | pr::fin -> fin@[pr]
;;

tourner_list [Vert;Jaune;Rouge] ;;
(* - : couleur list = [Jaune; Rouge; Vert]*)
tourner_list [1;2;3] ;;
(* - : int list = [2; 3; 1] *)

(*-----------------------------------------------------------------------------------------------------------*)

(*  
    - Profil : der_liste : 'a list -> 'a 
    - Sémantique :  La fonction der_liste l nous donne le dernier element de la list.
    -Exemples :
    der_liste [e1;e2;..;en] = en 
    der_liste [Vert;Jaune;Rouge] = Rouge *)
let rec der_liste (l : 'a list) : 'a =
  match l with 
  |[] -> failwith "Erreur : Argument negatif non autorise"
  |pr::[] -> pr
  |pr::fin -> der_liste fin
;;
der_liste [Vert;Jaune;Rouge] ;;
(* - : couleur = Rouge *)
der_liste [1;2;3] ;;
(* - : int = 3 *)

(*-----------------------------------------------------------------------------------------------------------*)

(* Q11. 
    -  equations recurrsive:
    remplir_segment m c = [c] si m=1
    remplir_segment m c = [(i,j,k)] @ remplir_segment (m-1) (i, j+1, k-1) si m >1 *)

let rec remplir_segment (m:int) (c:case) : case list =
  let (i,j,k)=c in
  match m with
  |1 -> [c]
  |m -> [(i,j,k)]@remplir_segment (m-1) (i,j+1,k-1) 
;;
remplir_segment 3 (-4,1,3) ;;
(* - : case list = [(-4, 1, 3); (-4, 2, 2); (-4, 3, 1)] *)
remplir_segment 4 (0,0,0) ;;
(* - : case list = [(0, 0, 0); (0, 1, -1); (0, 2, -2); (0, 3, -3)] *)

(*-----------------------------------------------------------------------------------------------------------*)

(* Q12.
   - equations recurrsive:
    remplir_triangle_bas m (i,j,k) = [(i,j,k)] si m=1
    remplir_triangle_bas m (i,j,k) = (remplir_segment m (i, j, k)) @ remplir_triangle_bas (m-1) (i-1, j+1, k) si m>1*)


let rec remplir_triangle_bas (m: int) ((i,j,k):case) : case list =
  if m=1 then [(i,j,k)]
  else (remplir_segment m (i, j, k)) @ remplir_triangle_bas (m-1) (i-1, j+1, k);;
  
;;

remplir_triangle_bas 3 (-4,1,3) ;;
(* - : case list = [(-4, 1, 3); (-4, 2, 2); (-4, 3, 1); (-5, 2, 3); (-5, 3, 2); (-6, 3, 3)] *)

(*-----------------------------------------------------------------------------------------------------------*)

(* Q13.
   - equations recurrsive:
    remplir_triangle_haut m (i,j,k) = [((i,j,k)] si m=1
    remplir_triangle_haut m (i,j,k) = (remplir_segment m (i, j, k)) @ remplir_triangle_haut (m-1) (i+1,j,k-1) *)
     
let rec remplir_triangle_haut (m: int) ((i,j,k):case) : case list =
  match m with
  |1 -> [(i,j,k)]
  |m -> remplir_segment m (i,j,k) @ remplir_triangle_haut (m-1) (i+1,j,k-1)
;;

remplir_triangle_haut 3 (-3, 4, -1) ;;
(* - : case list =[(-3, 4, -1); (-3, 5, -2); (-3, 6, -3); (-2, 4, -2); (-2, 5, -3);(-1, 4, -3)] *)

(*-----------------------------------------------------------------------------------------------------------*)

(* Q14. 
    - Profil : colorie : couleur -> case list -> case_oloree list
    - Sémantique :  La fonction colorie coul cl est la list formee a partir des cases de lc en leur ajoutant 
    la couleur coul.
    -Exemples :
    colorie Vert [(-6,3,3);(-5,1,4)] = [([-6,3,3),Vert);((-5,1,4),Vert)]  *)

let rec colorie (coul:couleur) (c: case list ) : case_coloree list  =
  match c with
  | [] -> []
  | pr::fin -> [(pr,coul)]@colorie coul fin
;;

colorie Vert [(-6,3,3);(-5,1,4)] ;;
(* - : case_coloree list = [((-6, 3, 3), Vert); ((-5, 1, 4), Vert)] *)
colorie Jaune [(-4,1,3);(-5,1,4)] ;;
(* - : case_coloree list = [((-4, 1, 3), Jaune); ((-5, 1, 4), Jaune)] *)

let conf_1=([((0,0,0),Jaune)],[Jaune],2);;
affiche conf_1;;
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;

(*-----------------------------------------------------------------------------------------------------------*)

(*Fonction auxiliaire pour supprimer le dernier element d'une liste*)
let rec supprime_dernier (l:'a list ) : 'a list =
  match l with
  | [] -> [] (* Cas où la liste est vide, retourne une liste vide *)
  | pr::[] -> []
  | pr::fin -> [pr]@supprime_dernier fin 
;;
supprime_dernier [(-4,1,3);(-5,1,4)] ;;
(*- : (int * int * int) list = [(-4, 1, 3)]*)
supprime_dernier [(-6,3,3);(-5,1,4)] ;;
(*- : (int * int * int) list = [(-6, 3, 3)]*)


(*Fonction auxiliaire pour renvoyer le dernier element d'une liste*)
let rec renvoie_dernier (l:'a list ) : 'a =
  match l with
  | [] -> failwith "La liste est vide" (* Cas où la liste est vide, lance une erreur *)
  | pr::[] -> pr
  | pr::fin -> renvoie_dernier fin 
;;

renvoie_dernier [(-6,3,3);(-5,1,4)] ;;
(* - : int * int * int = (-5, 1, 4) *)
renvoie_dernier [Vert;Rouge] ;;
(* - : couleur = Rouge *)


(*-----------------------------------------------------------------------------------------------------------*)

(*Q15*)
let tourner_config (ccList,coul,dim:configuration) : configuration =
  let dernier = renvoie_dernier coul in
  let coul_finale = dernier :: (supprime_dernier coul) in
  
  let rec aux_config (c : case_coloree list) : case_coloree list =
    match c with
    | [] -> []
    | (ca,c)::fin -> (tourner_case 2 ca,c)::(aux_config fin)
  in
  
  let conf_final = (renvoie_dernier (aux_config ccList)) :: (supprime_dernier (aux_config ccList)) in
  
  conf_final, coul_finale, dim
;;

conf_reggae ;;
(* - : ((int * int * int) * couleur) list * couleur list * int =
([((0, -1, 1), Vert); ((0, 0, 0), Jaune); ((0, 1, -1), Rouge)],
 [Vert; Jaune; Rouge], 1) *)
tourner_config conf_reggae ;;
(* - : configuration =
([((1, -1, 0), Rouge); ((-1, 1, 0), Vert); ((0, 0, 0), Jaune)],
 [Rouge; Vert; Jaune], 1) *)

(*-----------------------------------------------------------------------------------------------------------*)

(*Q16*)

(*fonction auxiliaire qui fait tourner toutes les cases du camp sud*)
let rec aux_case_joueur (joueurs : couleur list) (dim : dimension) : case list =
  let base = remplir_triangle_bas dim (-1*(dim+1),1,dim) in 
  List.map (fun e -> tourner_case 3 e) base ;;
;;

aux_case_joueur [Jaune;Vert] 2 ;;
(* - : case list = [(3, -1, -2); (3, -2, -1); (4, -2, -2)] *)


(*
remplir-init : liste-joueur -> dimension ->
configuration qui à partir d’une liste de joueurs et la dimension du plateau construit la
configuration initiale.
*)
let remplir_init (joueurs : couleur list) (dim : dimension) : case_coloree list =
  match joueurs with
  |[] -> []
  |h::t::q -> colorie h (remplir_triangle_bas dim (-1*(dim+1),1,dim))@(colorie t (aux_case_joueur q dim )) 
  |_ -> []
;;
remplir_init [Jaune;Rouge;Vert] 3 ;;
(*- : case_coloree list =
[((-4, 1, 3), Jaune); ((-4, 2, 2), Jaune); ((-4, 3, 1), Jaune);
 ((-5, 2, 3), Jaune); ((-5, 3, 2), Jaune); ((-6, 3, 3), Jaune);
 ((1, 3, -4), Rouge); ((2, 2, -4), Rouge); ((3, 1, -4), Rouge);
 ((2, 3, -5), Rouge); ((3, 2, -5), Rouge); ((3, 3, -6), Rouge)]
*)
(*TEST sur fonction affiche*)

affiche ((remplir_init [Jaune;Rouge] 2 ),[Jaune;Rouge],2) ;;
                                                       
(*

                            .                          


                         .     .                       


          .     .     .     .     .     R     R        


             .     .     .     .     .     R           


                .     .     .     .     .              


             .     .     .     .     .     .           


          .     .     .     .     .     .     .        


                         J     J                       


                            J                          
*)

(*-----------------------------------------------------------------------------------------------------------*)

(*Q17*)
(* quelle-couleur: case ->
configuration ->couleur qui étant donnée une case et une configuration retourne Libre 
si la case est libre, sinon la couleur du pion qui occupe cette case *)
let quelle_couleur (c:case) (config:configuration) : couleur = 
  let (lcol,col,d)= config in 
  associe c lcol Libre ;; 

quelle_couleur (0,0,0) conf_reggae ;;
(*- : couleur = Jaune*)

(*Fonction auxiliaire qui extrait les couleur d'une liste case*couleur*)

let rec extraire_col (cascol :case_coloree list): couleur list = 
  match cascol with 
  |[] -> []
  |(c,col)::fin -> col::extraire_col fin ;;


(*-----------------------------------------------------------------------------------------------------------*)

(*Q18*)

(*
supprime-dans-config: configuration -> case
-> configuration tel que supprime-dans-config conf c est la configuration conf
dans laquelle on a supprimé, dans la liste de cases colorées, la case colorée correspondant
à la case c.
*)

let supprime_dans_config (cofig:configuration) (c:case): configuration = 
  let  (cascol,col,dim) = cofig in 
  ( List.filter (fun (a,b)-> a <> c) cascol,extraire_col (List.filter (fun (a,b)-> a <> c) cascol),dim   )
;;

supprime_dans_config conf_reggae (0,0,0) ;;
(*- : configuration = ([((0, -1, 1), Vert); ((0, 1, -1), Rouge)], [Vert; Rouge], 1) *)

(*-----------------------------------------------------------------------------------------------------------*)

(*Q19*)
(*
est-coup-valide: configuration -> coup
-> bool tel que est-coup-valide conf (Du(c1,c2)) est vrai ssi le coup
(Du(c1,c2)) est valide dans la configuration conf. C’est a dire ssi les cases c1 et
c2 sont voisines, c1 contient un pion du joueur dont c’est le tour de jouer, c2 est libre et
c2 est dans le losange Nord-Sud.
*)
let est_coup_valide (confio: configuration) (c: coup): bool =
  let (l, _, _) = confio in
  match l with
  | [] -> false 
  | (cas, col) :: fin ->
    match c with
    | Sm(_) -> failwith "saut multiples non implementes"
    | Du(c1, c2) -> (sont_cases_voisines c1 c2) && (cas = c1) && not(List.exists (fun (x, y) -> x = c2) l) 
;;
est_coup_valide conf_reggae (Du((1, 0, -1) ,(1, 1, -2))) ;;
(*- : bool = false*)
est_coup_valide conf_reggae (Du((0, -1, 1) ,(-1, -1, 2))) ;;
(*- : bool = true *)

(*-----------------------------------------------------------------------------------------------------------*)

(*Q20*)
(*
appliquer-coup: configuration -> coup ->
configuration qui applique le coup (ici, un déplacement unitaire) en supposant qu’il
soit valide.
*)
let applique_coup (confio: configuration) (c: coup) : configuration =
  let (l, cols, dim) = confio in
  match l with
  | [] -> confio (* Cas où la liste est vide, retourne la configuration inchangée *)
  | (cas, col) :: fin ->
    match c with 
    | Sm(lc) -> failwith "saut multiples non implementes"
    | Du(c1, c2) -> (((c2, col) :: fin), cols, dim)
;;
 
applique_coup conf_reggae (Du((0, -1, 1) ,(-1, -1, 2))) ;;
(*- : configuration =
([((-1, -1, 2), Vert); ((0, 0, 0), Jaune); ((0, 1, -1), Rouge)],
 [Vert; Jaune; Rouge], 1) *)
applique_coup conf_reggae (Du((0, -5, 1) ,(-1, -1, 2))) ;;
(*- : configuration =
([((-1, -1, 2), Vert); ((0, 0, 0), Jaune); ((0, 1, -1), Rouge)],
 [Vert; Jaune; Rouge], 1) *)

 (*REMARQUE : Si la case n'appartient pas a la configuration ca renvoie la configuration actuelle inchangé*)
(*-----------------------------------------------------------------------------------------------------------*)

(*Q21*)
(*
mettre-a-jour-configuration:
configuration -> coup -> configuration pour quelle mette à jour le plateau pour
les coups unitaires.
*)
let mettre_a_jour_configuration (confio: configuration) (c: coup) : configuration =
  let (l, cols, dim) = confio in
  match l with
  | [] -> failwith "La configuration est vide"
  | (cas, col) :: fin ->
    match c with 
    | Sm(lc) -> failwith "Saut multiples non implementes"
    | Du(c1, c2) ->
      if est_coup_valide confio c then
        tourner_config (applique_coup confio c)
      else
        failwith "Ce coup n'est pas valide, le joueur doit rejouer"
;;
mettre_a_jour_configuration conf_reggae (Du((0, -1, 1) ,(-1, -1, 2))) ;;
(*- : configuration =
([((1, -1, 0), Rouge); ((-1, 2, -1), Vert); ((0, 0, 0), Jaune)],
 [Rouge; Vert; Jaune], 1)*)

(*Predicat qui renvoie true si la case est dans la configuration sinon false*)
let rec aux_exist (c:case) ((l,col,dim):configuration) : bool =
  match l with 
  | [] -> false
  |(c2,_)::fin ->(c2=c) || aux_exist c  (fin,col,dim) ;;

(*-----------------------------------------------------------------------------------------------------------*)

(*Q22*) 
(*
est-libre-seg : case ->case ->configuration
-> bool telle que est-libre-seg c1 c2 conf retourne true si toutes les cases
entre c1 et c2 sont libres, c1 et c2 sont supposées alignées.   
*) 
let est_libre_seg (c1:case) (c2:case) (config:configuration): bool = 
  let ((x,y,z),num) = vec_et_dist c1 c2  in
  let (i,j,k) = c2 in
  match (cord_align c1 c2 ) with
  |(-1) -> failwith "Non aligner"
  |_ -> let rec verif (num:int) ((i,j,k):case) ((x,y,z):case) (config:configuration): bool =
          match num with 
          | 1 -> not(aux_exist (i+x,j+y,k+z) config)
          | m -> not(aux_exist (i,j,k) config) && (verif (m-1) (i+x,j+y,k+z) (x,y,z) config) in
      verif num (i+x,j+y,k+z) (x,y,z) config ;;
    
est_libre_seg (0,0,0) (0,1,-1) conf_reggae ;;
(*- : bool = false*)   

(*-----------------------------------------------------------------------------------------------------------*)

(*Q23*)
(*
est-saut: case -> case -> configuration ->
bool qui retourne true si le déplacement de la première case à la seconde est un saut
valide.
*)
let est_saut (c1:case) (c2:case) (config:configuration): bool = 
  let ((x,y,z),num) = vec_et_dist c1 c2  in
  let (lc,c,dim) = config in 
  match num with 
  |2 -> not( est_libre_seg c1 c2 config ) && not(aux_exist c2 config) && est_dans_losange c2 dim 
  |_ -> false 
;;
est_saut (0,0,0) (-3,1,3) conf_reggae ;;
(*- : bool = false *)
(*-----------------------------------------------------------------------------------------------------------*)

(*Q24*)
(*
est-saut-multiple: case list ->configuration-> bool 
qui retourne true si le déplacement de la première case à la dernière est un
saut multiple valide passant par toutes les cases intermédiaires.   
*)
let rec est_saut_multiple (cl : coup) (config:configuration): bool = 
  match cl with 
  |Du(c1,c2) -> false
  |Sm([]) -> true 
  |Sm((_,_,_)::[])-> true
  |Sm((i,j,k)::(x,y,z)::q) -> ( est_saut (i,j,k) (x,y,z) config) && ( est_saut_multiple (Sm((x,y,z)::q)) config ) ;; 

est_saut_multiple (Sm([(0, 0, 0); (1, -1, 0); (2, -2, 0); (3, -3, 0)])) conf_reggae ;;
(*- : bool = false *)
est_saut_multiple (Du((0, -1, 1), (1, -1, 0))) conf_reggae ;;
(*- : bool = false*)

(*REMARQUE: Si le deplacement est unitaire le predicat renvoie false*)
(*-----------------------------------------------------------------------------------------------------------*)
let conf_test = ([(0, -1, 1),Vert ; (1, -1, 0),Rouge],[Vert;Rouge],2)

(*Q25On integre le cas des sauts multiples dans les fonctions precedantes*)
let est_coup_valide (confio: configuration) (c: coup): bool = 
  let (l, _, _) = confio in
  match l with
  | [] -> false (* Cas où la liste est vide*)
  | (cas, col) :: fin ->
    match c with
    | Du(c1, c2) -> (sont_cases_voisines c1 c2) && (cas = c1) && not(List.exists (fun (x, y) -> x = c2) l)
    | Sm(lc) -> est_saut_multiple c confio (* Appel de la fonction pour le saut multiple *)
;;
est_coup_valide conf_test (Sm([(0, -1, 1); (2, -1, -1)])) ;;
(*- : bool = true*)
est_coup_valide conf_test (Du((0, -1, 1), (2, -1, -1))) ;;
(*- : bool = false*)

(*-----------------------------------------------------------------------------------------------------------*)

let applique_coup (confio: configuration) (c: coup) : configuration = 
  let (l, cols, dim) = confio in
  match l with
  | [] -> confio (* Si la liste est vide, retourner la configuration inchangée *)
  | (cas, col) :: fin ->
    match c with 
    | Sm(lc) -> ((((renvoie_dernier lc), col) :: fin), cols, dim)
    | Du(c1, c2) -> (((c2, col) :: fin), cols, dim)
;;

applique_coup conf_test (Sm([(0, -1, 1); (2, -1, -1)])) ;;
(*- : configuration = ([((2, -1, -1), Vert); ((1, -1, 0), Rouge)], [Vert; Rouge], 2) *)

(*-----------------------------------------------------------------------------------------------------------*)

let mettre_a_jour_configuration (confio: configuration) (c: coup) : configuration = 
  let (l, cols, dim) = confio in
  match l with
  | [] -> failwith "La configuration est vide"
  | (cas, col) :: fin ->
    if est_coup_valide confio c then
      tourner_config (applique_coup confio c)
    else
      failwith "Ce coup n'est pas valide, le joueur doit rejouer"
;;

mettre_a_jour_configuration conf_test (Sm([(0, -1, 1); (2, -1, -1)])) ;;
(*- : configuration =([((-1, 0, 1), Rouge); ((-1, -1, 2), Vert)], [Rouge; Vert], 2) *)
mettre_a_jour_configuration conf_test (Du((0, -1, 1), (2, -1, -1))) ;;
(* Exception: (Failure "Ce coup n'est pas valide, le joueur doit rejouer") *)

(*-----------------------------------------------------------------------------------------------------------*)
(* 
score: configuration -> int qui retourne le score
du joueur dont c'est le tour de jouer.
REMARQUE: elle calcule le score de tout les pion du joueur
*)
let rec score ((ccl, cl, dim) : configuration) : int =
  match cl with
  | [] -> 0 (* Cas où la liste cl est vide *)
  | h::t ->
    let auxscore ((i, _, _), couleur) =
      if couleur = h then i else 0
    in
    List.fold_right (fun x acc -> acc + auxscore x) ccl 0
;;
score  ([(3, -1, 1),Vert ;(1, -1, 0),Rouge ; (3, -1, 0),Vert],[Vert;Rouge],2) ;;
(*- : int = 6 *)

(*-----------------------------------------------------------------------------------------------------------*)

(*Q27*)
(*
score-gagnant : dimension -> int qui donne le
score de la configuration gagnante pour le protagoniste, c’est à dire quand tous ses pions
sont dans le camp Nord.
*)
let score_gagnant (dim : int) : int =
  let l1 = List.init dim (fun x -> x) in
  List.fold_right (fun x acc -> acc + (x+1)*(2*dim - x)) l1 0 ;; (* Calcule le score gagnant pour une dimension donnée *)
(*Elle renvoie le score maximum qu'un joueur pourrai avoir dans une partie*)

score_gagnant 3 ;;
(*- : int = 28*)
score_gagnant 2 ;;
(*- : int = 10*)
score_gagnant 5 ;;
(*- : int = 110*) 

(*-----------------------------------------------------------------------------------------------------------*)

(* Détermine si un joueur a gagné *)
let gagne (conf:configuration) : bool =
  let (l,col,dim)=conf in
  (score conf)=(score_gagnant dim) ;; (* Vérifie si le score de la configuration est égal au score gagnant *)

gagne conf_reggae ;;
(*- : bool = false*)
(*-----------------------------------------------------------------------------------------------------------*)
(*est-partie: configuration -> coup list ->
couleur qui prend comme argument une configuration et une liste de coups et vérifie
que cette liste de coups correspond à une partie valide et retourne la couleur du gagnant.
Elle retourne Libre si aucun joueur n’a gagné*)

(* Détermine la couleur gagnante dans une configuration après une séquence de coups *)
let est_partie ((l,coll,dim):configuration) (cp:coup list) : couleur =
  let conf_apres_coup =  List.fold_left (fun (l, coll, dim) c -> mettre_a_jour_configuration (l, coll, dim) c) (l, coll, dim) cp in (* Met à jour la configuration après une séquence de coups *)
  let (l1,c1,dim2) = conf_apres_coup in

  let liste_des_score = List.map (fun ((x1,x2)) -> (score ((x1,x2)::[],c1,dim2) , x2)) l1 in (* Calcule les scores pour chaque case finale après la séquence de coups *)

  let (score_max, coul_max) =           (*Jumle les score de chaque couleur*)
    List.fold_left (fun (max_score, max_color) (score, color) ->
        if score > max_score then (score, color) else (max_score, max_color)
      ) (0, Libre) liste_des_score in (* Détermine le score maximal et la couleur correspondante *)

    if (score_max = score_gagnant dim) then coul_max else Libre (* Retourne la couleur gagnante si le score maximal est égal au score gagnant *)
;;

(*-----------------------------------------------------------------------------------------------------------*)

(* Q29 et Q30 *) 

(*
coup-possibles: configuration -> case ->
(case*coup) list qui étant données une case 𝑐 et une configuration, retourne des
couples case*coup *)
let voisins ((i, j, k) : case) (dim : dimension) : case list =
  let coords = [(i + 1, j - 1, k); (i + 1, j, k - 1); (i, j + 1, k - 1); (i - 1, j + 1, k); (i - 1, j, k + 1); (i, j - 1, k + 1)] in
  List.filter (fun (x, y, z) -> est_dans_losange (x, y, z) dim) coords 
let rec construire_chemin_saut (c1: case) (c2: case) (conf: configuration) : case list =
  if c1 = c2 then
    []
  else
    let (vec, _) = (vec_et_dist c1 c2) in
    let next_case = (translate c1 vec) in
    if next_case <> c2 then
      next_case :: (construire_chemin_saut next_case c2 conf)
    else
      [c2] 
let cases_accessibles_saut (c: case) (conf: configuration) : case list =
  let (cases_colorees, _, dim) = conf in
  let couleur_c = quelle_couleur c conf in
  if couleur_c = Libre then [] (* Si la case est libre, aucun saut n'est possible *)
  else
    let cases_valides = List.filter (fun (i, j, k) -> est_dans_losange (i, j, k) dim && est_saut c (i, j, k) conf) (voisins c dim) in
    List.filter (fun c' -> est_libre_seg c c' conf) cases_valides
 

let coup_possibles (conf : configuration) (c: case) : (case * coup) list =
  let (cases_colorees, _, dim) = conf in
  let couleur_c = quelle_couleur c conf in
  if couleur_c = Libre then [] (* Si la case est libre, aucun coup n'est possible *)
  else
    let deplacements_unitaires = List.filter_map (fun c' ->
        if est_dans_losange c' dim && sont_cases_voisines c c' && quelle_couleur c' conf = Libre then
          Some (c', Du(c, c'))
        else
          None
      ) (voisins c dim) in
    let sauts_multiples = List.filter_map (fun c' ->
        if est_dans_losange c' dim && est_saut c c' conf then
          let chemin_saut = construire_chemin_saut c c' conf in
          if chemin_saut <> [] then
            Some (c', Sm chemin_saut)
          else
            None
        else
          None
      ) (cases_accessibles_saut c conf) in
    deplacements_unitaires @ sauts_multiples
        ;;

