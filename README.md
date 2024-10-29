Projet de Jeu Hexagonal - README
Description
Ce projet implémente un jeu hexagonal à plusieurs joueurs en OCaml. Le jeu se déroule sur un plateau en forme de losange où les joueurs doivent déplacer leurs pièces en suivant certaines règles de déplacement et de capture, en respectant des configurations de cases et de couleurs.

Les principales fonctionnalités du jeu incluent la création de configurations initiales, le déplacement des pièces en fonction des règles de coup valides et invalides, et le suivi des configurations mises à jour au fil du jeu.

Structure des Fichiers
Le projet est divisé en plusieurs sections fonctionnelles, chacune correspondant aux exigences de différentes questions posées dans le code.

Fonctions principales
Types de données :

couleur, case, case_coloree, configuration, et coup sont des types de données utilisés pour modéliser les éléments du jeu (cases, configurations, couleurs, coups possibles).
Fonctions d'initialisation :

remplir_triangle_bas et remplir_init permettent d'initialiser les configurations de base, avec des cases de couleur spécifiques pour chaque joueur.
Manipulation de configurations :

tourner_case, tourner_config, et colorie effectuent des transformations et des manipulations sur les configurations de cases et de couleurs.
Fonctions auxiliaires :

Fonctions comme supprime_dernier, renvoie_dernier, et associe facilitent la gestion des listes et des configurations.
Règles de déplacement et de capture :

est_coup_valide, applique_coup, et mettre_a_jour_configuration vérifient la validité des coups et mettent à jour la configuration en conséquence.
est_saut et est_saut_multiple vérifient les sauts valides, y compris les sauts multiples, en fonction de la configuration actuelle.
Instructions d'utilisation
Pour exécuter ce projet, OCaml doit être installé sur votre machine. Utilisez l'interpréteur OCaml pour charger les fonctions et tester les différents exemples fournis :

ocaml
Copier le code
#use "PROJET.ml";;
Exemples d'utilisation
Initialisation :

ocaml
Copier le code
let conf_1 = ([((0,0,0),Jaune)], [Jaune], 2);;
affiche conf_1;;
Vérification de coup valide :

ocaml
Copier le code
let conf_reggae = ([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)], [Vert; Jaune; Rouge], 1);;
est_coup_valide conf_reggae (Du((1, 0, -1), (1, 1, -2)));;
Mise à jour de configuration :

ocaml
Copier le code
let nouvelle_config = mettre_a_jour_configuration conf_reggae (Du((0, -1, 1), (-1, -1, 2)));;
affiche nouvelle_config;;
