### Projet de Jeu Hexagonal

#### Description

Ce projet implémente un jeu hexagonal à plusieurs joueurs en **OCaml**. Le jeu se déroule sur un plateau en forme de losange où les joueurs déplacent leurs pièces en suivant des règles de déplacement et de capture, en respectant des configurations de cases et de couleurs.

Les principales fonctionnalités du jeu incluent :
- La création de configurations initiales.
- Le déplacement des pièces selon les règles de coups valides et invalides.
- Le suivi des configurations mises à jour au fil du jeu.

#### Structure des Fichiers

Le projet est divisé en plusieurs sections fonctionnelles, chacune correspondant aux exigences de différentes étapes du code.

#### Fonctions principales

1. **Types de données** :
    - `couleur`, `case`, `case_coloree`, `configuration`, et `coup` sont des types de données utilisés pour modéliser les éléments du jeu (cases, configurations, couleurs, coups possibles).

2. **Fonctions d'initialisation** :
    - `remplir_triangle_bas` et `remplir_init` : permettent d'initialiser les configurations de base, avec des cases de couleur spécifiques pour chaque joueur.

3. **Manipulation de configurations** :
    - `tourner_case`, `tourner_config`, et `colorie` : effectuent des transformations et des manipulations sur les configurations de cases et de couleurs.

4. **Fonctions auxiliaires** :
    - `supprime_dernier`, `renvoie_dernier`, et `associe` : facilitent la gestion des listes et des configurations.

5. **Règles de déplacement et de capture** :
    - `est_coup_valide`, `applique_coup`, et `mettre_a_jour_configuration` : vérifient la validité des coups et mettent à jour la configuration en conséquence.
    - `est_saut` et `est_saut_multiple` : vérifient les sauts valides, y compris les sauts multiples, en fonction de la configuration actuelle.

#### Instructions d'utilisation

Pour exécuter ce projet, **OCaml** doit être installé sur votre machine. Utilisez l'interpréteur OCaml pour charger les fonctions et tester les différents exemples fournis :

```ocaml
#use "PROJET.ml";;
```
#### Exemples d'utilisation

- **Initialisation de la configuration** :
    ```ocaml
    let conf_1 = ([((0,0,0), Jaune)], [Jaune], 2);;
    affiche conf_1;;
    ```

- **Vérification de la validité d'un coup** :
    ```ocaml
    let conf_reggae = ([((0,-1,1), Vert); ((0,0,0), Jaune); ((0,1,-1), Rouge)], [Vert; Jaune; Rouge], 1);;
    est_coup_valide conf_reggae (Du((1, 0, -1), (1, 1, -2)));;
    ```

- **Mise à jour de la configuration après un coup** :
    ```ocaml
    let nouvelle_config = mettre_a_jour_configuration conf_reggae (Du((0, -1, 1), (-1, -1, 2)));;
    affiche nouvelle_config;;
    ```

# Auteurs
Racim ZENATI & Arris ZAIDI
