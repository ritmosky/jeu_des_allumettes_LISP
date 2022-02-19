# jeu_des_allumettes_lisp

Ce TP permet d’illustrer comment on peut représenter un problème comme un espace d’états et sa résolution comme une recherche dans cet espace.

Ce jeu se joue à deux. La situation de départ est un nombre donné d’allumettes (ce nombre sera
appelé la dimension du jeu), associé à l’indication du joueur qui doit commencer le jeu. Chacun
des joueurs retire alors à tour de rôle 1, 2 ou 3 allumettes parmi les allumettes encore
disponibles. Le joueur qui gagne est celui qui réussit à retirer la (ou les) dernière(s) allumette(s).

Chaque état est représenté par deux variables représentant le nombre d’allumettes disponibles et le joueur qui a la main.
