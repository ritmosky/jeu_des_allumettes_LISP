

;;;;; TP02 : Jeu des allumettes ;;;;;



;;;;; Q1 ;;;;; Liste des états possibles pour 11 allumettes



; pour un nombre d'allumettes donné il y'a 2 joueur qui peuvent agir selon les cas


(setq etatsP '( (11 J2) (11 J1) (10 J2) (10 J1) (9 J2) (9 J1) 
                (8 J2)  (8 J1)  (7 J2)  (7 J1)  (6 J2) (6 J1) 
                (5 J2)  (5 J1)  (4 J2)  (4 J1)  (3 J2) (3 J1)  
                (2 J2)  (2 J1)  (1 J2)  (1 J1)  (0 J2) (0 J1) )   )





;;;;; Q2 ;;;;; Listes des opérateurs 


; (J x) -> joueur J retire x allumette
; pour un etat courant (dim joueur) chaque joueur peut enlever jusqu'à 3 allumettesles 
; donc les opérateurs possibles sont ( (joueur 3) (joueur 2) (joueur 1) ) 




(setq operateursP '( (J2 3) (J1 3) 
                    (J2 2) (J1 2) 
                    (J2 1) (J1 1) )   )




;;;;; Q3 ;;;;; Les états



;; Etat Initial = (11 J1) 



;; Etats Finaux = (1 J1) (1 J2) (2 J1) (2 J2) (3 J1) (3 J2)




;;;;; Q5 ;;;;; fonctions de résolution selon parcours en profondeur ou en largeur d’abord




(defun creer_etat(dim J)
  (if (> dim 0)  
      (list dim J) ))     


; (creer_etat 4 'j1) -> (4 J1)
; (creer_etat 0 'j2) -> nil car dans notre modélisation la dimension 0 n'est jamais atteinte



(defun creer_operateur(dim J)
  (reverse (creer_etat dim J))   )


; (creer_operateur 1 'j2) -> (J2 1)
; (creer_operateur 0 'j2) -> nil car il faut retirer au moins 1 allumette



(defun joueur (etat)
  (cadr etat))


; (joueur '(6 j2)) -> J2



(defun dimension (etat)
  (car etat) )


; (dimension '(8 j1)) -> 8



(defun fin?(etat)
  (if  (or (equal (dimension etat) 3) 
           (equal (dimension etat) 2)           
           (equal (dimension etat) 1) )     
      t) )  
      

; (fin? '(8 j2)) -> nil
; (fin? '(3 j2)) -> T
; (fin? '(2 j2)) -> T
; (fin? '(1 j2)) -> T



(defun successeurs(etat)
  
  (let* ( (J (joueur etat))
         (dim (dimension etat)) 
         (ls ()) )   
  (cond
   ( (equal J 'J1) (setq J 'J2) )
   ( (equal J 'J2) (setq J 'J1) ))
    
   (if (> dim 3)
       (progn    
         (setq ls (list (creer_etat (- dim 1) J)) )    
         (dolist (i '(2 3) ls)
           (nconc ls (list (creer_etat (- dim i) J) ))))) ls)  )


; (successeurs '(4 j1)) -> ((3 J2) (2 J2) (1 J2))
; (successeurs '(3 j1)) -> nil



(defun successeurs_valides(etat etats_visites)  
  (let ( (lv (list ()))  )
    
    (dolist (x (successeurs etat))
      (if (equal x (car etats_visites))
        (progn
        (setq etats_visites (cdr etats_visites))  
        (successeurs_valides etat (cdr etats_visites)))
     
        (nconc lv (list x))  ))
    (cdr lv))   ) 


; (successeurs_valides '(3 j1) ()) -> nil
; (successeurs_valides '(5 j1) '((4 j2))) -> ((3 J2) (2 J2)) à titre d'exemple
; (successeurs_valides '(5 j1) '(6 j2)) -> ((4 J2) (3 J2) (2 J2))



(defun coup_effectue(etat1 etat2)
  (let* ((pioche (- (dimension etat1) (dimension etat2))) )
    
  (cond 
   ( (and (< pioche 4) (> pioche 0) (not (equal (joueur etat1) (joueur etat2))))
    (format t "~%~6@t ETAT = ~s | ~s a retiré ~s allumettes" etat1 (joueur etat1) pioche))
    
   ( (> pioche 3)
    (format t "~%~6@t ERREUR ~s a retiré plus DE 3 allumettes " (joueur etat1)))
   
   ( (< pioche 1)
    (format t "~%~6@t ERREUR ~s doit retiré au moins 1 allumette " (joueur etat1)))
   
   ( (equal (joueur etat1) (joueur etat2))
    (format t "~%~6@t ERREUR DE JOUEUR DANS L'ETAT ~s " etat2))) )  )


; (coup_effectue '(4 j1) '(2 j2)) -> ETAT = (4 J1) | J1 a retiré 2 allumettes
; (coup_effectue '(4 j1) '(2 j1)) -> ERREUR DE JOUEUR DANS L'ETAT (2 J1)
; (coup_effectue '(5 j2) '(5 j1)) -> ERREUR J2 doit retiré au moins 1 allumette
; (coup_effectue '(5 j2) '(1 j1)) -> ERREUR J2 a retiré plus DE 3 allumettes 



;; Q5 ;; PARCOURS



;; explore_profondeur renvoie tous les chemins parcourus menant directement à la victoire de j


;; On affiche l'état courant
;; On ajoute l'état courant dans chemin puis on l'affiche
;; si à la fin on a trouvé la solution cad un état avec 3, 2 ou 1 allumettes au tour de j, on renvoie le chemin
;; sinon
;;      suivants = successeurs valides
;;      pour chaque successeur valide
;;            on affiche l'opérateur qui amène au successeur 
;;            on rappelle la fonction explore_profondeur




(defun explore_profondeur (etat j chemin)
  (let ((suivants nil))
    
    (format t "~%~6@t Explore etat ~s | chemin = ~s~%" etat chemin)
    (setq chemin (append (list etat) chemin))  
    (format t "~6@t Ajout etat ~s dans chemin = ~s~%" etat chemin)   
    
    (if (and (fin? etat) (equal (joueur etat) j))
        (format t "/////~t ~s gagne | Chemin possible = ~s ~t/////~%" j chemin)
      (progn  
        (setq suivants (append suivants (successeurs_valides etat chemin)) )   
        (dolist (x suivants 'fin)
          (coup_effectue etat x)
          (explore_profondeur x j chemin)) ) ) ))



; affichage solution 
; (explore_profondeur '(5 j2) 'j1 '((6 j2))) -> /////  J1 gagne | Chemin possible = ((3 J1) (5 J2) (6 J2))  /////
; (explore_profondeur '(5 j2) 'j1 '((6 j2))) -> /////  J1 gagne | Chemin possible = ((2 J1) (5 J2) (6 J2))  /////
; (explore_profondeur '(4 j2) 'j2 '((6 j2))) -> pas de solution



;; explore_largeur (etats j chemin)
;; 
;;  pour chaque x dans etats
;;     ajouter x à chemin
;;     afficher successeurs de x 
;;     pour chaque successeur y de x 
;;          si y est un etat final pour lequel j gagne
;;                on ajoute y dans chemin
;;          




(defun explore_largeur (etats j chemin)
  
  (let ((suivants nil))
    (format t "~%~6@t Explore etats ~s | chemin = ~s~%" etats chemin)
    
    (dolist (x etats 'fin)
      (setq chemin (append (list x) chemin))
      
      (format t "~6@t Ajout etats ~s dans chemin = ~s~%" x chemin) 
      (setq suivants (append suivants (successeurs_valides x chemin)))
      
      (format t "~6@t suivants = ~s~%" suivants)     
      (dolist (y suivants)
        
        (format t "~%~6@t Explore etats ~s | chemin = ~s~%" y chemin)
        (if (and (fin? y) (equal j (joueur y)))
            (progn
              (setq chemin (append (list y) chemin))
              (format t "~6@t Ajout etats ~s dans chemin = ~s~%" y chemin)
              (format t "/////~t ~s gagne | Chemin possible = ~s ~t/////~%" j chemin))
              nil)    ))))  


  
; (explore_largeur '((4 j2)) 'j1 '()) -> 
; /////  J1 gagne | Chemin possible = ((3 J1) (4 J2))  /////
; /////  J1 gagne | Chemin possible = ((2 J1) (3 J1) (4 J2))  /////
; /////  J1 gagne | Chemin possible = ((1 J1) (2 J1) (3 J1) (4 J2))  /////


