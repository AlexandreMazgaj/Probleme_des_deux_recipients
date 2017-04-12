; action 1 : remplir R1
; action 2 : remplir R2
; action 3 : vider R1
; action 4 : vider R2
; action 5 : transvaser la toltalité de R2 dans R1
; action 6 : transvaser la toltalité de R1 dans R2
; action 7 : transvaser une partie de R2 dans R1 (R1 est rempli)
; action 8 : transvaser une partie de R1 dans R2 (R2 est rempli)


(defun actions (etat) 
  (let ((action nil))
   (if (< (car etat) 4) (push 1 action))
   (if (< (cadr etat) 3) (push 2 action))
   (if (> (car etat) 0) (push 3 action))
   (if (> (cadr etat) 0) (push 4 action))
   (if (and (<= (+ (car etat) (cadr etat)) 4) (> (cadr etat) 0))  (push 5 action))
   (if (and (<= (+ (car etat) (cadr etat)) 3) (> (car etat) 0)) (push 6 action))
   (if (and (> (+ (car etat) (cadr etat)) 4) (< (car etat) 4)) (push 7 action))
   (if (and (> (+ (car etat) (cadr etat)) 3) (< (cadr etat) 3)) (push 8 action))
   action))

(defun resultat-action (etat action)
  (cond
   ((= action 1) (list 4 (cadr etat)))
   ((= action 2) (list (car etat) 3))
   ((= action 3) (list 0 (cadr etat)))
   ((= action 4) (list (car etat) 0))
   ((= action 5) (list (+ (car etat) (cadr etat)) 0))
   ((= action 6) (list 0 (+ (car etat) (cadr etat))))
   ((= action 7) (list 4 (- (cadr etat) (- 4 (car etat)))))
   ((= action 8) (list (- (car etat) (- 3 (cadr etat))) 3))
    ))

(defun successeurs(etat etatsVisites)
  (let ((listeEtatsSuivants nil) 
        (listeAction (mapcar #' (lambda(x) (resultat-action etat x)) (actions etat))))
    
    (dolist (elem listeAction listeEtatsSuivants)
      (if (not (member elem etatsVisites :test #'equal))
          (push elem listeEtatsSuivants)
        ))
    ))


(defun rech-prof1(etat etatsVisites numeroEtape) 
  (let ((etatsSuivants (successeurs etat etatsVisites)))
    (push etat etatsVisites)
    (cond
     ((equal etatsSuivants nil) 
      (progn
        (format t " ->Plus d'états non visités~%~%----FIN DU PARCOURS DE CETTE SOUS-BRANCHE----~%~%")
        nil))
     ((= (car etat) 2) (list etat))
     (T 
      (while (not (equal etatsSuivants nil))
        (format t "~&Etape ~a :Aller de ~a à ~a" numeroEtape etat (car etatsSuivants))
        (setq sol (rech-prof1 (car etatsSuivants) etatsVisites (+ numeroEtape 1)))
        (if (equal sol nil) 
              (pop etatsSuivants)
          (progn 
            (format t " -> Etat solution trouvé~%~%----FIN DU PARCOURS DE CETTE BRANCHE----~%~%" numeroEtape)
            (push etat sol)
            (pop etatsSuivants)
            )))
      sol))))

(defun rech-prof2(etat etatsVisites) 
  (let ((etatsSuivants (successeurs etat etatsVisites)))
    (push etat etatsVisites)
    (cond
     ((equal etatsSuivants nil) nil)
     ((= (car etat) 2) (format t "~&solution : ~a" (reverse etatsVisites)))
     (T 
      (while (not (equal etatsSuivants nil))
        (setq sol (rech-prof2 (car etatsSuivants) etatsVisites))
        (pop etatsSuivants)
            )))))

(defun rech-prof(etat)
  (defparameter etatsVisites nil)
  (defparameter sol nil)
  (defparameter numeroEtape 1)
  
  (rech-prof2 etat etatsVisites))
  
;;(RECH-PROF '(0 0)) 


 

;-------------------------------------------------------------------------
; Recherche en largeur

(defvar *start* '(0 0))


(defun bfs (etat)
  (defparameter etatsVisites nil)
  (defparameter sol nil)
  (bfs1 (list (list etat)))
  sol)

(defun bfs1 (queue)
  (let ((etatsSuivants (successeurs (caar queue) etatsVisites)))
        (push (caar queue) etatsVisites)
    (cond
	((null queue) nil)
        ((= (caaar queue) 2) (push queue sol))
     (T
	     (bfs1
		 (append
		     (cdr queue)
		     (mapcar
			 #'(lambda (etat)
			       (cons etat (car queue)))
         etatsSuivants)))))))

;(bfs *start*)
