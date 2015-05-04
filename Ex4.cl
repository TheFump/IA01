(setq BaseTest '((Dupond Pierre Lyon 45 150)
(Dupond Marie Nice 32 200)
(Dupond Jacques Lyon 69 20)
(Perrot Jacques Geneve 28 500)
(Perrot Jean Nice 55 60)
(Perrot Anna Grenoble 19 180)
))

(defun nom (personne)
  (car personne)
  )

(defun prenom (personne)
  (cadr personne)
  )

(defun ville (personne)
  (caddr personne)
  )u


(defun age (personne)
  (cadddr personne)
  )

(defun nombre-livres (personne)
  (car (cddddr personne))
  )

(defun f1 (bdd)
  (mapcar #'(lambda (l)
               l
              )bdd
    )
  )


(defun f2 (bdd)
  (cond 
   ((eq bdd nil)nil)
   ((equal (nom (car bdd)) 'perrot) (print (car bdd)) (f2 (cdr bdd)))
   (T (printallperrot2 (cdr bdd)) )
)
  )

(defun f2bis (bdd)
  (dolist(l bdd "fin")(when(equal (nom l) 'perrot) (print l))))


(defun f3 (l n)
  (if (null l)
      nil
    (if (equal (nom (car l)) n)
        (append (list (car l)) (f3 (cdr l) n))
      (f3 (cdr l) n)
      )
    )
  )

(defun f3bis (bdd n)
  (dolist (l bdd "fin")(when (equal (nom l) n) (print l))))


(defun f4 (l n)
   (if (null l)
      nil
    (if (equal (age (car l)) n)
         (car l)
      (f4 (cdr l) n)
      )
    )
  )

(defun f4bis (bdd n)
  (dolist (l bdd nil)(when (equal (age l) n) (return l))))

(defun f5 (l)
  (if (null l)
   nil
    (if ( < (nombre-livres (car l)) 100) 
        (car l)
    (less100books (cdr l)) 
    )
   )
  )

(defun f5bis (bdd)
  (dolist (l bdd nil)(when( < (nombre-livres l) 100) (return l))))

(setq moyenne 0)
(setq compteur 0)


     
(defun f6bis (bdd name compteur moyenne)
  (if (null bdd)
  (setq moyenne (/ moyenne compteur))
    (if (equal (nom (car bdd)) name)
        (progn  (setq compteur (+ compteur 1)) (setq moyenne ( + moyenne (age (car bdd)))) (f6 (cdr bdd) name compteur moyenne) )
        (progn (f6 (cdr bdd) name compteur moyenne) nil)
      )
    )  moyenne
  )

(defun f6 (bdd name compteur moyenne)
  (dolist (l bdd "fin") (when (equal (nom l) name)
        (progn  (setq compteur (+ compteur 1)) (setq moyenne ( + moyenne (age l))) )
                        
                          )
    )  (setq moyenne (/ moyenne compteur))
  )
