(setq GI 0)
(setq GB 0)
(setq GM 0)
(setq GSM 0)
(setq GP 0)
(setq GSU 0)

; Nveaix : 0, 1

(defun  setBF (Provenance Matière Genre Sociabilité  Facilité Salaire Exellence Qualité Travail1 Travail2 Polyvalent  Specialisation  Travail_Exterieur Carriere  Ambiance International *BF* GI GB GL GSM GP GSU)
  (set *BF* (list 
            (cons 'Provenance Provenance)
            (cons 'Matière_Favorite Matière)
            (cons 'Genre Genre)
             (cons 'Sociabilité Sociabilité)
             (cons 'Facilité Facilité)
             (cons 'Salaire Salaire)
             (cons 'Exellence Exellence)
             (cons 'Enseignement Qualité)
             (cons 'Travail_Deductif Travail1)
             (cons 'Travail_Par_Coeur Travail2)
             (cons 'Polyvalent Polyvalent)
             (cons 'Specialisation Specialisation)    
             (cons 'Travail_Exterieur Travail_Exterieur)
             (cons 'Carriere Carriere)
             (cons 'Ambiance Ambiance)
              (cons 'International International)
             )
       )
  )

(setq goal (list (GI GB GM GSM GSU GP)))
            
    
(defun defBF (BF)
  (print "Donnez 5 domaines auxquels vous accordez une importance particulière : 0 si NON, 1 si OUI" )
  (if (
  (set Provenance 
  )

(defun regle_valide (*BF* *BR*) 
  (let ((trouve nil)(output nil))
    (dolist (bf *BF*)
    (loop for br in *BR* while (not (equal trouve t)) do 
          (if (equal (caddr (car (assoc (car br) bf))) cadr bf) ;test si la regle corresponf au fait
              (progn
                (setq trouve nil)
                (push br output)
                (pop br)
                )
            )
          )
      (setq trouve nil)
      )
    output)
  )

(defun inférence (BF BR)  
  (let ((output nil)(valide nil)(brbuff nil))
    (setq brbuff BR)
    (setq valide (regle_valide BF brbuff))
    (loop while (and (not (equal valide nil)) (not (eqaul brbuff nil))) do
          (dolist (r (cadr(pop valide)))
            (eval r)
            )
          (when (equal valide nil)
            (setq valide (regle_valide BF brbuff))
            )
          )
    (resultat goal)
    )
  )

(defun resultat (goal)
  (let ((max (car goal)))
  (dolist (g goal)
    (when (> g max)
      (setq max g)
      )
    )
    )
  )
 
(setq *BR* '(
            ((Provenance = Tronc_Commun)((setq profil_actuel Tronc_Commun)(set math 2)(set technique 2))) ;;1
            ((Provenance = DUT)((setq profil_actuel DUT)(set math 1)(set technique 3))) ;;2
            (( Provenance = Prépa)((setq profil_actuel Prépa)(set math 3)(set technique 1))) ;;3
            ((Provenance = DEUG)((setq profil_actuel DEUG)(set math 2)(set technique 1));; 4
            ;; Règles d'informations
            
            ((Matière_Favorite = Informatique)((setq GI (+ GI 3))(setq GM (+ GM 1))(setq GB (+ GB 1))))
            ((Matière_Favorite = Programmation)(setq GI (+ GI 2)))
            ((Matière_Favorite = Biologie)(setq GB (+ GB 3))(setq GP (+ GP 1)))
            ((Matière_Favorite = Chimie)(setq GP (+ GP 3))(setq GM (+ GM 1))(setq GB (+ GB 2))) 
            ((Matière_Favorite = Physique)(setq GM (+ GM 3))(setq GP (+ GP 2)))
             
             ; regle genre = fille
             (((Genre = Fille)(Matière_Favorite = Math)(Salaire = Elevé))(setq GI (+ GI 5)))
             (((Genre = Fille)(Matière_Favorite = Biologie))(setq GB(+ GB 3)))
             (((Genre = Fille)(Sociabilité = Importante))(setq GM(+ GM 3)))
             ((Genre = Fille)(setq GB (- GB 1)))
             (((Genre = Fille)(Facilité = Importante))((setq GI (+ GI 1))))
             
             
             ;regle grenre homme
             (((Genre = Homme)(Salaire = Important))((setq GI (+ GI 3))))
             (((Genre = Homme)(Exellence = Important))((setq GI (+ GI 2))(setq GM (+ GM 3))))
             (((Genre = Homme)(Ecellence = Important))((setq GM (+ GM 2))))
             (((Genre = Homme)(Enseignement = Important))(setq GM (+ GM 1))(setq GSM (+ GSM 1)))
             ((Genre = Homme)(setq GSM (+ GSM 1)))
             ((Genre = Homme)((setq GB(- GB 2))(setq  GP(- GP 1))))
             
             ;regle generale 
             
             ((Travail_Deductif = Important)((setq GI (+ GI 2))(setq GM (+ GM 2))(setq GSM (+ GSM 2))))
             ((Travail_Par_Coeur = Important)((setq GB (+ GB 3))(setq GP (+ GP 2))))
             ((Polyvalent = Important)((setq GI (+ GI 3))(setq GB (+ GB 3))))
             ((Specialisation = Important)((setq GM (+ GM 3))(setq GSM (+ GSM 2))))
             ((Travail_Exterieur = Important)((setq GM (+ GM 1))(setq GSU (+ GSU +2))))
             ((Travail_Exterieur = bas)((setq GI (+ GI 1))(setq GB (+ GB 1))))
             ((Carriere = Bas)((setq GSM (+ GSM 1))(setq GSU (+ GSU 1))))
             ((Salaire = Important)((set GI (+ GI 3))(setq GP (+ GP 1))))
             ((Salaire = Bas)((setq GB (+ GB 3))(setq GM (+ GM 2))))
             ((Liberté = Important)((setq GSU (+ GSU 3))(setq GI (+ GI 2))))
             ((Recherche = Important)((setq GB (+ GB 1))(setq GI (+ GI 1))))
             ((Enseignement = Important)((setq GM (+ GM 1))(setq GP (+ GP 1))))
             ((Facilité = Bas)((setq GB (- GB 3))(setq GP (+ GP 1))))
             ((Ambiance = Important)((setq GSU (+ GSU 1)))
              ((International = Important)((setq GI (+ GI 2))(setq GB (+ GB 2))))
              
              
              
                                                       
                                                       
                                                       
                                                       
                                                       
             
               
