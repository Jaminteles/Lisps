(defun c:HT ( / ent ent2 ent3 pt pt2 ok)

  (vl-load-com)

  ;; ================================
  ;; Funções auxiliares
  ;; ================================

  (defun get-area (ent)
    (if (and ent (vlax-property-available-p (vlax-ename->vla-object ent) 'Area))
      (vlax-get (vlax-ename->vla-object ent) 'Area)
      nil
    )
  )

  (defun format-area-str (area)
    (if area
      (progn
        (setq inteira (fix area))
        (setq decimal (fix (* (- area inteira) 100)))
        (strcat
          (itoa inteira)
          ","
          (if (< decimal 10)
            (strcat "0" (itoa decimal))
            (itoa decimal)
          )
        )
      )
      "0,00"
    )
  )

  (defun get-layer (ent)
    (cdr (assoc 8 (entget ent)))
  )

  (defun get-text-style ()
    (if (tblsearch "style" "ARIAL") "ARIAL" "Standard")
  )

  ;; ================================
  ;; Ponto base
  ;; ================================
  (setq pt (getpoint "\nClique o ponto onde inserir as anotações: "))

  ;; ================================
  ;; 1ª SELEÇÃO — ATERRO
  ;; ================================
  (setq ok nil ent nil)
  (while (not ok)
    (setq sel (entsel "\nSelecione o HATCH de ATERRO ou ENTER para 0,00: "))
    (cond
      ((null sel) (setq ok T ent nil))
      ((= (get-layer (car sel)) "ÁREA ATERRO")
       (setq ent (car sel) ok T))
      (T (prompt "\n❌ ERRO: layer deve ser ÁREA ATERRO."))
    )
  )

  ;; ================================
  ;; 2ª SELEÇÃO — CORTE 1ª e 2ª
  ;; ================================
  (setq ok nil ent2 nil)
  (while (not ok)
    (setq sel (entsel "\nSelecione o HATCH de CORTE 1ª e 2ª ou ENTER para 0,00: "))
    (cond
      ((null sel) (setq ok T ent2 nil))
      ((= (get-layer (car sel)) "ÁREA CORTE 1ª e 2ª Cat.")
       (setq ent2 (car sel) ok T))
      (T (prompt "\n❌ ERRO: layer deve ser ÁREA CORTE 1ª e 2ª Cat."))
    )
  )

  ;; ================================
  ;; 3ª SELEÇÃO — CORTE 3ª
  ;; ================================
  (setq ok nil ent3 nil)
  (while (not ok)
    (setq sel (entsel "\nSelecione o HATCH de CORTE 3ª ou ENTER para 0,00: "))
    (cond
      ((null sel) (setq ok T ent3 nil))
      ((= (get-layer (car sel)) "ÁREA CORTE 3ª Cat.")
       (setq ent3 (car sel) ok T))
      (T (prompt "\n❌ ERRO: layer deve ser ÁREA CORTE 3ª Cat."))
    )
  )

  ;; ================================
  ;; INSERÇÃO DOS TEXTOS
  ;; ================================

  ;; ATERRO — cor 40
  (setq pt2 (list (car pt) (- (cadr pt) 1.5) (caddr pt)))
  (entmake
    (list
      (cons 0 "TEXT")
      (cons 10 pt2)
      (cons 40 0.4)
      (cons 1 (strcat "ÁREA ATERRO = " (format-area-str (get-area ent)) " m²"))
      (cons 7 (get-text-style))
      (cons 62 40)
    )
  )

  ;; CORTE 1ª e 2ª — cor 20
  (setq pt2 (list (car pt) (- (cadr pt) 2.5) (caddr pt)))
  (entmake
    (list
      (cons 0 "TEXT")
      (cons 10 pt2)
      (cons 40 0.4)
      (cons 1
        (strcat
          "ÁREA CORTE 1ª e 2ª Cat. = "
          (format-area-str (get-area ent2))
          " m²"
        )
      )
      (cons 7 (get-text-style))
      (cons 62 20)
    )
  )

  ;; CORTE 3ª — cor 3
  (setq pt2 (list (car pt) (- (cadr pt) 3.5) (caddr pt)))
  (entmake
    (list
      (cons 0 "TEXT")
      (cons 10 pt2)
      (cons 40 0.4)
      (cons 1
        (strcat
          "ÁREA CORTE 3ª Cat. = "
          (format-area-str (get-area ent3))
          " m²"
        )
      )
      (cons 7 (get-text-style))
      (cons 62 22)
    )
  )

  (princ)
)
