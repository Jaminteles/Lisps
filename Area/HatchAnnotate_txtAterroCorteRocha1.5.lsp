(defun c:HTxt ( / estEnt estaca
               ent ent2 ent3
               area1 area2 area3
               pt pt2 ok
               arq path linha)

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
  
  (defun get-layer (ent)
    (cdr (assoc 8 (entget ent)))
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

  (defun get-text-value (ent)
    (cond
      ((= (cdr (assoc 0 (entget ent))) "TEXT")
       (cdr (assoc 1 (entget ent))))
      ((= (cdr (assoc 0 (entget ent))) "MTEXT")
       (cdr (assoc 1 (entget ent))))
      (T "")
    )
  )

  (defun get-text-style ()
    (if (tblsearch "style" "ARIAL") "ARIAL" "Standard")
  )

  ;; ================================
  ;; SELEÇÃO DA ESTACA
  ;; ================================
  (setq estEnt nil)
  (while (null estEnt)
    (setq sel (entsel "\nSelecione o TEXTO da ESTACA: "))
    (if sel
      (setq estEnt (car sel))
      (prompt "\n❌ Selecione um texto válido.")
    )
  )

  (setq estaca (get-text-value estEnt))

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
  ;; ÁREAS FORMATADAS
  ;; ================================
  (setq area1 (format-area-str (get-area ent)))
  (setq area2 (format-area-str (get-area ent2)))
  (setq area3 (format-area-str (get-area ent3)))

  ;; ================================
  ;; CRIA TEXTOS NO DESENHO
  ;; ================================

  ;; ATERRO
  (setq pt2 (list (car pt) (- (cadr pt) 1.5) (caddr pt)))
  (entmake
    (list
      '(0 . "TEXT")
      (cons 10 pt2)
      '(40 . 0.4)
      (cons 1 (strcat "ÁREA ATERRO = " area1 " m²"))
      (cons 7 (get-text-style))
      '(62 . 40)
    )
  )

  ;; CORTE 1ª e 2ª
  (setq pt2 (list (car pt) (- (cadr pt) 2.5) (caddr pt)))
  (entmake
    (list
      '(0 . "TEXT")
      (cons 10 pt2)
      '(40 . 0.4)
      (cons 1 (strcat "ÁREA CORTE 1ª e 2ª Cat. = " area2 " m²"))
      (cons 7 (get-text-style))
      '(62 . 20)
    )
  )

  ;; CORTE 3ª 
  (setq pt2
    (list
      (+ (car pt) 10.5)   ; deslocamento horizontal
      (- (cadr pt) 2.5) ; mesma linha do CORTE 1ª e 2ª
      (caddr pt)
    )
  )

  (entmake
    (list
      '(0 . "TEXT")
      (cons 10 pt2)
      '(40 . 0.4)
      (cons 1 (strcat "ÁREA CORTE 3ª Cat. = " area3 " m²"))
      (cons 7 (get-text-style))
      '(62 . 22)
    )
  )


  ;; ================================
  ;; GRAVA ARQUIVO TXT (APPEND)
  ;; ================================
  (setq path (strcat (getvar "DWGPREFIX") "areas_estacas.txt"))
  (setq arq (open path "a"))

  (setq linha
    (strcat
      estaca ";" area1 ";" area2 ";" area3
    )
  )

  (write-line linha arq)
  (close arq)

  (prompt (strcat "\n✔ Linha gravada em: " path))
  (princ)
)
