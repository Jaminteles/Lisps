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
  ;; SELEÇÃO AUTOMÁTICA (ATERRO / CORTE)
  ;; ================================
  (setq ent nil ent2 nil)

  (prompt "\nSelecione os HATCHES (Aterro/Corte). ENTER para finalizar.")

  (while (setq sel (entsel "\nClique no hatch: "))
    (setq obj (car sel))
    (setq layer (get-layer obj))

    (cond
      ((= layer "Aterro")
      (setq ent obj)
      (prompt "\n✔ Aterro identificado."))
      
      ((= layer "Corte")
      (setq ent2 obj)
      (prompt "\n✔ Corte identificado."))
      
      (T
      (prompt "\n❌ Layer inválido (use Aterro ou Corte)."))
    )
  )

  ;; ================================
  ;; ÁREAS FORMATADAS
  ;; ================================
  (setq area1 (format-area-str (get-area ent)))
  (setq area2 (format-area-str (get-area ent2)))
  
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
      (cons 1 (strcat "ÁREA CORTE = " area2 " m²"))
      (cons 7 (get-text-style))
      '(62 . 20)
    )
  )

  ;; ================================
  ;; GRAVA ARQUIVO TXT (APPEND)
  ;; ================================
  (setq path (strcat (getvar "DWGPREFIX") "areas_estacas.txt"))
  (setq arq (open path "a"))

  (setq linha
    (strcat
      estaca ";" area1 ";" area2 ";"
    )
  )

  (write-line linha arq)
  (close arq)

  (prompt (strcat "\n✔ Linha gravada em: " path))
  (princ)
)
