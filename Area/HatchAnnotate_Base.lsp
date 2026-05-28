(defun c:HT ()
  (vl-load-com) ; Garante que o VLA está carregado

  ;; Função para pegar a área de uma entidade
  (defun get-area (ent)
    (if (and ent (vlax-property-available-p (vlax-ename->vla-object ent) 'Area))
      (vlax-get (vlax-ename->vla-object ent) 'Area)
      nil
    )
  )

  ;; Função para formatar área sempre com duas casas decimais e vírgula
  (defun format-area-str (area)
    (if area
      (progn
        (setq inteira (fix area))
        (setq decimal (fix (* (- area inteira) 100)))
        (strcat 
          (itoa inteira) 
          "," 
          (if (< decimal 10) (strcat "0" (itoa decimal)) (itoa decimal))
        )
      )
      "0,00"
    )
  )

  ;; Função para pegar a layer da entidade
  (defun get-layer (ent)
    (cdr (assoc 8 (entget ent)))
  )

  ;; Função para pegar o estilo de texto
  (defun get-text-style ()
    (if (tblsearch "style" "ARIAL") "ARIAL" "Standard")
  )

  ;; Seleção das entidades
  (setq ent (car (entsel "\nSelecione a primeira entidade: ")))
  (setq ent2 (car (entsel "\nSelecione a segunda entidade: ")))

  ;; Ponto base para as anotações
  (setq pt (getpoint "\nClique o ponto onde inserir a anotação: "))

  ;; 1ª entidade
  (if ent
    (progn
      (setq layer (get-layer ent))
      (setq area (get-area ent))
      (setq pt2 (list (car pt) (- (cadr pt) 1.5) (caddr pt)))
      (entmake
        (list
          (cons 0 "TEXT")
          (cons 10 pt2)
          (cons 40 0.4)
          (cons 1 (strcat layer " = " (format-area-str area) " m²"))
          (cons 7 (get-text-style))
          (cons 8 layer)
        )
      )
    )
    (progn
      ;; Caso não selecione
      (setq pt2 (list (car pt) (- (cadr pt) 1.5) (caddr pt)))
      (entmake
        (list
          (cons 0 "TEXT")
          (cons 10 pt2)
          (cons 40 0.4)
          (cons 1 "ÁREA CORTE = 0,00 m²")
          (cons 7 (get-text-style))
          (cons 62 20)
        )
      )
    )
  )

  ;; 2ª entidade
  (if ent2
    (progn
      (setq layer (get-layer ent2))
      (setq area (get-area ent2))
      (setq pt2 (list (car pt) (- (cadr pt) 2.5) (caddr pt)))
      (entmake
        (list
          (cons 0 "TEXT")
          (cons 10 pt2)
          (cons 40 0.4)
          (cons 1 (strcat layer " = " (format-area-str area) " m²"))
          (cons 7 (get-text-style))
          (cons 8 layer)
        )
      )
    )
    (progn
      ;; Caso não selecione
      (setq pt2 (list (car pt) (- (cadr pt) 2.5) (caddr pt)))
      (entmake
        (list
          (cons 0 "TEXT")
          (cons 10 pt2)
          (cons 40 0.4)
          (cons 1 "ÁREA ATERRO = 0,00 m²")
          (cons 7 (get-text-style))
          (cons 62 40)
        )
      )
    )
  )
  (princ)
)