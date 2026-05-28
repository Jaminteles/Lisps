(defun c:PONTOS_ACESSO_LV_EIXO
 ( / ss i obj pts eixo p lastPt rawDesc)

  (vl-load-com)

  (princ "\nSelecione os COGO Points do eixo...")
  (setq ss (ssget '((0 . "AECC_COGO_POINT"))))

  (if (null ss)
    (progn (princ "\nNada selecionado.") (princ) (return))
  )

  ;; coleta SOMENTE pontos com RawDescription = "EIX"
  (setq pts '())
  (setq i 0)
  (while (< i (sslength ss))
    (setq obj (vlax-ename->vla-object (ssname ss i)))
    (setq rawDesc (vlax-get obj 'RawDescription))

    (if (= (strcase rawDesc) "EIX")
      (setq pts
        (append pts
          (list
            (list
              (vlax-get obj 'Easting)
              (vlax-get obj 'Northing)
              (vlax-get obj 'Elevation)
            )
          )
        )
      )
    )

    (setq i (1+ i))
  )

  (if (< (length pts) 2)
    (progn
      (princ "\nPontos EIX insuficientes.")
      (princ)
      (return)
    )
  )

  ;; ordena por avanço
  (setq pts (vl-sort pts '(lambda (a b) (< (cadr a) (cadr b)))))

  ;; monta eixo direto (sem pular pontos)
  (setq eixo '())
  (setq lastPt nil)

  (foreach p pts
    (if (or (null lastPt) (<= (dist2d lastPt p) 25.0))
      (progn
        (setq eixo (append eixo (list p)))
        (setq lastPt p)
      )
      (progn
        (princ "\nEixo cortado (distância > 25m).")
        (setq lastPt nil)
      )
    )
  )

  ;; cria polyline
  (if (> (length eixo) 1)
    (progn
      (command "_.3DPOLY")
      (foreach p eixo (command p))
      (command "")
      (princ "\nEixo criado corretamente (EIX filtrado).")
    )
    (princ "\nFalha ao criar eixo.")
  )

  (princ)
)







(defun dist2d (p1 p2)
  (distance
    (list (car p1) (cadr p1))
    (list (car p2) (cadr p2))
  )
)







(defun c:PONTOS_ACESSO_LV_ESQ
 ( / ss i obj pts grp secoes p bordo)

  (vl-load-com)

  (princ "\nSelecione os COGO Points do acesso...")
  (setq ss (ssget '((0 . "AECC_COGO_POINT"))))

  (if (or (null ss) (< (sslength ss) 2))
    (progn (princ "\nPontos insuficientes.") (princ) (return))
  )

  ;; coleta pontos
  (setq pts '())
  (setq i 0)
  (while (< i (sslength ss))
    (setq obj (vlax-ename->vla-object (ssname ss i)))
    (setq pts
      (append pts
        (list
          (list
            (vlax-get obj 'Easting)
            (vlax-get obj 'Northing)
            (vlax-get obj 'Elevation)
          )
        )
      )
    )
    (setq i (1+ i))
  )

  ;; ordena por avanço
  (setq pts (vl-sort pts '(lambda (a b) (< (cadr a) (cadr b)))))

  ;; cria seções de 2 pontos
  (setq secoes '() grp '())

  (foreach p pts
    (setq grp (append grp (list p)))
    (if (= (length grp) 2)
      (progn
        (setq secoes (append secoes (list grp)))
        (setq grp '())
      )
    )
  )

  ;; monta bordo esquerdo
  (setq bordo '())

  (foreach grp secoes
    (setq grp (vl-sort grp '(lambda (a b) (< (car a) (car b)))))
    (setq bordo (append bordo (list (car grp))))
  )

  ;; cria polyline
  (if (> (length bordo) 1)
    (progn
      (command "_.3DPOLY")
      (foreach p bordo (command p))
      (command "")
      (princ "\nBordo esquerdo criado corretamente.")
    )
    (princ "\nFalha ao criar bordo esquerdo.")
  )

  (princ)
)







(defun c:PONTOS_ACESSO_LV_DIR
 ( / ss i obj pts grp secoes p bordo)

  (vl-load-com)

  (princ "\nSelecione os COGO Points do acesso...")
  (setq ss (ssget '((0 . "AECC_COGO_POINT"))))

  (if (or (null ss) (< (sslength ss) 2))
    (progn (princ "\nPontos insuficientes.") (princ) (return))
  )

  ;; coleta pontos
  (setq pts '())
  (setq i 0)
  (while (< i (sslength ss))
    (setq obj (vlax-ename->vla-object (ssname ss i)))
    (setq pts
      (append pts
        (list
          (list
            (vlax-get obj 'Easting)
            (vlax-get obj 'Northing)
            (vlax-get obj 'Elevation)
          )
        )
      )
    )
    (setq i (1+ i))
  )

  ;; ordena por avanço
  (setq pts (vl-sort pts '(lambda (a b) (< (cadr a) (cadr b)))))

  ;; cria seções de 2 pontos
  (setq secoes '() grp '())

  (foreach p pts
    (setq grp (append grp (list p)))
    (if (= (length grp) 2)
      (progn
        (setq secoes (append secoes (list grp)))
        (setq grp '())
      )
    )
  )

  ;; monta bordo direito
  (setq bordo '())

  (foreach grp secoes
    (setq grp (vl-sort grp '(lambda (a b) (< (car a) (car b)))))
    (setq bordo (append bordo (list (cadr grp))))
  )

  ;; cria polyline
  (if (> (length bordo) 1)
    (progn
      (command "_.3DPOLY")
      (foreach p bordo (command p))
      (command "")
      (princ "\nBordo direito criado corretamente.")
    )
    (princ "\nFalha ao criar bordo direito.")
  )

  (princ)
)