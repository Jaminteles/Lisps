(defun c:PONTOS_ACESSO_EIXO
 ( / ss i obj pts grp eixoPts p)

  (vl-load-com)

  (princ "\nSelecione os COGO Points do acesso...")
  (setq ss (ssget '((0 . "AECC_COGO_POINT"))))

  (if (or (null ss) (< (sslength ss) 3))
    (progn
      (princ "\nPontos insuficientes.")
      (princ)
      (return)
    )
  )

  ;; coleta pontos
  (setq pts '())
  (setq i 0)
  (while (< i (sslength ss))
    (setq obj (vlax-ename->vla-object (ssname ss i)))
    (setq pts
      (cons
        (list
          (vlax-get obj 'Easting)
          (vlax-get obj 'Northing)
          (vlax-get obj 'Elevation)
        )
        pts
      )
    )
    (setq i (1+ i))
  )

  ;; ordena por avanço longitudinal
  (setq pts (vl-sort pts '(lambda (a b) (< (cadr a) (cadr b)))))

  ;; cria seções de 3 pontos
  (setq grp '())
  (setq eixoPts '())

  (foreach p pts
    (setq grp (append grp (list p)))
    (if (= (length grp) 3)
      (progn
        ;; ordena lateralmente (X)
        (setq grp (vl-sort grp '(lambda (a b) (< (car a) (car b)))))

        ;; pega o ponto CENTRAL REAL
        (setq eixoPts (append eixoPts (list (cadr grp))))

        ;; limpa grupo
        (setq grp '())
      )
    )
  )

  ;; cria 3DPOLY
  (if (> (length eixoPts) 1)
    (progn
      (command "_.3DPOLY")
      (foreach p eixoPts (command p))
      (command "")
      (princ "\nEixo criado corretamente sobre os pontos reais.")
    )
    (princ "\nNão foi possível gerar o eixo.")
  )

  (princ)
)

















(defun c:PONTOS_ACESSO_ESQ
 ( / ss i obj pts grp secoes p bordo)

  (vl-load-com)

  (princ "\nSelecione os COGO Points do acesso...")
  (setq ss (ssget '((0 . "AECC_COGO_POINT"))))

  (if (or (null ss) (< (sslength ss) 3))
    (progn (princ "\nPontos insuficientes.") (princ) (return))
  )

  ;; coleta pontos
  (setq pts '())
  (setq i 0)
  (while (< i (sslength ss))
    (setq obj (vlax-ename->vla-object (ssname ss i)))
    (setq pts
      (cons
        (list
          (vlax-get obj 'Easting)
          (vlax-get obj 'Northing)
          (vlax-get obj 'Elevation)
        )
        pts
      )
    )
    (setq i (1+ i))
  )

  ;; ordena por avanço
  (setq pts (vl-sort pts '(lambda (a b) (< (cadr a) (cadr b)))))

  ;; cria seções de 3 pontos (lateral)
  (setq secoes '())
  (setq grp '())

  (foreach p pts
    (setq grp (append grp (list p)))
    (if (= (length grp) 3)
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




















(defun c:PONTOS_ACESSO_DIR
 ( / ss i obj pts grp secoes p bordo)

  (vl-load-com)

  (princ "\nSelecione os COGO Points do acesso...")
  (setq ss (ssget '((0 . "AECC_COGO_POINT"))))

  (if (or (null ss) (< (sslength ss) 3))
    (progn (princ "\nPontos insuficientes.") (princ) (return))
  )

  ;; coleta pontos
  (setq pts '())
  (setq i 0)
  (while (< i (sslength ss))
    (setq obj (vlax-ename->vla-object (ssname ss i)))
    (setq pts
      (cons
        (list
          (vlax-get obj 'Easting)
          (vlax-get obj 'Northing)
          (vlax-get obj 'Elevation)
        )
        pts
      )
    )
    (setq i (1+ i))
  )

  ;; ordena por avanço
  (setq pts (vl-sort pts '(lambda (a b) (< (cadr a) (cadr b)))))

  ;; cria seções de 3 pontos
  (setq secoes '())
  (setq grp '())

  (foreach p pts
    (setq grp (append grp (list p)))
    (if (= (length grp) 3)
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
    (setq bordo (append bordo (list (last grp))))
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
