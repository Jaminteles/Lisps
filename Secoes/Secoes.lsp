;; =====================================================
;; FUNÇÕES AUXILIARES
;; =====================================================

(defun getVertices (ent)
  (mapcar 'cdr
    (vl-remove-if-not
      '(lambda (e) (= (car e) 10))
      (entget ent)
    )
  )
)

(defun interpY (x p1 p2)
  (setq x1 (car p1)
        y1 (cadr p1)
        x2 (car p2)
        y2 (cadr p2))
  (+ y1 (* (- x x1) (/ (- y2 y1) (- x2 x1))))
)

(defun getYatX (x pts / p1 p2 y)
  (setq y nil)
  (while (and (not y) (cadr pts))
    (setq p1 (car pts)
          p2 (cadr pts))
    (if (and
          (<= (min (car p1) (car p2)) x)
          (>= (max (car p1) (car p2)) x))
      (setq y (interpY x p1 p2))
    )
    (setq pts (cdr pts))
  )
  y
)

(defun getAxisX (ent / pts xs)
  (setq pts (getVertices ent))
  (setq xs (mapcar 'car pts))
  (/ (apply '+ xs) (float (length xs)))
)

;; =====================================================
;; ALEATORIEDADE COMPATÍVEL
;; =====================================================

(defun rand01 ( / ms)
  (setq ms (getvar "MILLISECS"))
  (/ (rem ms 1000) 1000.0)
)

(defun rand-range (a b)
  (+ a (* (rand01) (- b a)))
)

;; =====================================================
;; COMANDO PRINCIPAL
;; =====================================================

(defun c:SECOES ( / perc fator
                    entTN entPista entEixo
                    ptsTN ptsPista
                    xmin xmax eixoX
                    xs x passo xAnt
                    novosPts
                    yTN yP dy yNovo
                    qtdPts)

  ;; -----------------------------
  ;; Percentual de aumento
  ;; -----------------------------
  (setq perc (getreal "\nPercentual de aumento da área <75>: "))
  (if (null perc) (setq perc 75.0))
  (setq fator (+ 1.0 (/ perc 100.0)))

  (princ
    (strcat
      "\nModo contínuo."
      "\nAumento: " (rtos perc 2 1) "%"
      "\nESC para encerrar."
    )
  )

  ;; =============================
  ;; LOOP CONTÍNUO
  ;; =============================
  (while T

    ;; -----------------------------
    ;; Seleções
    ;; -----------------------------
    (setq entTN (car (entsel "\nSelecione a PL do TERRENO NATURAL: ")))
    (setq entPista (car (entsel "\nSelecione a PL da PISTA: ")))
    (setq entEixo (car (entsel "\nSelecione a PL do EIXO (vertical): ")))

    (if (or (null entTN) (null entPista) (null entEixo))
      (progn
        (princ "\nComando encerrado.")
        (exit)
      )
    )

    ;; -----------------------------
    ;; Preparação
    ;; -----------------------------
    (setq ptsTN    (getVertices entTN))
    (setq ptsPista (getVertices entPista))

    (setq xmin (car (car ptsTN)))
    (setq xmax (car (last ptsTN)))

    (setq eixoX (getAxisX entEixo))

    ;; -----------------------------
    ;; Geração de X naturais
    ;; -----------------------------
    (setq xs (list xmin xmax eixoX))
    (setq x xmin)
    (setq xAnt xmin)

    (while (< x xmax)
      (setq passo (rand-range 4.0 6.0))
      (setq x (+ x passo))
      (if (and (< x xmax) (> (- x xAnt) 3.0))
        (progn
          (setq xs (cons x xs))
          (setq xAnt x)
        )
      )
    )

    (setq xs (vl-sort xs '<))

    ;; -----------------------------
    ;; Montagem da polyline
    ;; -----------------------------
    (setq novosPts '())

    ;; Primeiro ponto – TN
    (setq yTN (getYatX xmin ptsTN))
    (setq novosPts (list (list xmin yTN 0.0)))

    ;; Pontos internos ajustados
    (foreach x xs
      (if (and (> x xmin) (< x xmax))
        (progn
          (setq yTN (getYatX x ptsTN))
          (setq yP  (getYatX x ptsPista))
          (if (and yTN yP)
            (progn
              (setq dy (- yTN yP))
              (setq yNovo (+ yP (* dy fator)))
              (setq novosPts
                (append novosPts (list (list x yNovo 0.0))))
            )
          )
        )
      )
    )

    ;; Último ponto – TN
    (setq yTN (getYatX xmax ptsTN))
    (setq novosPts
      (append novosPts (list (list xmax yTN 0.0)))
    )

    ;; -----------------------------
    ;; Criação da polyline
    ;; -----------------------------
    (command "_.PLINE")
    (foreach p novosPts (command p))
    (command "")

    (setq qtdPts (length novosPts))

    (princ
      (strcat
        "\nPL criada | Pontos: "
        (itoa qtdPts)
      )
    )
  )

  (princ)
)
