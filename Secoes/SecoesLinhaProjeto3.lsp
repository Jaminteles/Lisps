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

(defun selectPLbyLayer (msg layerName / ent ed lay)
  (while
    (progn
      (setq ent (car (entsel msg)))
      (cond
        ((null ent)
         (princ "\nNada selecionado. Selecione novamente.")
         T
        )
        ((not (member (cdr (assoc 0 (entget ent))) '("LWPOLYLINE" "POLYLINE")))
         (princ "\nEntidade não é uma polyline.")
         T
        )
        ((/= (cdr (assoc 8 (entget ent))) layerName)
         (princ
           (strcat
             "\nLayer incorreto. Esperado: "
             layerName
           )
         )
         T
        )
        (T nil)
      )
    )
  )
  ent
)

(defun selectLINEbyLayer (msg layerName / ent ed)
  (while
    (progn
      (setq ent (car (entsel msg)))
      (cond
        ((null ent)
         (princ "\nNada selecionado. Selecione novamente.")
         T
        )
        ((/= (cdr (assoc 0 (entget ent))) "LINE")
         (princ "\nEntidade não é uma LINE.")
         T
        )
        ((/= (cdr (assoc 8 (entget ent))) layerName)
         (princ
           (strcat
             "\nLayer incorreto. Esperado: "
             layerName
           )
         )
         T
        )
        (T nil)
      )
    )
  )
  ent
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

(defun isPolyline (ent)
  (and ent
       (wcmatch (cdr (assoc 0 (entget ent))) "*POLYLINE"))
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

(defun c:SDCOES ( / perc fator
                    entTN entPista entEixo
                    ptsTN ptsPista
                    xmin xmax eixoX
                    xs x passo xAnt
                    novosPts
                    yTN yP dy yNovo
                    yP_ini yP_fim
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

  (while T

    ;; -----------------------------
    ;; Seleções (com validação de layer)
    ;; -----------------------------
    (setq entTN
      (selectPLbyLayer
        "\nSelecione a PL do TERRENO NATURAL: "
        "F-SC-VIEW"
      )
    )

    (setq entPista
      (selectPLbyLayer
        "\nSelecione a PL da PISTA: "
        "F-SC-PROJETO"
      )
    )

    (setq entEixo
      (selectLINEbyLayer
        "\nSelecione a LINE do EIXO: "
        "F-SC-MALHA-TXT"
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
    ;; Geração de X
    ;; -----------------------------
    (setq xs (list xmin xmax eixoX))
    (setq x xmin xAnt xmin)

    (while (< x xmax)
      (setq passo (rand-range 4.0 6.0))
      (setq x (+ x passo))
      (if (and (< x xmax) (> (- x xAnt) 3.0))
        (setq xs (cons x xs) xAnt x)
      )
    )

    (setq xs (vl-sort xs '<))

    ;; -----------------------------
    ;; Montagem da polyline
    ;; -----------------------------
    (setq novosPts '())

    ;; Pontos da pista
    (setq pPistaIni  (car ptsPista))
    (setq pPistaIni2 (cadr ptsPista))

    (setq pPistaFim  (last ptsPista))
    (setq pPistaFim2 (nth (- (length ptsPista) 2) ptsPista))

    ;; -----------------------------
    ;; Cálculo do offset - INÍCIO
    ;; -----------------------------
    (setq dyIni (- (cadr pPistaIni) (cadr pPistaIni2)))

    (if (< dyIni 0)
      (setq offsetIni 0.003)   ;; sobe
      (setq offsetIni -0.003)  ;; desce
    )

    ;; -----------------------------
    ;; Cálculo do offset - FIM
    ;; -----------------------------
    (setq dyFim (- (cadr pPistaFim) (cadr pPistaFim2)))

    (if (< dyFim 0)
      (setq offsetFim 0.003)
      (setq offsetFim -0.003)
    )

    ;; -----------------------------
    ;; 1️⃣ Primeiro ponto – TN
    ;; -----------------------------
    (setq yTN (getYatX xmin ptsTN))
    (setq novosPts
      (list (list xmin yTN 0.0))
    )

    ;; -----------------------------
    ;; 2️⃣ Segundo ponto – relativo à pista (INÍCIO)
    ;; -----------------------------
    (setq novosPts
      (append novosPts
        (list
          (list
            (car pPistaIni)
            (+ (cadr pPistaIni) offsetIni)
            0.0
          )
        )
      )
    )

    ;; -----------------------------
    ;; Pontos internos ajustados
    ;; -----------------------------
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

    ;; -----------------------------
    ;; 4️⃣ Penúltimo ponto – relativo à pista (FIM)
    ;; -----------------------------
    (setq novosPts
      (append novosPts
        (list
          (list
            (car pPistaFim)
            (+ (cadr pPistaFim) offsetFim)
            0.0
          )
        )
      )
    )

    ;; -----------------------------
    ;; 5️⃣ Último ponto – TN
    ;; -----------------------------
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