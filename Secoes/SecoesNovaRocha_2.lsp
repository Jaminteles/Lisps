;; =====================================================
;; SecoesNovaRocha.lsp  --  comando NROCHA   (Civil 3D)
;; -----------------------------------------------------
;; Gera automaticamente uma NOVA LINHA DE ROCHA (linha de
;; topo) em seções transversais: uma cupula que sobe da rocha
;; existente em direcao ao Terreno Natural, com aspecto
;; NATURAL (poucos pontos, espacamento e altura irregulares).
;;
;; MODELO:
;;   * A cupula sobe ate uma FRAÇÃO do vao rocha->TN
;;     (*ALTURA-FRAC*), sobre um arco de referencia, com
;;     pontos em espacamento aleatorio (*PMIN*..*PMAX*) e
;;     jitter vertical (*JIT*) -> sem "padrao certinho".
;;   * Ha SEMPRE um vertice sobre o EIXO.
;;   * EXTENSÃO LATERAL (rocha de um lado so): a linha avanca
;;     um pouco alem do eixo para o outro lado (*ADV-EIXO*) e
;;     FECHA descendo ate o projeto (nao sobe pela lateral).
;;     Se a rocha cruza o eixo (central) ou ja e larga, usa a
;;     propria largura da rocha.
;;   * NUNCA passa acima do TN (*TN-GAP*), nem abaixo da rocha
;;     existente ou do projeto. Só em CORTE.
;;   * PONTA REAL DA ROCHA (esquerda = rxMin / direita = rxMax),
;;     verificado em CADA LADO separadamente contra a lateral do
;;     projeto (pxMin / pxMax):
;;       - Se a rocha ULTRAPASSA a lateral do projeto nesse lado:
;;         os vertices da rocha que ficam FORA do projeto sao
;;         copiados tal e qual (nao sao recalculados pela cupula).
;;       - Se a rocha NAO ultrapassa a lateral do projeto nesse
;;         lado: a ponta da nova linha fica de *GAP-PROJ-MIN* a
;;         *GAP-PROJ-MAX* ABAIXO do projeto nesse ponto.
;;     (Isso so vale para a ponta que e a extremidade real da
;;     rocha; o lado que e estendido em direcao ao eixo continua
;;     fechando exatamente no projeto, como antes.)
;;
;; SELEÇÃO por clique, validando so o TIPO (a layer e impressa).
;; Ordem: TERRENO NATURAL, PROJETO, ROCHA existente, EIXO.
;;
;; Uso: rode NROCHA. Roda em loop ate ESC / clique no vazio.
;; =====================================================

(vl-load-com)

;; --------- CONFIGURAÇÃO ---------
(setq *LAYER-NOVA*  "F-SC-ROCHA-NOVA") ; layer de saida da nova linha
(setq *COR-NOVA*    22)                ; cor da nova linha (22 = rocha 3a cat.)

(setq *ALTURA-FRAC* 0.60)  ; altura da cupula: fracao do vao rocha->TN (0..1)
(setq *ADV-EIXO*    2.0)   ; avanco alem do eixo quando a rocha e de um lado (m)
(setq *PMIN*        2.0)   ; espacamento minimo entre pontos (m)
(setq *PMAX*        3.5)   ; espacamento maximo entre pontos (m)
(setq *JIT*         0.15)  ; irregularidade vertical dos pontos (m)
(setq *TN-GAP*      0.05)  ; folga minima abaixo do TN (m)
(setq *GAP-PROJ-MIN* 0.10) ; folga min. abaixo do projeto (ponta real da rocha, qdo nao cruza a lateral) (m)
(setq *GAP-PROJ-MAX* 0.25) ; folga max. abaixo do projeto (ponta real da rocha, qdo nao cruza a lateral) (m)
(setq *STEP*        0.25)  ; passo so para o calculo de area (m)
;; --------------------------------

;; ================================
;; ALEATORIEDADE (LCG com semente)
;; ================================
(defun rnd ( / )
  (if (null *nr-seed*)
    (setq *nr-seed* (fix (abs (+ 1.0 (getvar "MILLISECS"))))))
  (setq *nr-seed* (rem (+ (* *nr-seed* 1103515245) 12345) 2147483648))
  (/ (abs *nr-seed*) 2147483648.0)
)
(defun rrange (a b) (+ a (* (rnd) (- b a))))

;; ================================
;; FUNÇÕES AUXILIARES
;; ================================
(defun getVertices (ent)
  (vl-sort
    (mapcar 'cdr
      (vl-remove-if-not '(lambda (e) (= (car e) 10)) (entget ent)))
    '(lambda (a b) (< (car a) (car b)))
  )
)

(defun getAxisX (ent) (car (cdr (assoc 10 (entget ent)))))

;; Y interpolado em X (clamp fora do intervalo)
(defun y-at-x (x pts / p1 p2 y)
  (cond
    ((<= x (car (car pts)))  (cadr (car pts)))
    ((>= x (car (last pts))) (cadr (last pts)))
    (T
      (setq y nil)
      (while (and (null y) (cadr pts))
        (setq p1 (car pts) p2 (cadr pts))
        (if (and (<= (car p1) x) (<= x (car p2)) (/= (car p1) (car p2)))
          (setq y (+ (cadr p1)
                     (* (- x (car p1))
                        (/ (- (cadr p2) (cadr p1)) (- (car p2) (car p1))))))
        )
        (setq pts (cdr pts))
      )
      (if y y 0.0)
    )
  )
)

;; Seleção validando apenas o TIPO. Retorna ename ou nil (ESC).
(defun sel-tipo (msg tipos / ent res)
  (setq res 'retry)
  (while (eq res 'retry)
    (setq ent (car (entsel msg)))
    (cond
      ((null ent) (setq res nil))
      ((not (member (cdr (assoc 0 (entget ent))) tipos))
       (princ (strcat "\nERRO: tipo invalido (esperado "
                      (apply 'strcat (mapcar '(lambda (s) (strcat s " ")) tipos))
                      "). Tente novamente.")))
      (T
       (princ (strcat "  [layer: " (cdr (assoc 8 (entget ent))) "]"))
       (setq res ent))
    )
  )
  res
)

(defun ensure-layer (name col)
  (if (not (tblsearch "layer" name))
    (entmake
      (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
            '(100 . "AcDbLayerTableRecord")
            (cons 2 name) (cons 62 col) '(70 . 0)))
  )
)

(defun draw-pl (pts lay cor / dxf)
  (setq dxf (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                  (cons 8 lay) (cons 62 cor)
                  '(100 . "AcDbPolyline")
                  (cons 90 (length pts)) '(70 . 0)))
  (foreach p pts
    (setq dxf (append dxf (list (cons 10 (list (car p) (cadr p)))))))
  (entmake dxf)
)

;; ================================
;; ARCO DE REFERENCIA
;; usa variaveis dinamicas: ptsTN ptsProj ptsRocha
;;   rxMin rxMax  (extensao da rocha)
;;   xStart xEnd yStart yEnd  (extensao/pontas da nova linha)
;; ================================
(defun arc-at (x / base cap fl u ch bump tv)
  (setq base (y-at-x x ptsProj))
  (setq cap  (- (y-at-x x ptsTN) *TN-GAP*))
  ;; piso: rocha onde ela existe; fora dela, o projeto
  (setq fl   (if (and (>= x rxMin) (<= x rxMax)) (y-at-x x ptsRocha) base))
  (if (< cap base)
    base
    (progn
      (setq u  (if (> (- xEnd xStart) 1e-6) (/ (- x xStart) (- xEnd xStart)) 0.0))
      (setq ch (+ yStart (* u (- yEnd yStart))))   ; corda entre as pontas
      (setq bump (sin (* pi u)))                    ; 0 nas pontas, 1 no centro
      (setq tv (+ ch (* *ALTURA-FRAC* (- cap ch) bump)))
      (setq tv (min tv cap))
      (setq tv (max tv fl base))
      tv
    )
  )
)

;; Integra (topo - projeto) entre x0 e x1, so a parte em corte
(defun area-int (topfn x0 x1 / x a xmid t0 b0)
  (setq a 0.0 x x0)
  (while (< x x1)
    (setq xmid (+ x (/ *STEP* 2.0)))
    (setq b0 (y-at-x xmid ptsProj))
    (setq t0 (apply topfn (list xmid)))
    (if (> t0 b0) (setq a (+ a (* (- t0 b0) *STEP*))))
    (setq x (+ x *STEP*))
  )
  a
)

(defun top-orig (x) (min (y-at-x x ptsRocha) (- (y-at-x x ptsTN) *TN-GAP*)))

;; topo da linha NOVA ja finalizada (inclui trechos copiados da rocha - regra 1)
(defun top-new (x) (y-at-x x pts))

;; ================================
;; COMANDO PRINCIPAL
;; ================================
(defun c:NROCHA ( / entTN entProj entRocha entEixo
                    ptsTN ptsProj ptsRocha
                    rxMin rxMax yL yR eixoX pxMin pxMax
                    xStart xEnd yStart yEnd
                    leftExtra rightExtra regraEsq regraDir
                    xs xx x base cap fl av yv pts A0 A1 lado)

  (ensure-layer *LAYER-NOVA* *COR-NOVA*)

  (princ (strcat "\n=== NROCHA === cupula ~"
                 (rtos (* 100 *ALTURA-FRAC*) 2 0)
                 "% do vao rocha->TN, com pontos naturais."
                 " ESC/clique no vazio encerra."))

  (while T
    (princ "\n\n--- Nova secao ---")

    (setq entTN (sel-tipo "\n1) Clique a PL do TERRENO NATURAL: " '("LWPOLYLINE" "POLYLINE")))
    (if (null entTN) (progn (princ "\nEncerrado.") (exit)))
    (setq entProj (sel-tipo "\n2) Clique a PL do PROJETO: " '("LWPOLYLINE" "POLYLINE")))
    (if (null entProj) (progn (princ "\nEncerrado.") (exit)))
    (setq entRocha (sel-tipo "\n3) Clique a PL da ROCHA existente: " '("LWPOLYLINE" "POLYLINE")))
    (if (null entRocha) (progn (princ "\nEncerrado.") (exit)))
    (setq entEixo (sel-tipo "\n4) Clique a LINE do EIXO: " '("LINE")))
    (if (null entEixo) (progn (princ "\nEncerrado.") (exit)))

    ;; ---- geometria ----
    (setq ptsTN    (getVertices entTN))
    (setq ptsProj  (getVertices entProj))
    (setq ptsRocha (getVertices entRocha))
    (setq eixoX    (getAxisX entEixo))

    (setq rxMin (car (car ptsRocha)))
    (setq rxMax (car (last ptsRocha)))
    (setq yL    (cadr (car ptsRocha)))
    (setq yR    (cadr (last ptsRocha)))
    (setq pxMin (car (car ptsProj)))       ; limites do projeto (seguranca)
    (setq pxMax (car (last ptsProj)))

    (setq leftExtra nil rightExtra nil regraEsq "" regraDir "")

    ;; ---- decide a extensao lateral (em relacao ao EIXO) ----
    ;; + em cada ponta que for a extremidade REAL da rocha, aplica
    ;;   a regra 1 (ultrapassa a lateral do projeto -> segue a rocha)
    ;;   ou a regra 2 (nao ultrapassa -> fica um pouco abaixo do projeto)
    (cond
      ;; rocha toda a ESQUERDA do eixo -> estende para a direita (fecha no projeto)
      ((<= rxMax eixoX)
       (setq lado "esq (estende p/ direita)")
       (setq xEnd (min pxMax (+ eixoX *ADV-EIXO*)))
       (setq yEnd (y-at-x xEnd ptsProj))
       ;; lado esquerdo = ponta real da rocha
       (if (< rxMin pxMin)
         (progn
           (setq leftExtra (vl-remove-if-not '(lambda (p) (< (car p) pxMin)) ptsRocha))
           (setq xStart pxMin  yStart (y-at-x pxMin ptsRocha))
           (setq regraEsq "regra1(segue rocha, fora do projeto)"))
         (progn
           (setq xStart rxMin
                 yStart (- (y-at-x rxMin ptsProj) (rrange *GAP-PROJ-MIN* *GAP-PROJ-MAX*)))
           (setq regraEsq "regra2(recuo abaixo do projeto)"))
       ))
      ;; rocha toda a DIREITA do eixo -> estende para a esquerda (fecha no projeto)
      ((>= rxMin eixoX)
       (setq lado "dir (estende p/ esquerda)")
       (setq xStart (max pxMin (- eixoX *ADV-EIXO*)))
       (setq yStart (y-at-x xStart ptsProj))
       ;; lado direito = ponta real da rocha
       (if (> rxMax pxMax)
         (progn
           (setq rightExtra (vl-remove-if-not '(lambda (p) (> (car p) pxMax)) ptsRocha))
           (setq xEnd pxMax  yEnd (y-at-x pxMax ptsRocha))
           (setq regraDir "regra1(segue rocha, fora do projeto)"))
         (progn
           (setq xEnd rxMax
                 yEnd (- (y-at-x rxMax ptsProj) (rrange *GAP-PROJ-MIN* *GAP-PROJ-MAX*)))
           (setq regraDir "regra2(recuo abaixo do projeto)"))
       ))
      ;; rocha cruza o eixo (central/larga) -> as DUAS pontas sao reais
      (T
       (setq lado "central (sem extensao pelo eixo)")
       (if (< rxMin pxMin)
         (progn
           (setq leftExtra (vl-remove-if-not '(lambda (p) (< (car p) pxMin)) ptsRocha))
           (setq xStart pxMin  yStart (y-at-x pxMin ptsRocha))
           (setq regraEsq "regra1(segue rocha, fora do projeto)"))
         (progn
           (setq xStart rxMin
                 yStart (- (y-at-x rxMin ptsProj) (rrange *GAP-PROJ-MIN* *GAP-PROJ-MAX*)))
           (setq regraEsq "regra2(recuo abaixo do projeto)"))
       )
       (if (> rxMax pxMax)
         (progn
           (setq rightExtra (vl-remove-if-not '(lambda (p) (> (car p) pxMax)) ptsRocha))
           (setq xEnd pxMax  yEnd (y-at-x pxMax ptsRocha))
           (setq regraDir "regra1(segue rocha, fora do projeto)"))
         (progn
           (setq xEnd rxMax
                 yEnd (- (y-at-x rxMax ptsProj) (rrange *GAP-PROJ-MIN* *GAP-PROJ-MAX*)))
           (setq regraDir "regra2(recuo abaixo do projeto)"))
       ))
    )

    ;; ---- abscissas: espacamento aleatorio + eixo obrigatorio ----
    (setq xs (list xStart) xx xStart)
    (while (< xx xEnd)
      (setq xx (+ xx (rrange *PMIN* *PMAX*)))
      (if (< xx (- xEnd (* 0.5 *PMIN*))) (setq xs (cons xx xs))))
    (if (and (> eixoX (+ xStart 0.2)) (< eixoX (- xEnd 0.2)))
      (setq xs (cons eixoX xs)))
    (setq xs (cons xEnd xs))
    (setq xs (vl-sort xs '<))

    ;; ---- monta os pontos (arco + jitter; pontas fixas em yStart/yEnd) ----
    (setq pts '())
    (foreach x xs
      (cond
        ((= x xStart) (setq yv yStart))  ; ponta real da rocha: usa o valor exato (regra 1/2)
        ((= x xEnd)   (setq yv yEnd))
        (T
          (setq av   (arc-at x))
          (setq base (y-at-x x ptsProj))
          (setq cap  (- (y-at-x x ptsTN) *TN-GAP*))
          (setq fl   (if (and (>= x rxMin) (<= x rxMax)) (y-at-x x ptsRocha) base))
          (setq yv   (+ av (rrange (- *JIT*) *JIT*)))
          (setq yv   (min yv cap))
          (setq yv   (max yv fl base))
        )
      )
      (setq pts (cons (list x yv) pts))
    )
    (setq pts (reverse pts))

    ;; ---- regra 1: prefixa/sufixa com os vertices REAIS da rocha que ficam fora do projeto ----
    (if leftExtra  (setq pts (append leftExtra pts)))
    (if rightExtra (setq pts (append pts rightExtra)))

    (draw-pl pts *LAYER-NOVA* *COR-NOVA*)

    ;; ---- relatorio ----
    (setq A0 (area-int 'top-orig rxMin rxMax))
    (setq A1 (area-int 'top-new (car (car pts)) (car (last pts))))
    (princ
      (strcat
        "\nOK | " lado
        " | esq: " regraEsq " | dir: " regraDir
        " | pts: " (itoa (length pts))
        " | area orig: " (rtos A0 2 2) " m2 | nova: " (rtos A1 2 2) " m2"
        (if (> A0 1e-6)
          (strcat " | aumento: " (rtos (* 100 (- (/ A1 A0) 1.0)) 2 1) "%")
          " | (area orig ~0: confira se clicou na rocha)")
      )
    )
  )
  (princ)
)

(princ "\nSecoesNovaRocha.lsp carregado. Digite NROCHA para executar.")
(princ)