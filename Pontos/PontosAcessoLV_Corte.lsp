;; ==========================================================================
;;  PONTOS DE ACESSO - LV - VERSAO COM CORTE (ENTRADA/SAIDA) - DETECCAO
;;
;;  Os pontos de ENTRADA/SAIDA NAO tem descricao/layer proprios: estao
;;  misturados nos pontos de bordo (ACE-LE / ACE-LD). Entao um algoritmo
;;  decide quais sao entrada, por GEOMETRIA:
;;
;;    1) Ordena cada bordo por caminhamento (vizinho + proximo do inicial).
;;    2) Mede a ESTACA de cada ponto projetando no EIXO (que voce clica).
;;    3) Marca como ENTRADA o ponto onde:
;;         - o angulo fecha demais (bico / linha volta em si) OU
;;         - o passo anda muito de LADO e quase nada de ESTACA
;;           (entrou/saiu perpendicular ao eixo = boca da entrada).
;;       -> nao depende da via ser larga ou estreita (serve p/ primitivo).
;;    4) Mostra os suspeitos em VERMELHO e pede confirmacao:
;;         Aceitar  = corta nos detectados
;;         Manual   = ignora a deteccao, VOCE clica os pontos de entrada
;;         Continuo = nao corta (bordo inteiro)
;;    5) O bordo e CORTADO na entrada (nao liga os dois lados) e, se a
;;       entrada tiver 2+ pontos, eles viram uma linha "ENTRADA" propria.
;;
;;  O eixo e sempre desenhado continuo.
;;  Independente do PontosAcessoLV.lsp (helpers com prefixo lvc-).
;; ==========================================================================


;; distancia 2D (ignora cota) -------------------------------------------------
(defun lvc-dist2d (p1 p2)
  (distance (list (car p1) (cadr p1)) (list (car p2) (cadr p2)))
)


;; le DESCRICAO + LAYER do ponto clicado (retorna (desc layer)) ---------------
(defun lvc-getkey (msg / e obj d ly)
  (setq e (car (entsel msg)))
  (if e
    (progn
      (setq obj (vlax-ename->vla-object e))
      (setq d  (vl-catch-all-apply 'vlax-get (list obj 'RawDescription)))
      (setq ly (vl-catch-all-apply 'vlax-get (list obj 'Layer)))
      (if (or (vl-catch-all-error-p d) (vl-catch-all-error-p ly))
        (progn (princ " -> nao e um COGO Point valido.") nil)
        (progn
          (princ (strcat " -> descricao: " d " | layer: " ly))
          (list (strcase d) (strcase ly))
        )
      )
    )
    (progn (princ " -> nada selecionado.") nil)
  )
)


;; coleta pontos (E N Z) que casam DESCRICAO e LAYER da amostra (key) ---------
(defun lvc-collect (ss key / i obj desc layer pts)
  (setq pts '() i 0)
  (if (and ss key)
    (progn
      (setq desc (car key) layer (cadr key))
      (while (< i (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (if (and (= (strcase (vlax-get obj 'RawDescription)) desc)
                 (= (strcase (vlax-get obj 'Layer)) layer))
          (setq pts
            (cons (list (vlax-get obj 'Easting)
                        (vlax-get obj 'Northing)
                        (vlax-get obj 'Elevation)) pts))
        )
        (setq i (1+ i))
      )
    )
  )
  pts
)


;; ponto da lista mais proximo de 'ref' (2D) ----------------------------------
(defun lvc-nearest (ref lst / best bestd d)
  (setq best (car lst) bestd (lvc-dist2d ref (car lst)))
  (foreach p (cdr lst)
    (setq d (lvc-dist2d ref p))
    (if (< d bestd) (setq best p bestd d))
  )
  best
)


;; ordena pontos por vizinho mais proximo a partir do inicial (caminhamento) --
(defun lvc-order-pts (pts startPt / remaining ordered cur nxt)
  (if (< (length pts) 1)
    nil
    (progn
      (setq remaining pts ordered '())
      (setq cur (if startPt (lvc-nearest startPt remaining) (car remaining)))
      (setq ordered (list cur))
      (setq remaining (vl-remove cur remaining))
      (while remaining
        (setq nxt (lvc-nearest cur remaining))
        (setq ordered (append ordered (list nxt)))
        (setq remaining (vl-remove nxt remaining))
        (setq cur nxt)
      )
      ordered
    )
  )
)


;; espacamento medio entre pontos consecutivos --------------------------------
(defun lvc-avgspace (pts / i n s)
  (setq n (length pts) s 0.0 i 1)
  (while (< i n)
    (setq s (+ s (lvc-dist2d (nth (1- i) pts) (nth i pts))) i (1+ i))
  )
  (if (> n 1) (/ s (1- n)) 1.0)
)


;; angulo interno no vertice b, formado por a-b-c (radianos, 0..pi) -----------
;;   ~pi (180) = reto ; pequeno = bico (linha volta em si)
(defun lvc-iang (a b c / a1 a2 d)
  (setq a1 (angle b a) a2 (angle b c) d (abs (- a1 a2)))
  (if (> d pi) (setq d (- (+ pi pi) d)))
  d
)


;; cria polyline temporaria (plana, Z=0) do eixo p/ medir estaca --------------
(defun lvc-mkcurve (pts)
  (command "_.PLINE")
  (foreach p pts (command (list (car p) (cadr p) 0.0)))
  (command "")
  (entlast)
)


;; estaca (distancia ao longo do eixo) da projecao do ponto no eixo -----------
(defun lvc-station (curve pt / cp)
  (setq cp (vlax-curve-getClosestPointTo curve (list (car pt) (cadr pt) 0.0)))
  (vlax-curve-getDistAtPoint curve cp)
)


;; DETECCAO: classifica cada ponto ordenado como 'norm ou 'brk (entrada) ------
;;   pts  = pontos do bordo JA ORDENADOS por caminhamento
;;   stns = estaca de cada ponto (mesma ordem)
(defun lvc-classify (pts stns / n i tags avgs perp along ia brk)
  (setq n (length pts) i 0 tags '() avgs (lvc-avgspace pts))
  (while (< i n)
    (setq brk nil)
    ;; passo perpendicular: andou de lado e quase nada de estaca
    (if (> i 0)
      (progn
        (setq perp  (lvc-dist2d (nth (1- i) pts) (nth i pts)))
        (setq along (abs (- (nth i stns) (nth (1- i) stns))))
        (if (and (> perp (* 0.30 avgs)) (< along (* 0.50 perp)))
          (setq brk T)
        )
      )
    )
    ;; bico: angulo fecha demais (< ~60 graus)
    (if (and (> i 0) (< i (1- n)))
      (progn
        (setq ia (lvc-iang (nth (1- i) pts) (nth i pts) (nth (1+ i) pts)))
        (if (< ia 1.047) (setq brk T))
      )
    )
    (setq tags (append tags (list (if brk 'brk 'norm))))
    (setq i (1+ i))
  )
  tags
)


;; lista dos pontos marcados como 'brk ----------------------------------------
(defun lvc-brkpts (pts tags / i n out)
  (setq i 0 n (length pts) out '())
  (while (< i n)
    (if (eq (nth i tags) 'brk) (setq out (cons (nth i pts) out)))
    (setq i (1+ i))
  )
  out
)


;; todos 'norm (opcao Continuo) -----------------------------------------------
(defun lvc-allnorm (pts) (mapcar '(lambda (x) 'norm) pts))


;; tags a partir de cliques manuais (brk se perto de algum clique) ------------
(defun lvc-tagclicks (pts clicks / tol)
  (setq tol (* 1.20 (lvc-avgspace pts)))
  (mapcar
    '(lambda (p)
       (if (vl-some '(lambda (c) (<= (lvc-dist2d p c) tol)) clicks) 'brk 'norm))
    pts)
)


;; coleta cliques de pontos de entrada (ENTER termina) ------------------------
(defun lvc-getclicks ( / pts p)
  (setq pts '())
  (princ "\nClique os pontos de ENTRADA (ENTER para terminar):")
  (while (setq p (getpoint "\n  ponto de entrada: "))
    (setq pts (cons p pts))
  )
  pts
)


;; desenha circulos nos suspeitos, retorna as enames --------------------------
(defun lvc-mark (pts rad / ents)
  (setq ents '())
  (foreach p pts
    (command "_.CIRCLE" (list (car p) (cadr p)) rad)
    (setq ents (cons (entlast) ents))
  )
  ents
)


;; apaga entidades (marcadores / curva temporaria) ----------------------------
(defun lvc-erase (ents)
  (foreach e ents (if e (entdel e)))
)


;; cria uma 3DPOLY a partir de pontos -----------------------------------------
(defun lvc-draw (pts nome)
  (if (and pts (> (length pts) 1))
    (progn
      (command "_.3DPOLY")
      (foreach p pts (command p))
      (command "")
      (princ (strcat "\n  " nome " (" (itoa (length pts)) " pts)."))
    )
  )
)


;; desenha o bordo CORTANDO nas entradas --------------------------------------
;;   pts ordenados + tags alinhados. Trechos 'norm viram segmentos; cada
;;   corrida de 'brk (2+ pts) vira uma linha "ENTRADA".
(defun lvc-draw-split (pts tags nome / segs seg runs run i n tag pt np ne)
  (setq segs '() seg '() runs '() run '() i 0 n (length pts))
  (while (< i n)
    (setq pt (nth i pts) tag (nth i tags))
    (if (eq tag 'brk)
      (progn
        (if seg (setq segs (cons seg segs) seg '()))
        (setq run (append run (list pt)))
      )
      (progn
        (if run (setq runs (cons run runs) run '()))
        (setq seg (append seg (list pt)))
      )
    )
    (setq i (1+ i))
  )
  (if seg (setq segs (cons seg segs)))
  (if run (setq runs (cons run runs)))
  (setq segs (reverse segs) runs (reverse runs))
  (setq np (length segs) ne 0 i 1)
  (princ (strcat "\n" nome ":"))
  (foreach s segs
    (lvc-draw s (if (> np 1) (strcat nome " parte " (itoa i)) nome))
    (setq i (1+ i))
  )
  (foreach r runs
    (if (> (length r) 1)
      (progn (setq ne (1+ ne)) (lvc-draw r (strcat nome " ENTRADA " (itoa ne))))
    )
  )
  (if (> np 1) (princ (strcat "  [cortado em " (itoa np) " partes]")))
)


;; ===========================================================================
;;  COMANDO: eixo + bordos, detectando e cortando entradas/saidas
;; ===========================================================================
(defun c:PONTOS_ACESSO_LV_CORTE
  ( / ss kEixo kEsq kDir start os ce col
      ptsEixo ptsEsq ptsDir eixoOrd curve
      esqOrd dirOrd esqStn dirStn esqTags dirTags
      susp rad marks resp clicks)

  (vl-load-com)
  (setq ss (ssget "_X" '((0 . "AECC_COGO_POINT"))))

  (if (null ss)
    (princ "\nNenhum COGO Point no desenho.")
    (progn
      ;; --- cliques de amostra + ponto inicial (com osnap normal) -----------
      (setq kEixo (lvc-getkey "\nClique um ponto do EIXO: "))
      (setq kEsq  (lvc-getkey "\nClique um ponto da ESQUERDA: "))
      (setq kDir  (lvc-getkey "\nClique um ponto da DIREITA: "))
      (setq start (getpoint "\nClique o ponto INICIAL (estaca 0): "))

      (setq ptsEixo (lvc-collect ss kEixo)
            ptsEsq  (lvc-collect ss kEsq)
            ptsDir  (lvc-collect ss kDir))

      (if (< (length ptsEixo) 2)
        (princ "\nPreciso de pelo menos 2 pontos de EIXO para medir estaca.")
        (progn
          ;; --- estado de desenho: sem osnap, sem eco --------------------
          (setq os (getvar 'OSMODE) ce (getvar 'CMDECHO) col (getvar 'CECOLOR))
          (setvar 'CMDECHO 0)
          (setvar 'OSMODE 0)

          ;; --- eixo continuo + curva de medicao -------------------------
          (setq eixoOrd (lvc-order-pts ptsEixo start))
          (lvc-draw eixoOrd "EIXO")
          (setq curve (lvc-mkcurve eixoOrd))

          ;; --- ordena bordos por caminhamento + mede estaca -------------
          (setq esqOrd (lvc-order-pts ptsEsq start)
                dirOrd (lvc-order-pts ptsDir start))
          (setq esqStn (mapcar '(lambda (p) (lvc-station curve p)) esqOrd)
                dirStn (mapcar '(lambda (p) (lvc-station curve p)) dirOrd))

          ;; --- deteccao automatica --------------------------------------
          (setq esqTags (lvc-classify esqOrd esqStn)
                dirTags (lvc-classify dirOrd dirStn))

          ;; --- marca suspeitos em vermelho ------------------------------
          (setq susp (append (lvc-brkpts esqOrd esqTags)
                             (lvc-brkpts dirOrd dirTags)))
          (setq rad (* 0.35 (max (lvc-avgspace esqOrd) (lvc-avgspace dirOrd) 0.5)))
          (setvar 'CECOLOR "1")
          (setq marks (lvc-mark susp rad))
          (setvar 'CECOLOR col)
          (princ (strcat "\n>>> " (itoa (length susp))
                         " ponto(s) de ENTRADA detectado(s) (em vermelho)."))

          ;; --- confirmacao ----------------------------------------------
          (initget "Aceitar Manual Continuo")
          (setq resp
            (cond ((getkword "\nConfirmar? [Aceitar/Manual/Continuo] <Aceitar>: "))
                  ("Aceitar")))
          (lvc-erase marks)

          (cond
            ((= resp "Continuo")
              (setq esqTags (lvc-allnorm esqOrd)
                    dirTags (lvc-allnorm dirOrd)))
            ((= resp "Manual")
              (setvar 'OSMODE os)               ;; osnap p/ clicar os pontos
              (setq clicks (lvc-getclicks))
              (setvar 'OSMODE 0)
              (setq esqTags (lvc-tagclicks esqOrd clicks)
                    dirTags (lvc-tagclicks dirOrd clicks)))
          )

          ;; --- desenha bordos com corte ---------------------------------
          (lvc-draw-split esqOrd esqTags "ESQUERDA")
          (lvc-draw-split dirOrd dirTags "DIREITA")

          ;; --- limpa curva temporaria + restaura estado -----------------
          (if curve (entdel curve))
          (setvar 'OSMODE os) (setvar 'CMDECHO ce) (setvar 'CECOLOR col)
          (princ "\nConcluido.")
        )
      )
    )
  )
  (princ)
)
