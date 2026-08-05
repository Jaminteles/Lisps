;; ==========================================================================
;;  PONTOS DE PE E CRISTA  ->  3DPOLY
;;
;;  Liga os COGO Points de pe/crista em polilinhas 3D que "fazem sentido":
;;  cresce cada linha de forma DIRECIONAL (sempre pra frente), com cone de
;;  angulo e limite lateral, para NAO pular de uma linha para a paralela
;;  (ex.: pe/crista do outro lado do bordo).
;;
;;  - Os pontos sao separados automaticamente por LAYER + DESCRICAO, entao
;;    pe e crista saem em linhas distintas numa unica passada.
;;  - Cada 3DPOLY e criada NO MESMO LAYER dos pontos que a geraram.
;;  - Voce seleciona as linhas que NAO podem ser atravessadas (eixo/bordo):
;;    nenhuma ligacao vai cruzar essas linhas.
;;
;;  COMANDOS:
;;    PONTOS_PE_CRISTA    -> automatico: separa por layer+descricao e desenha
;;                           todas as linhas de uma vez.
;;    PONTOS_LINHA_1      -> manual: voce clica 1 ponto de amostra (define
;;                           layer+descricao) e o ponto INICIAL; desenha 1
;;                           linha so. Bom para amontoados/entradas/montes.
;; ==========================================================================


;; -------- parametros ajustaveis (edite aqui se precisar) -------------------
(setq *PC-DIST*  25.0)   ; distancia maxima entre pontos consecutivos (m)
(setq *PC-ANG*   70.0)   ; abertura maxima do cone pra frente (graus)
(setq *PC-LAT*    5.0)   ; desvio lateral maximo em relacao a direcao (m)


;; -------- utilitarios geometricos ------------------------------------------

;; distancia 2D (ignora cota)
(defun pc-d2 (a b)
  (sqrt (+ (expt (- (car b) (car a)) 2.0)
           (expt (- (cadr b) (cadr a)) 2.0))))

;; angulo nao-orientado entre dois vetores 2D (0..pi)
(defun pc-ang (v1 v2)
  (abs (atan (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
             (+ (* (car v1) (car v2)) (* (cadr v1) (cadr v2))))))

;; desvio lateral (perpendicular) de p em relacao a reta (curr, dir)
(defun pc-lat (p curr dir / vx vy dx dy m)
  (setq dx (car dir) dy (cadr dir))
  (setq m (sqrt (+ (* dx dx) (* dy dy))))
  (if (> m 1e-9) (setq dx (/ dx m) dy (/ dy m)))
  (setq vx (- (car p) (car curr))
        vy (- (cadr p) (cadr curr)))
  (abs (- (* vx dy) (* vy dx))))


;; -------- barreiras (linhas que nao podem ser atravessadas) ----------------

;; ponto 2D a partir de lista/variant/safearray
(defun pc-xy (v)
  (if (= (type v) 'variant)   (setq v (vlax-variant-value v)))
  (if (= (type v) 'safearray) (setq v (vlax-safearray->list v)))
  (list (car v) (cadr v)))

;; segmentos 2D de uma entidade (LINE ou qualquer polyline)
(defun pc-obj-segs (e / obj typ co stride pts segs n i)
  (setq obj (vlax-ename->vla-object e)
        typ (vla-get-ObjectName obj))
  (cond
    ((= typ "AcDbLine")
       (list (list (pc-xy (vlax-get obj 'StartPoint))
                   (pc-xy (vlax-get obj 'EndPoint)))))
    (t
       (setq co (vlax-safearray->list
                  (vlax-variant-value (vla-get-Coordinates obj)))
             stride (if (= typ "AcDbPolyline") 2 3)
             n  (length co) i 0 pts '())
       (while (< i n)
         (setq pts (cons (list (nth i co) (nth (1+ i) co)) pts))
         (setq i (+ i stride)))
       (setq pts (reverse pts) segs '() i 0)
       (while (< (1+ i) (length pts))
         (setq segs (cons (list (nth i pts) (nth (1+ i) pts)) segs))
         (setq i (1+ i)))
       (if (equal (vl-catch-all-apply 'vlax-get (list obj 'Closed)) :vlax-true)
         (setq segs (cons (list (last pts) (car pts)) segs)))
       segs)))

;; monta a lista de segmentos de todas as barreiras selecionadas
(defun pc-bar-segs (ss / segs i)
  (setq segs '() i 0)
  (if ss
    (while (< i (sslength ss))
      (setq segs (append segs (pc-obj-segs (ssname ss i))))
      (setq i (1+ i))))
  segs)

;; orientacao (sinal da area do triangulo)
(defun pc-orient (a b c)
  (- (* (- (car b) (car a)) (- (cadr c) (cadr a)))
     (* (- (cadr b) (cadr a)) (- (car c) (car a)))))

;; os segmentos p1-p2 e q1-q2 se cruzam de fato?
(defun pc-seg-x (p1 p2 q1 q2)
  (and (< (* (pc-orient p1 p2 q1) (pc-orient p1 p2 q2)) 0)
       (< (* (pc-orient q1 q2 p1) (pc-orient q1 q2 p2)) 0)))

;; a ligacao p1->p2 cruza alguma barreira?
(defun pc-cross (p1 p2 segs / a b hit)
  (setq a (list (car p1) (cadr p1))
        b (list (car p2) (cadr p2))
        hit nil)
  (foreach s segs
    (if (and (not hit) (pc-seg-x a b (car s) (cadr s)))
      (setq hit t)))
  hit)


;; -------- leitura dos pontos -----------------------------------------------

;; le um COGO point (vla-obj) -> (E N Z LAYER DESC)
(defun pc-read (obj / z d ly)
  (setq z  (vl-catch-all-apply 'vlax-get (list obj 'Elevation)))
  (setq d  (vl-catch-all-apply 'vlax-get (list obj 'RawDescription)))
  (setq ly (vl-catch-all-apply 'vlax-get (list obj 'Layer)))
  (list (vlax-get obj 'Easting)
        (vlax-get obj 'Northing)
        (if (vl-catch-all-error-p z) 0.0 z)
        (if (vl-catch-all-error-p ly) "" (strcase ly))
        (if (vl-catch-all-error-p d)  "" (strcase d))))

;; chave de agrupamento de um ponto lido = "LAYER|DESC"
(defun pc-key (p) (strcat (nth 3 p) "|" (nth 4 p)))


;; -------- crescimento direcional de UMA linha ------------------------------
;; recebe: pontos livres, eixo de semeadura e segmentos-barreira.
;; devolve (linha . restantes).
(defun pc-grow (livres eixo bar / distLim angLim curr dir poly used
                                   melhor melhord ang lat d cand)
  (setq distLim *PC-DIST*
        angLim  (/ (* *PC-ANG* pi) 180.0))

  ;; ponto inicial = extremo ao longo do eixo (min projecao)
  (setq curr
    (car (vl-sort livres
      (function (lambda (a b)
        (< (+ (* (car a) (car eixo)) (* (cadr a) (cadr eixo)))
           (+ (* (car b) (car eixo)) (* (cadr b) (cadr eixo)))))))))

  (setq used (list curr)
        poly (list curr)
        dir  eixo)          ; semeia direcao pelo eixo principal

  (while
    (progn
      (setq melhor nil melhord 1e99)
      (foreach p livres
        (if (not (member p used))
          (progn
            (setq d (pc-d2 curr p))
            (if (<= d distLim)
              (progn
                (setq cand (list (- (car p) (car curr))
                                 (- (cadr p) (cadr curr))))
                (setq ang (pc-ang dir cand))
                (setq lat (pc-lat p curr dir))
                (if (and (<= ang angLim)
                         (or (equal dir eixo 1e-9) (<= lat *PC-LAT*))
                         (not (pc-cross curr p bar))
                         (< d melhord))
                  (setq melhor p melhord d)))))))
      melhor)

    (setq dir  (list (- (car melhor) (car curr))
                     (- (cadr melhor) (cadr curr))))
    (setq curr melhor
          poly (cons melhor poly)
          used (cons melhor used)))

  (setq poly (reverse poly))
  (cons poly (vl-remove-if (function (lambda (x) (member x used))) livres)))


;; -------- eixo principal de um conjunto (para semear direcao) --------------
(defun pc-eixo (pts / minE maxE minN maxN)
  (setq minE 1e99 maxE -1e99 minN 1e99 maxN -1e99)
  (foreach p pts
    (if (< (car p)  minE) (setq minE (car p)))
    (if (> (car p)  maxE) (setq maxE (car p)))
    (if (< (cadr p) minN) (setq minN (cadr p)))
    (if (> (cadr p) maxN) (setq maxN (cadr p))))
  (if (>= (- maxE minE) (- maxN minN)) '(1.0 0.0) '(0.0 1.0)))


;; -------- desenho (no layer informado) -------------------------------------
(defun pc-draw (poly layer / p)
  (if (and poly (> (length poly) 1))
    (progn
      (if (and layer (/= layer "")) (setvar 'CLAYER layer))
      (command "_.3DPOLY")
      (foreach p poly
        (command (list (car p) (cadr p) (caddr p))))
      (command "")
      t)
    nil))


;; -------- particiona um grupo em varias linhas -----------------------------
(defun pc-linhas-grupo (pts bar layer / eixo livres res linha n)
  (setq eixo   (pc-eixo pts)
        livres pts
        n      0)
  (while (>= (length livres) 2)
    (setq res    (pc-grow livres eixo bar)
          linha  (car res)
          livres (cdr res))
    (if (pc-draw linha layer) (setq n (1+ n)))
    ;; se a linha saiu com 1 ponto so, descarta esse ponto para nao travar
    (if (< (length linha) 2)
      (setq livres (vl-remove (car linha) livres))))
  n)


;; ===========================================================================
;;  COMANDO PRINCIPAL: separa por layer+descricao e desenha tudo
;; ===========================================================================
(defun c:PONTOS_PE_CRISTA ( / ss ssBar bar i obj p lidos grupos g k
                             achou total clay)
  (vl-load-com)

  (princ "\nSelecione os COGO Points de pe/crista...")
  (setq ss (ssget '((0 . "AECC_COGO_POINT"))))

  (if (or (null ss) (< (sslength ss) 2))
    (progn (princ "\nSelecione pelo menos dois COGO Points.") (princ))
    (progn
      (princ "\nSelecione as linhas que NAO podem ser atravessadas (Enter=nenhuma)...")
      (setq ssBar (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE"))))
      (setq bar   (pc-bar-segs ssBar))
      (setq clay  (getvar 'CLAYER))

      ;; le e agrupa por layer+descricao
      (setq lidos '() i 0)
      (while (< i (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (setq lidos (cons (pc-read obj) lidos))
        (setq i (1+ i)))

      (setq grupos '())
      (foreach p lidos
        (setq k     (pc-key p)
              achou (assoc k grupos))
        (if achou
          (setq grupos (subst (cons k (cons p (cdr achou))) achou grupos))
          (setq grupos (cons (cons k (list p)) grupos))))

      ;; processa cada grupo (desenha no layer do proprio grupo)
      (setq total 0)
      (foreach g grupos
        (princ (strcat "\nGrupo [" (car g) "] : "
                       (itoa (length (cdr g))) " pontos"))
        (setq total (+ total
                       (pc-linhas-grupo (cdr g) bar (nth 3 (cadr g))))))

      (setvar 'CLAYER clay)
      (princ (strcat "\n\n" (itoa total) " linha(s) 3D criada(s)."))
      (princ))))


;; ===========================================================================
;;  COMANDO MANUAL: 1 amostra + ponto inicial -> desenha 1 linha
;;  (ideal para amontoados / entradas / montes onde o automatico erra)
;; ===========================================================================
(defun c:PONTOS_LINHA_1 ( / ss ssBar bar e obj key layer desc i p pts start
                           eixo curr dir poly used melhor melhord clay
                           d ang lat cand distLim angLim)
  (vl-load-com)

  (setq ss (ssget "_X" '((0 . "AECC_COGO_POINT"))))
  (if (null ss)
    (progn (princ "\nNenhum COGO Point no desenho.") (princ))
    (progn
      ;; amostra: define layer + descricao
      (setq e (car (entsel "\nClique um ponto de AMOSTRA da linha: ")))
      (if (null e)
        (progn (princ "\nNada selecionado.") (princ))
        (progn
          (setq obj   (vlax-ename->vla-object e)
                layer (strcase (vlax-get obj 'Layer))
                desc  (strcase (vlax-get obj 'RawDescription)))
          (princ (strcat " -> layer: " layer " | desc: " desc))

          ;; coleta os pontos que casam layer+descricao
          (setq pts '() i 0)
          (while (< i (sslength ss))
            (setq obj (vlax-ename->vla-object (ssname ss i)))
            (setq p (pc-read obj))
            (if (and (= (nth 3 p) layer) (= (nth 4 p) desc))
              (setq pts (cons p pts)))
            (setq i (1+ i)))

          (if (< (length pts) 2)
            (progn (princ "\nPontos insuficientes nessa amostra.") (princ))
            (progn
              (princ "\nSelecione as linhas que NAO podem ser atravessadas (Enter=nenhuma)...")
              (setq ssBar (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE"))))
              (setq bar   (pc-bar-segs ssBar))

              (setq start (getpoint "\nClique o ponto INICIAL da linha: "))
              (setq eixo (pc-eixo pts))
              ;; semeia curr no ponto real mais proximo do clique
              (setq curr
                (if start
                  (car (vl-sort pts
                    (function (lambda (a b)
                      (< (pc-d2 start a) (pc-d2 start b))))))
                  (car (vl-sort pts
                    (function (lambda (a b)
                      (< (+ (* (car a) (car eixo)) (* (cadr a) (cadr eixo)))
                         (+ (* (car b) (car eixo)) (* (cadr b) (cadr eixo))))))))))

              ;; direcao inicial: do clique para o 1o ponto, se houver clique
              (setq dir
                (if start
                  (list (- (car curr) (car start)) (- (cadr curr) (cadr start)))
                  eixo))
              (if (< (+ (expt (car dir) 2.0) (expt (cadr dir) 2.0)) 1e-9)
                (setq dir eixo))

              (setq distLim *PC-DIST*
                    angLim  (/ (* *PC-ANG* pi) 180.0)
                    used    (list curr)
                    poly    (list curr))

              (while
                (progn
                  (setq melhor nil melhord 1e99)
                  (foreach p pts
                    (if (not (member p used))
                      (progn
                        (setq d (pc-d2 curr p))
                        (if (<= d distLim)
                          (progn
                            (setq cand (list (- (car p) (car curr))
                                             (- (cadr p) (cadr curr))))
                            (setq ang (pc-ang dir cand))
                            (setq lat (pc-lat p curr dir))
                            (if (and (<= ang angLim)
                                     (<= lat *PC-LAT*)
                                     (not (pc-cross curr p bar))
                                     (< d melhord))
                              (setq melhor p melhord d)))))))
                  melhor)
                (setq dir  (list (- (car melhor) (car curr))
                                 (- (cadr melhor) (cadr curr))))
                (setq curr melhor
                      poly (cons melhor poly)
                      used (cons melhor used)))

              (setq poly (reverse poly))
              (setq clay (getvar 'CLAYER))
              (if (pc-draw poly layer)
                (princ (strcat "\nLinha criada (" (itoa (length poly))
                               " pontos) no layer " layer "."))
                (princ "\nNao foi possivel criar a linha."))
              (setvar 'CLAYER clay)
              (princ)))))))
  (princ))


(princ "\nPontosPeCrista.lsp carregado.  Comandos: PONTOS_PE_CRISTA | PONTOS_LINHA_1")
(princ)
