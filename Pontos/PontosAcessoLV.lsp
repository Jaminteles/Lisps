;; ==========================================================================
;;  PONTOS DE ACESSO - LEVANTAMENTO (LV)
;;  Desenha eixo / bordo esquerdo / bordo direito a partir de COGO Points.
;;
;;  Todos os pontos ficam no MESMO layer; o que diferencia e a DESCRICAO.
;;  Voce nao precisa mais decorar a descricao: o LISP le a descricao do
;;  ponto que voce clicar como amostra de cada tipo (eixo/esq/dir).
;;  O ponto INICIAL clicado define de onde a linha comeca (estaca 0) e o
;;  sentido, ordenando por vizinho mais proximo (serve p/ qualquer orientacao).
;; ==========================================================================


;; distancia 2D (ignora cota) --------------------------------------------------
(defun dist2d (p1 p2)
  (distance
    (list (car p1) (cadr p1))
    (list (car p2) (cadr p2))
  )
)


;; le a DESCRICAO + LAYER do ponto que o usuario clicar (retorna (desc layer))-
(defun lv-getkey (msg / e obj d ly)
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


;; coleta os pontos (E N Z) que casam DESCRICAO e LAYER da amostra (key) ------
(defun lv-collect (ss key / i obj desc layer pts)
  (setq pts '() i 0)
  (if (and ss key)
    (progn
      (setq desc (car key) layer (cadr key))
      (while (< i (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (if (and (= (strcase (vlax-get obj 'RawDescription)) desc)
                 (= (strcase (vlax-get obj 'Layer)) layer))
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
        )
        (setq i (1+ i))
      )
    )
  )
  pts
)


;; ponto da lista mais proximo de 'ref' (2D) ----------------------------------
(defun lv-nearest (ref lst / best bestd d)
  (setq best (car lst) bestd (dist2d ref (car lst)))
  (foreach p (cdr lst)
    (setq d (dist2d ref p))
    (if (< d bestd) (setq best p bestd d))
  )
  best
)


;; ordena a lista comecando pelo mais proximo do ponto inicial (vizinho +proximo)
(defun lv-order (pts startPt / remaining ordered cur nxt)
  (if (< (length pts) 1)
    nil
    (progn
      (setq remaining pts ordered '())
      (setq cur (if startPt (lv-nearest startPt remaining) (car remaining)))
      (setq ordered (list cur))
      (setq remaining (vl-remove cur remaining))
      (while remaining
        (setq nxt (lv-nearest cur remaining))
        (setq ordered (append ordered (list nxt)))
        (setq remaining (vl-remove nxt remaining))
        (setq cur nxt)
      )
      ordered
    )
  )
)


;; cria a 3DPOLY -------------------------------------------------------------
(defun lv-draw (pts nome)
  (if (and pts (> (length pts) 1))
    (progn
      (command "_.3DPOLY")
      (foreach p pts (command p))
      (command "")
      (princ (strcat "\n" nome " criado (" (itoa (length pts)) " pontos)."))
    )
    (princ (strcat "\nFalha ao criar " nome ": pontos insuficientes."))
  )
)


;; worker de um lado so (usado pelos comandos separados) ----------------------
(defun lv-run-one (roleName / ss key startPt)
  (vl-load-com)
  (setq ss (ssget "_X" '((0 . "AECC_COGO_POINT"))))
  (if ss
    (progn
      (setq key (lv-getkey (strcat "\nClique um ponto de amostra do " roleName ": ")))
      (setq startPt (getpoint "\nClique o ponto INICIAL (estaca 0): "))
      (lv-draw (lv-order (lv-collect ss key) startPt) roleName)
    )
    (princ "\nNada selecionado.")
  )
  (princ)
)


;; ===========================================================================
;;  COMANDO UNICO: faz eixo + esquerda + direita numa passada so
;; ===========================================================================
(defun c:PONTOS_ACESSO_LV
  ( / ss dEixo dEsq dDir startPt)

  (vl-load-com)

  (setq ss (ssget "_X" '((0 . "AECC_COGO_POINT"))))

  (if ss
    (progn
      (setq dEixo (lv-getkey "\nClique um ponto do EIXO: "))
      (setq dEsq  (lv-getkey "\nClique um ponto da ESQUERDA: "))
      (setq dDir  (lv-getkey "\nClique um ponto da DIREITA: "))
      (setq startPt (getpoint "\nClique o ponto INICIAL (estaca 0): "))

      (lv-draw (lv-order (lv-collect ss dEixo) startPt) "EIXO")
      (lv-draw (lv-order (lv-collect ss dEsq)  startPt) "ESQUERDA")
      (lv-draw (lv-order (lv-collect ss dDir)  startPt) "DIREITA")
    )
    (princ "\nNada selecionado.")
  )

  (princ)
)


;; ===========================================================================
;;  COMANDOS SEPARADOS (mesmo fluxo interativo, um lado por vez)
;; ===========================================================================
(defun c:PONTOS_ACESSO_LV_EIXO () (lv-run-one "EIXO"))
(defun c:PONTOS_ACESSO_LV_ESQ  () (lv-run-one "ESQUERDA"))
(defun c:PONTOS_ACESSO_LV_DIR  () (lv-run-one "DIREITA"))
