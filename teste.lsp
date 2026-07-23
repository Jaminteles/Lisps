;;; ------------------------------------------------------------
;;; LinhaHatch.lsp
;;; Seleciona hatches e desenha uma linha verde vertical (2D)
;;; para cima no centro dos hatches com area > 100.
;;; Comando: LH
;;; ------------------------------------------------------------

(defun c:LH ( / doc ms comprimento ss i ent obj area
                minpt maxpt cx cy p1 p2 ln cont)
  (vl-load-com)

  ;; >>> Ajuste aqui <<<
  (setq comprimento 100.0)   ; comprimento da linha (unidades do desenho)
  ;; --------------------

  (setq doc  (vla-get-ActiveDocument (vlax-get-acad-object))
        ms   (vla-get-ModelSpace doc)
        cont 0)

  (princ "\nSelecione os hatches: ")
  (if (setq ss (ssget '((0 . "HATCH"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              obj (vlax-ename->vla-object ent))

        ;; pega a area com protecao contra hatch invalido
        (setq area (vl-catch-all-apply 'vla-get-Area (list obj)))

        (if (and (not (vl-catch-all-error-p area)) (> area 100.0))
          (progn
            ;; centro pelo bounding box
            (vla-GetBoundingBox obj 'minpt 'maxpt)
            (setq minpt (vlax-safearray->list minpt)
                  maxpt (vlax-safearray->list maxpt)
                  cx    (/ (+ (car  minpt) (car  maxpt)) 2.0)
                  cy    (/ (+ (cadr minpt) (cadr maxpt)) 2.0)
                  p1    (list cx cy 0.0)
                  p2    (list cx (+ cy comprimento) 0.0)
                  ln    (vla-AddLine ms
                          (vlax-3d-point p1)
                          (vlax-3d-point p2)))
            (vla-put-Color ln 3)   ; 3 = verde
            (setq cont (1+ cont))
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n" (itoa cont) " linha(s) criada(s) em hatch(es) com area > 100."))
    )
    (princ "\nNenhum hatch selecionado.")
  )
  (princ)
)
(princ "\nLinhaHatch.lsp carregado. Digite LH para executar.")
(princ)