(setq *seed* (fix (* (getvar "DATE") 1000000)))

(defun rand ( / )
  (setq *seed* (rem (+ (* *seed* 1664525) 1013904223) 4294967296))
  (/ *seed* 4294967296.0)
)

;; =========================
;; SELEÇÃO SEGURA (RETORNA NIL SE ESC)
;; =========================
(defun getValidEnt (msg validTypes / ent sel obj)
  (while
    (progn
      (setq sel (vl-catch-all-apply 'entsel (list msg)))

      (cond
        ;; ESC → sai geral
        ((vl-catch-all-error-p sel)
          (prompt "\nCancelado.")
          (setq ent nil)
          nil
        )

        ;; nada selecionado
        ((null sel)
          (prompt "\nNenhuma entidade selecionada.")
          T
        )

        ;; valida tipo
        (T
          (setq ent (car sel))
          (setq obj (cdr (assoc 0 (entget ent))))

          (if (member obj validTypes)
            nil
            (progn
              (prompt "\nTipo inválido. Tente novamente...")
              T
            )
          )
        )
      )
    )
  )
  ent
)

;; =========================
;; COMANDO PRINCIPAL EM LOOP
;; =========================
(defun c:ZLINHA_RANDOMICA ( / entBase entEixo objBase eixoX len dir
                             numPts i dist pt randOffset randY pts midIndex
                             yPrev yNext ySmooth
                             p0 p1 pN pN1)

  (while T  ;; 🔥 LOOP CONTÍNUO
    (prompt "\n--- Nova execução (ESC para sair) ---")

    ;; seleção
    (setq entBase (getValidEnt "\nSelecione a linha base (LINE ou PL): "
                              '("LINE" "LWPOLYLINE" "POLYLINE")))

    (if (null entBase) (progn (prompt "\nSaindo...") (exit)))

    (setq entEixo (getValidEnt "\nSelecione a LINE do eixo: "
                              '("LINE")))

    (if (null entEixo) (progn (prompt "\nSaindo...") (exit)))

    ;; =========================
    ;; PROCESSAMENTO
    ;; =========================
    (setq objBase (vlax-ename->vla-object entBase))

    (setq len (vlax-curve-getDistAtParam objBase
                (vlax-curve-getEndParam objBase)))

    (setq dir
      (angle
        (vlax-curve-getPointAtDist objBase 0)
        (vlax-curve-getPointAtDist objBase 0.1)
      )
    )

    (setq eixoX (car (cdr (assoc 10 (entget entEixo)))))

    (setq numPts (+ 5 (fix (* (rand) 4))))
    (setq midIndex (fix (/ numPts 2)))

    (setq i 0)
    (setq pts '())

    (while (<= i numPts)
      (setq dist (* (/ (float i) numPts) len))
      (setq pt (vlax-curve-getPointAtDist objBase dist))

      (if (= i midIndex)
        (progn
          (setq yPrev (if pts (cadr (last pts)) (cadr pt)))
          (setq yNext (cadr pt))
          (setq ySmooth (/ (+ yPrev yNext) 2.0))
          (setq pt (list eixoX ySmooth))
        )
        (progn
          (setq randOffset (+ 0.2 (* (rand) 0.2)))
          (setq randY (- (* (rand) 0.2) 0.1))

          (setq pt
            (polar pt
                   (- dir (/ pi 2))
                   (+ randOffset randY)
            )
          )
        )
      )

      (setq pts (append pts (list pt)))
      (setq i (1+ i))
    )

    ;; =========================
    ;; AJUSTE DAS PONTAS
    ;; =========================
    (setq p0 (nth 0 pts))
    (setq p1 (nth 1 pts))
    (setq pN (nth (1- (length pts)) pts))
    (setq pN1 (nth (- (length pts) 2) pts))

    (if (<= (cadr p0) (cadr p1))
      (setq p0 (list (car p0) (+ (cadr p1) 0.2)))
    )

    (if (<= (cadr pN) (cadr pN1))
      (setq pN (list (car pN) (+ (cadr pN1) 0.2)))
    )

    (setq pts
      (append
        (list p0)
        (cdr (reverse (cdr (reverse pts))))
        (list pN)
      )
    )

    ;; =========================
    ;; CRIA POLYLINE
    ;; =========================
    (command "_.PLINE")
    (foreach p pts
      (command p)
    )
    (command "")
  )

  (princ)
)