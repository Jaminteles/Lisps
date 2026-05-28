(defun c:QSubir ( / entNova entProj objProj fator pts newPts pt ptProj x yNova yProj novoY)

  ;; 🔧 ajuste aqui uma vez só
  ;; fator de aproximação (0.0 a 1.0)
  (setq fator (getreal "\nFator de aproximação (0.0 a 1.0): "))
  (if (null fator) (setq fator 0.5))
  (setq distMin 0.37)
  (setq distMax 0.5)
  
  (while T
    (prompt "\n--- Nova execução (ESC para sair) ---")

    ;; seleção
    (setq entNova (car (entsel "\nSelecione a linha NOVA: ")))
    (if (null entNova) (exit))

    (setq entProj (car (entsel "\nSelecione a linha do PROJETO: ")))
    (if (null entProj) (exit))

    (setq objProj (vlax-ename->vla-object entProj))

    ;; =========================
    ;; PEGA VÉRTICES (100% confiável)
    ;; =========================
    (setq pts
      (mapcar 'cdr
        (vl-remove-if-not
          '(lambda (x) (= (car x) 10))
          (entget entNova)
        )
      )
    )

    ;; =========================
    ;; AJUSTA OS PONTOS
    ;; =========================
    (setq newPts '())

    (foreach pt pts
      (setq ptProj (vlax-curve-getClosestPointTo objProj pt))

      (setq x (car pt))
      (setq yNova (cadr pt))
      (setq yProj (cadr ptProj))

    (setq distAtual (distance pt ptProj))

    ;; Se maior que o fator de ajuste, aplica correção
    (if (< distAtual distMax)
      ;; 🔒 já está perto → não mexe
      (setq novoY yNova)

      ;; senão aplica ajuste
      (progn
        (setq novoY (+ yNova (* fator (- yProj yNova))))

        ;; limite de distância mínima
        (if (< yNova yProj)
          (setq novoY (min novoY (- yProj distMin)))
          (setq novoY (max novoY (+ yProj distMin)))
        )
      )
    )

      (setq newPts (append newPts (list (list x novoY))))
    )

    ;; =========================
    ;; CRIA NOVA POLYLINE
    ;; =========================
    (command "_.PLINE")
    (foreach p newPts
      (command p)
    )
    (command "")
  )

  (princ)
)