(defun get-raw-desc (ent)
  (if ent
    (vl-catch-all-apply
      '(lambda ()
         (vlax-get (vlax-ename->vla-object ent) 'RawDescription)
       )
    )
  )
)

(defun get-cogo-point (ent)
  (if ent
    (vl-catch-all-apply
      '(lambda ()
         (vlax-get (vlax-ename->vla-object ent) 'Location)
       )
    )
  )
)

(defun ponto-inicial-p (desc)
  (and desc (wcmatch (strcase desc) "*INICIAL*"))
)

(defun ponto-final-p (desc)
  (and desc (wcmatch (strcase desc) "*FINAL*"))
)

(defun selecionar-ponto-valido (msg tipo-func / sel ent desc)
  (while
    (progn
      (setq sel (entsel msg))

      (cond
        ((not sel)
          (prompt "\n❌ Clique obrigatório!")
          T
        )

        ((not (setq ent (car sel)))
          T
        )

        ((not (setq desc (get-raw-desc ent)))
          (prompt "\n❌ Objeto não possui Raw Description!")
          T
        )

        ((not (apply tipo-func (list desc)))
          (prompt "\n❌ Tipo incorreto!")
          T
        )

        (T nil)
      )
    )
  )
  ent
)

(defun c:InicioFim ( / ent eixo staIni
                              pt1 pt2 cp1 cp2
                              sta1 sta2 ini1 ini2
                              frac1 frac2 fracStr1 fracStr2
                              lado linhas arq ini-arq fim-arq
                              ent1 ent2)

  (vl-load-com)

  ;; eixo
  (setq ent (car (entsel "\nSelecione o EIXO: ")))
  (setq eixo (vlax-ename->vla-object ent))
  (setq staIni (vlax-get eixo 'StartingStation))

  ;; cabeçalho
  (setq linhas
    (list "INICIAL;+;FRACAO;FINAL;+;FRACAO;LADO"))

  (prompt "\n--- SELECIONE PONTO INICIAL E FINAL ---")

  ;; loop com captura de ESC
  (if (vl-catch-all-error-p
    (setq resultado
      (vl-catch-all-apply
        '(lambda ()
           (while T

             ;; seleção segura
             (setq ent1 (selecionar-ponto-valido "\nSelecione o ponto INICIAL: " 'ponto-inicial-p))
             (setq ent2 (selecionar-ponto-valido "\nSelecione o ponto FINAL: " 'ponto-final-p))

             ;; coordenadas corretas (COGO)
             (setq pt1 (get-cogo-point ent1))
             (setq pt2 (get-cogo-point ent2))

             ;; proteção extra
             (if (or (null pt1) (null pt2))
               (prompt "\n❌ Erro ao obter coordenadas dos pontos!")

               (progn
                 ;; projeção
                 (setq cp1 (vlax-curve-getClosestPointTo eixo pt1))
                 (setq cp2 (vlax-curve-getClosestPointTo eixo pt2))

                 ;; estações
                 (setq sta1 (+ staIni (vlax-curve-getDistAtPoint eixo cp1)))
                 (setq sta2 (+ staIni (vlax-curve-getDistAtPoint eixo cp2)))

                 ;; inicial
                 (setq ini1 (fix (/ sta1 20.0)))
                 (setq frac1 (rem sta1 20.0))
                 (setq fracStr1 (ponto->virgula (rtos frac1 2 2)))

                 ;; final
                 (setq ini2 (fix (/ sta2 20.0)))
                 (setq frac2 (rem sta2 20.0))
                 (setq fracStr2 (ponto->virgula (rtos frac2 2 2)))

                 ;; controle arquivo
                 (if (null ini-arq)
                   (setq ini-arq ini1)
                 )
                 (setq fim-arq ini2)

                 ;; lado (baseado no inicial)
                 (setq lado (lado-offset eixo pt1 cp1))

                 ;; adiciona linha
                 (setq linhas
                   (append linhas
                     (list
                       (strcat
                         (itoa ini1) ";+;" fracStr1 ";"
                         (itoa ini2) ";+;" fracStr2 ";"
                         lado
                       )
                     )
                   )
                 )

                 (prompt "\nAdicionado!")
               )
             )
           )
         )
      )
    )
  )
    ;; ESC foi pressionado (erro capturado) - gera CSV
    (progn
      (if ini-arq
        (progn
          (setq arq (salvar-csv linhas ini-arq fim-arq))
          (alert (strcat "CSV gerado:\n" arq))
        )
        (alert "Nenhum trecho foi adicionado.")
      )
    )
  )

  (princ)
)