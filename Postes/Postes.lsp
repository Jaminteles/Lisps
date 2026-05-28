(defun lado-offset (eixo pt cp / tan v1 v2 cruz)
  ;; vetor tangente do eixo
  (setq tan (vlax-curve-getFirstDeriv eixo (vlax-curve-getParamAtPoint eixo cp)))
  ;; vetor do eixo até o ponto
  (setq v1 tan)
  (setq v2 (mapcar '- pt cp))
  ;; produto vetorial Z
  (setq cruz (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2))))
  (if (> cruz 0) "E" "D")
)

(defun lado-offset (eixo pt cp / tan v2 cruz)
  (setq tan (vlax-curve-getFirstDeriv eixo (vlax-curve-getParamAtPoint eixo cp)))
  (setq v2 (mapcar '- pt cp))
  (setq cruz (- (* (car tan) (cadr v2)) (* (cadr tan) (car v2))))
  (if (> cruz 0) "E" "D")
)

(defun c:POSTES_TXT ( / ent eixo staIni pt cp sta off lado estaca)

  (vl-load-com)

  ;; Seleciona eixo
  (prompt "\nSelecione o EIXO (Alignment): ")
  (setq ent (car (entsel)))
  (setq eixo (vlax-ename->vla-object ent))
  (setq staIni (vlax-get eixo 'StartingStation))

  (princ "\nESTACA\tOFFSET\tLADO\tNORTE\tESTE")
  (prompt "\n--- CLIQUE NOS POSTES (F3 ATIVO) / ENTER PARA FINALIZAR ---")

  ;; Clique livre com OSNAP
  (while (setq pt (getpoint "\nClique no poste: "))
    ;; Projeta no eixo
    (setq cp (vlax-curve-getClosestPointTo eixo pt))
    (setq sta (+ staIni (vlax-curve-getDistAtPoint eixo cp)))
    (setq off (distance pt cp))
    (setq lado (lado-offset eixo pt cp))

    ;; DEBUG
    (prompt (strcat "\nDEBUG - pt: " (vl-princ-to-string pt) " | cp: " (vl-princ-to-string cp) " | off: " (rtos off 2 2)))

    ;; Estaca formatada (intervalo de 20m)
    (setq estaca
      (strcat
        (itoa (fix (/ sta 20)))
        "+"
        (rtos (rem sta 20) 2 3)
      )
    )

    ;; Saída
    (princ
      (strcat
        "\n"
        estaca "\t"
        (rtos off 2 2) "\t"
        lado "\t"
        (rtos (cadr pt) 2 3) "\t"
        (rtos (car pt) 2 3)
      )
    )
  )

  (princ "\n--- FIM ---")
  (princ)
)

(defun lado-offset (eixo pt cp / tan v cruz)
  (setq tan (vlax-curve-getFirstDeriv eixo (vlax-curve-getParamAtPoint eixo cp)))
  (setq v (mapcar '- pt cp))
  (setq cruz (- (* (car tan) (cadr v)) (* (cadr tan) (car v))))
  (if (> cruz 0) "E" "D")
)

(defun lado-offset (eixo pt cp / tan v cruz)
  (setq tan (vlax-curve-getFirstDeriv eixo (vlax-curve-getParamAtPoint eixo cp)))
  (setq v (mapcar '- pt cp))
  (setq cruz (- (* (car tan) (cadr v)) (* (cadr tan) (car v))))
  (if (> cruz 0) "LE" "LD")
)

(defun pt-virgula (str)
  (vl-string-subst "," "." str)
)

(defun lado-offset (eixo pt cp / tan v cruz)
  ;; Determina se o ponto está à esquerda ou direita do eixo
  (setq tan (vlax-curve-getFirstDeriv
              eixo
              (vlax-curve-getParamAtPoint eixo cp)))
  (setq v (mapcar '- pt cp))
  (setq cruz (- (* (car tan) (cadr v)) (* (cadr tan) (car v))))
  (if (> cruz 0) "LE" "LD")
)

(defun ponto->virgula (str)
  (vl-string-subst "," "." str)
)

(defun lado-offset (eixo pt cp / tan v cruz)
  (setq tan (vlax-curve-getFirstDeriv
              eixo
              (vlax-curve-getParamAtPoint eixo cp)))
  (setq v (mapcar '- pt cp))
  (setq cruz (- (* (car tan) (cadr v)) (* (cadr tan) (car v))))
  (if (> cruz 0) "LE" "LD")
)

(defun ponto->virgula (str)
  (vl-string-subst "," "." str)
)

(defun salvar-csv (linhas / arq f)
  (setq arq (strcat (getvar "DWGPREFIX") "postes_eixo.csv"))
  (setq f (open arq "w"))
  (foreach l linhas (write-line l f))
  (close f)
  arq
)

(defun lado-offset (eixo pt cp / tan v cruz)
  (setq tan (vlax-curve-getFirstDeriv
              eixo
              (vlax-curve-getParamAtPoint eixo cp)))
  (setq v (mapcar '- pt cp))
  (setq cruz (- (* (car tan) (cadr v)) (* (cadr tan) (car v))))
  (if (> cruz 0) "LE" "LD")
)

(defun ponto->virgula (str)
  (vl-string-subst "," "." str)
)

(defun salvar-csv (linhas ini fim / arq nome f)
  (setq nome
    (strcat
      "postes_eixo_"
      (itoa ini) "_"
      (itoa fim)
      ".csv"
    )
  )
  (setq arq (strcat (getvar "DWGPREFIX") nome))
  (setq f (open arq "w"))
  (foreach l linhas (write-line l f))
  (close f)
  arq
)

(defun c:POSTES_CSV ( / *error* ent eixo staIni
                        pt cp sta inicial fracao fracaoStr
                        off lado norte este
                        linhas arq
                        ini-arq fim-arq)

  (vl-load-com)

  ;; Tratamento de erro
  (defun *error* (msg)
    (if (and linhas ini-arq fim-arq)
      (progn
        (setq arq (salvar-csv linhas ini-arq fim-arq))
        (alert
          (strcat
            "⚠ Comando interrompido.\n\n"
            "Arquivo salvo:\n"
            arq "\n\n"
            "Mensagem:\n" msg
          )
        )
      )
    )
    (princ)
  )

  ;; Seleciona eixo
  (prompt "\nSelecione o EIXO (Alignment): ")
  (setq ent (car (entsel)))
  (setq eixo (vlax-ename->vla-object ent))
  (setq staIni (vlax-get eixo 'StartingStation))

  ;; Cabeçalho
  (setq linhas
    (list "INICIAL;+;FRACAO;NORTE;ESTE;LADO;DISTANCIA"))

  (prompt "\n--- CLIQUE NOS POSTES (F3 ATIVO) / ENTER PARA FINALIZAR ---")

  ;; Loop
  (while (setq pt (getpoint "\nClique no poste: "))
    (setq cp (vlax-curve-getClosestPointTo eixo pt))
    (setq sta (+ staIni (vlax-curve-getDistAtPoint eixo cp)))

    ;; Estaca (20 m)
    (setq inicial (fix (/ sta 20.0)))
    (setq fracao (rem sta 20.0))
    (setq fracaoStr (ponto->virgula (rtos fracao 2 2)))

    ;; Guarda inicial do primeiro e último ponto
    (if (null ini-arq)
      (setq ini-arq inicial)
    )
    (setq fim-arq inicial)

    ;; Offset
    (setq off (distance pt cp))
    (setq lado (lado-offset eixo pt cp))

    ;; Coordenadas
    (setq norte (cadr pt))
    (setq este  (car pt))

    ;; Linha CSV
    (setq linhas
      (append linhas
        (list
          (strcat
            (itoa inicial) ";"
            "+" ";"
            fracaoStr ";"
            (ponto->virgula (rtos norte 2 3)) ";"
            (ponto->virgula (rtos este  2 3)) ";"
            lado ";"
            (ponto->virgula (rtos off 2 2))
          )
        )
      )
    )
  )

  ;; Final normal
  (if (and ini-arq fim-arq)
    (progn
      (setq arq (salvar-csv linhas ini-arq fim-arq))
      (alert (strcat "CSV gerado com sucesso:\n" arq))
    )
  )

  (princ)
)












(defun c:LAYERTOSHEET ( / ent entData layerName filePath file)

  ;; Seleciona a entidade
  (setq ent (car (entsel "\nClique no ponto/objeto: ")))

  (if ent
    (progn
      ;; Pega os dados da entidade
      (setq entData (entget ent))
      (setq layerName (cdr (assoc 8 entData)))

      ;; Caminho do arquivo CSV
      (setq filePath (strcat (getvar "DWGPREFIX") "layers_export.csv"))

      ;; Abre ou cria o arquivo
      (setq file (open filePath "a"))

      ;; Escreve no CSV
      (write-line layerName file)

      ;; Fecha arquivo
      (close file)

      (princ (strcat "\nLayer salvo: " layerName))
    )
    (princ "\nNenhum objeto selecionado.")
  )

  (princ)
)