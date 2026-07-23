(defun c:SCTOPO (/ arq modo file
                  estTxt cotTxt
                  estVal cotVal
                  estPt cotPt eixoPt
                  pl obj coords
                  lst i p
                  xoff yoff cotaFinal
                  layEst layCot layPl
                  continuar resp e old_error)

  (vl-load-com)

  ;; ================================
  ;; TRATAMENTO DE ERRO
  ;; (evita travar o AutoCAD/Civil3D e evita corromper o
  ;;  arquivo TGS caso algo inesperado aconteça)
  ;; ================================
  (setq old_error *error*)
  (defun *error* (msg)
    (if (and file (eq (type file) 'FILE))
      (close file)
    )
    (setq *error* old_error)
    (if (not (member msg (list "Function cancelled" "quit / exit abort" nil)))
      (princ (strcat "\nSCTOPO - Erro: " msg))
    )
    (princ)
  )

  ;; ================================
  ;; FORMATADORES MANUAIS
  ;; ================================
  (defun fmt4 (n / sgn i d)
    (setq sgn (if (< n 0) "-" ""))
    (setq n (abs n))
    (setq i (fix n))
    (setq d (fix (* (- n i) 10000)))
    (strcat sgn (itoa i) "." (substr (strcat "0000" (itoa d)) (- (strlen (strcat "0000" (itoa d))) 3)))
  )

  (defun fmt3 (n / sgn i d)
    (setq sgn (if (< n 0) "-" ""))
    (setq n (abs n))
    (setq i (fix n))
    (setq d (fix (* (- n i) 1000)))
    (strcat sgn (itoa i) "." (substr (strcat "000" (itoa d)) (- (strlen (strcat "000" (itoa d))) 2)))
  )

  ;; ================================
  ;; TEXTO
  ;; ================================
  (defun getTextValue (e)
    (atof (vla-get-TextString (vlax-ename->vla-object e)))
  )

  (defun getTextPoint (e)
    (vlax-get (vlax-ename->vla-object e) 'InsertionPoint)
  )

  ;; ================================
  ;; SELEÇÃO "À PROVA DE CLIQUE ERRADO"
  ;; Se o usuário clicar em algo errado (tipo/layer incorreto)
  ;; o comando avisa e pede para selecionar de novo, sem sair.
  ;;
  ;; permiteSair = T  -> Enter/clique no vazio é uma saída válida (retorna nil)
  ;; permiteSair = nil -> obrigatório selecionar algo válido, insiste até acertar
  ;; ================================
  (defun PickValid (msg tipos layer permiteSair / sel e ok)
    (setq ok nil e nil)
    (while (not ok)
      (setq sel (entsel msg))
      (cond
        ((null sel)
         (if permiteSair
           (setq ok T)
           (princ "\n>> Nada foi selecionado. Tente novamente.")
         )
        )
        (t
         (setq e (car sel))
         (cond
           ((and tipos (not (member (cdr (assoc 0 (entget e))) tipos)))
            (princ "\n>> Objeto inválido (tipo incorreto). Tente novamente.")
            (setq e nil)
           )
           ((and layer (/= (strcase (cdr (assoc 8 (entget e)))) (strcase layer)))
            (princ (strcat "\n>> O objeto precisa estar no layer \"" layer "\". Tente novamente."))
            (setq e nil)
           )
           (t (setq ok T))
         )
        )
      )
    )
    e
  )

  (defun PickPonto (msg / pt)
    (setq pt nil)
    (while (not pt)
      (setq pt (getpoint msg))
      (if (not pt) (princ "\n>> Ponto inválido. Tente novamente."))
    )
    pt
  )

  ;; ================================
  ;; CONFIGURAÇÃO DE LAYERS (fica salva no Windows/perfil do AutoCAD)
  ;; ================================
  (defun ConfigurarLayers ()
    (setq e (PickValid "\nSelecione um TEXTO de ESTACA (para definir o layer): " (list "TEXT" "MTEXT") nil nil))
    (setq layEst (cdr (assoc 8 (entget e))))
    (setenv "SCTOPO_LAY_ESTACA" layEst)

    (setq e (PickValid "\nSelecione um TEXTO de COTA (para definir o layer): " (list "TEXT" "MTEXT") nil nil))
    (setq layCot (cdr (assoc 8 (entget e))))
    (setenv "SCTOPO_LAY_COTA" layCot)

    (setq e (PickValid "\nSelecione a POLILINHA de TERRENO NATURAL (para definir o layer): " (list "LWPOLYLINE") nil nil))
    (setq layPl (cdr (assoc 8 (entget e))))
    (setenv "SCTOPO_LAY_TERRENO" layPl)

    (princ "\nLayers configurados com sucesso.")
  )

  (setq layEst (getenv "SCTOPO_LAY_ESTACA"))
  (setq layCot (getenv "SCTOPO_LAY_COTA"))
  (setq layPl  (getenv "SCTOPO_LAY_TERRENO"))

  (if (or (null layEst) (= layEst "") (null layCot) (= layCot "") (null layPl) (= layPl ""))
    (progn
      (princ "\n=== SCTOPO: configuração inicial de layers (só é necessário fazer isso uma vez) ===")
      (ConfigurarLayers)
    )
    (progn
      (princ (strcat "\nLayers salvos -> Estaca: " layEst "   Cota: " layCot "   Terreno: " layPl))
      (initget "Sim Nao")
;;     (setq resp (getkword "\nDeseja reconfigurar os layers? [Sim/Nao] <Nao>: "))
;;      (if (= resp "Sim") (ConfigurarLayers))
    )
  )

  ;; ================================
  ;; ARQUIVO
  ;; ================================
  (initget "Novo Continuação")
  (setq modo (getkword "\nArquivo [Novo/Continuação] <Continuação>: "))
  (if (null modo) (setq modo "Continuação"))

  (setq arq (getfiled "Arquivo TGS" "" "tgs" (if (= modo "Novo") 1 0)))
  (if (null arq)
    (progn
      (princ "\nOperação cancelada pelo usuário.")
      (exit)
    )
  )

  (setq file (open arq (if (= modo "Novo") "w" "a")))

  (write-line "*1" file)

  ;; ================================
  ;; LOOP PRINCIPAL
  ;; ================================
  (setq continuar T)
  (while continuar

    (setq estTxt (PickValid "\nSELECIONE O TEXTO Estaca <Enter para sair>: " (list "TEXT" "MTEXT") layEst nil))

    (if (null estTxt)
      (setq continuar nil)
      (progn
        (setq estVal (vla-get-TextString (vlax-ename->vla-object estTxt)))

        (setq cotTxt (PickValid "\nSELECIONE O TEXTO Cota: " (list "TEXT" "MTEXT") layCot nil))
        (setq cotVal (getTextValue cotTxt))
        (setq cotPt  (getTextPoint cotTxt))

        (setq eixoPt (PickPonto "\nPONTO DE REFERENCIA (EIXO): "))

        (setq pl (PickValid "\nSelecione a polilinha do terreno natural: " (list "LWPOLYLINE") layPl nil))
        (setq obj (vlax-ename->vla-object pl))
        (setq coords (vlax-get obj 'Coordinates))

        ;; Vértices
        (setq lst '() i 0)
        (while (< i (length coords))
          (setq lst (cons (list (nth i coords) (nth (+ i 1) coords)) lst))
          (setq i (+ i 2))
        )

        (setq lst (vl-sort lst '(lambda (a b) (< (car a) (car b)))))

        ;; Cabeçalho
        (write-line
          (strcat ">" estVal "\t" (fmt4 (length lst)))
          file
        )

        ;; Pontos
        (setq i 1)
        (foreach p lst
          (setq xoff (- (car p) (car eixoPt)))
          (setq yoff (- (cadr p) (cadr cotPt)))
          (setq cotaFinal (+ cotVal yoff))

          (write-line
            (strcat
              (fmt4 i) "\t\t"
              (fmt3 xoff) "\t"
              (fmt3 cotaFinal)
            )
            file
          )

          (setq i (1+ i))
        )

        ;; Espaço entre estacas
        (write-line "" file)
        (write-line "" file)
      )
    )
  )

  (close file)
  (setq file nil)
  (setq *error* old_error)
  (princ "\nArquivo TGS gerado corretamente.")
  (princ)
)

;; ================================
;; Comando auxiliar: apaga os layers salvos, caso precise
;; reconfigurar do zero (ex: layers com nomes diferentes)
;; ================================
(defun c:SCTOPORESET ()
  (setenv "SCTOPO_LAY_ESTACA" "")
  (setenv "SCTOPO_LAY_COTA" "")
  (setenv "SCTOPO_LAY_TERRENO" "")
  (princ "\nLayers do SCTOPO foram apagados. Rode SCTOPO novamente para reconfigurar.")
  (princ)
)

(princ "\nSCTOPO carregado. Digite SCTOPO para iniciar ou SCTOPORESET para reconfigurar layers.")
(princ)