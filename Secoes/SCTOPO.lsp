(defun c:SCTOPO (/ arq modo file
                  estTxt cotTxt
                  estVal cotVal
                  estPt cotPt eixoPt
                  pl obj coords
                  lst i p
                  xoff yoff cotaFinal)

  (vl-load-com)

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
  ;; ARQUIVO
  ;; ================================
  (initget "Novo Continuação")
  (setq modo (getkword "\nArquivo [Novo/Continuação] <Novo>: "))
  (if (null modo) (setq modo "Novo"))

  (setq arq (getfiled "Arquivo TGS" "" "tgs" (if (= modo "Novo") 1 0)))
  (if (null arq) (exit))

  (setq file (open arq (if (= modo "Novo") "w" "a")))

  (write-line "*1" file)

  ;; ================================
  ;; LOOP PRINCIPAL
  ;; ================================
  (while
    (setq estTxt (car (entsel "\nSELECIONE O TEXTO Estaca <Enter para sair>: ")))

    (setq estVal (vla-get-TextString (vlax-ename->vla-object estTxt)))

    (setq cotTxt (car (entsel "\nSELECIONE O TEXTO Cota: ")))
    (setq cotVal (getTextValue cotTxt))
    (setq cotPt  (getTextPoint cotTxt))

    (setq eixoPt (getpoint "\nPONTO DE REFERENCIA (EIXO): "))

    (setq pl (car (entsel "\nSelecione a polilinha do terreno natural: ")))
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

  (close file)
  (princ "\nArquivo TGS gerado corretamente.")
  (princ)
)
