(defun c:HTxt ( / estEnt estaca
               ent ent2
               area1 area2
               pt pt2
               sel obj tipo layer continuar
               arq path linha)

  (vl-load-com)

  ;; ================================
  ;; Funções auxiliares
  ;; ================================

  (defun get-area (e)
    (if (and e (vlax-property-available-p (vlax-ename->vla-object e) 'Area))
      (vlax-get (vlax-ename->vla-object e) 'Area)
      nil
    )
  )

  (defun get-layer (e)
    (cdr (assoc 8 (entget e)))
  )

  (defun format-area-str (area / inteira decimal)
    (if area
      (progn
        (setq inteira (fix area))
        (setq decimal (fix (* (- area inteira) 100)))
        (strcat
          (itoa inteira)
          ","
          (if (< decimal 10)
            (strcat "0" (itoa decimal))
            (itoa decimal)
          )
        )
      )
      "0,00"
    )
  )

  (defun get-text-value (e)
    (cond
      ((= (cdr (assoc 0 (entget e))) "TEXT")
       (cdr (assoc 1 (entget e))))
      ((= (cdr (assoc 0 (entget e))) "MTEXT")
       (cdr (assoc 1 (entget e))))
      (T "")
    )
  )

  (defun get-text-style ()
    (if (tblsearch "style" "ARIAL") "ARIAL" "Standard")
  )

  ;; ================================
  ;; SELEÇÃO DA ESTACA
  ;; ================================
  (while (null estEnt)
    (setq sel (entsel "\nSelecione o TEXTO da ESTACA: "))
    (if sel
      (progn
        (setq obj (car sel))
        (setq tipo (cdr (assoc 0 (entget obj))))
        (if (member tipo '("TEXT" "MTEXT"))
          (setq estEnt obj)
          (prompt "\n>> Apenas TEXT ou MTEXT sao permitidos.")
        )
      )
      (prompt "\n>> Selecione um texto valido.")
    )
  )

  (setq estaca (get-text-value estEnt))

  ;; ================================
  ;; Ponto base
  ;; ================================
  (setq pt (getpoint "\nClique o ponto onde inserir as anotacoes: "))

  ;; ================================
  ;; SELECAO AUTOMATICA (ATERRO / CORTE)
  ;; Sai sozinho quando tiver os 2 OU se o usuario der ENTER
  ;; ================================
  (setq ent nil ent2 nil continuar T)
  (prompt "\nSelecione os HATCHES (Aterro/Corte). Para apenas um, ENTER finaliza.")

  (while (and continuar (or (null ent) (null ent2)))
    (setvar 'errno 0)
    (setq sel (entsel "\nClique no hatch: "))
    (cond
      ;; Pegou alguma entidade
      (sel
        (setq obj  (car sel))
        (setq tipo (cdr (assoc 0 (entget obj))))
        (if (/= tipo "HATCH")
          (prompt "\n>> Apenas HATCH e permitido.")
          (progn
            (setq layer (get-layer obj))
            (cond
              ((= layer "Aterro")
                (setq ent obj)
                (prompt "\n>> Aterro identificado.")
              )
              ((= layer "Corte")
                (setq ent2 obj)
                (prompt "\n>> Corte identificado.")
              )
              (T
                (prompt "\n>> Layer invalido (use Aterro ou Corte).")
              )
            )
          )
        )
      )
      ;; Clicou no vazio (ERRNO = 7) -> apenas avisa e continua
      ((= (getvar 'errno) 7)
        (prompt "\n>> Nada selecionado. Clique em um hatch ou ENTER para finalizar.")
      )
      ;; ENTER ou Esc (ERRNO = 52) -> finaliza o loop
      (T
        (setq continuar nil)
      )
    )
  ) ;; fim do while

  ;; ================================
  ;; AREAS FORMATADAS
  ;; ================================
  (setq area1 (format-area-str (get-area ent)))
  (setq area2 (format-area-str (get-area ent2)))

  ;; ================================
  ;; VALIDACAO + CRIACAO DE TEXTOS + GRAVACAO TXT
  ;; ================================
  (if (and (= area1 "0,00") (= area2 "0,00"))
    (prompt "\n>> Nenhuma area valida (aterro e corte = 0). Operacao cancelada.")
    (progn
      ;; ATERRO
      (setq pt2 (list (car pt) (- (cadr pt) 1.5) (caddr pt)))
      (entmake
        (list
          '(0 . "TEXT")
          (cons 10 pt2)
          '(40 . 0.4)
          (cons 1 (strcat "AREA ATERRO = " area1 " m2"))
          (cons 7 (get-text-style))
          '(62 . 40)
        )
      )

      ;; CORTE
      (setq pt2 (list (car pt) (- (cadr pt) 2.5) (caddr pt)))
      (entmake
        (list
          '(0 . "TEXT")
          (cons 10 pt2)
          '(40 . 0.4)
          (cons 1 (strcat "AREA CORTE = " area2 " m2"))
          (cons 7 (get-text-style))
          '(62 . 20)
        )
      )

      ;; GRAVA TXT
      (setq path (strcat (getvar "DWGPREFIX") "areas_estacas.txt"))
      (setq arq  (open path "a"))
      (if arq
        (progn
          (setq linha (strcat estaca ";" area1 ";" area2 ";"))
          (write-line linha arq)
          (close arq)
          (prompt (strcat "\n>> Linha gravada em: " path))
        )
        (prompt "\n>> Nao foi possivel abrir o arquivo TXT.")
      )
    )
  )

  (princ)
)