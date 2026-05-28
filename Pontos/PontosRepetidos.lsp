(defun c:CheckCogoDup ( / ss i obj pt lst tol key found dup)
  (vl-load-com)

  (setq tol 0.001)
  (setq lst '())
  (setq dup '())

  (setq ss (ssget "X" '((0 . "AECC_COGO_POINT"))))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (setq pt (vlax-get obj 'Location))

        ;; chave com tolerância
        (setq key (list
                    (fix (/ (car pt) tol))
                    (fix (/ (cadr pt) tol))
                  )
        )

        ;; verifica se já existe
        (setq found nil)
        (foreach item lst
          (if (equal key (car item))
            (progn
              (setq dup (cons (vlax-vla-object->ename obj) dup))
              (setq found T)
            )
          )
        )

        ;; adiciona se não existir
        (if (not found)
          (setq lst (cons (cons key obj) lst))
        )

        (setq i (1+ i))
      )

      ;; seleção dos duplicados
      (if dup
        (progn
          (sssetfirst nil (ssadd))
          (foreach e dup
            (ssadd e (ssget "P"))
          )
          (princ (strcat "\nDuplicados encontrados: " (itoa (length dup))))
        )
        (princ "\nNenhum duplicado encontrado.")
      )
    )
    (princ "\nNenhum COGO Point encontrado.")
  )

  (princ)
)