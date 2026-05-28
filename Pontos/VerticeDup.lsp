(defun c:CheckVertDup3D ( / ss i ent pts tol count)

  (vl-load-com)
  (setq tol 0.001)

  ;; pega pontos da 3dpoly
  (defun get-3dpoly-points (ent / pts v data)
    (setq pts '())
    (setq v (entnext ent))
    (while (and v (/= (cdr (assoc 0 (entget v))) "SEQEND"))
      (setq data (entget v))
      (if (= (cdr (assoc 0 data)) "VERTEX")
        (setq pts (cons (cdr (assoc 10 data)) pts))
      )
      (setq v (entnext v))
    )
    (reverse pts)
  )

  ;; compara pontos com tolerância
  (defun pt-eq (a b)
    (and
      (< (abs (- (car a) (car b))) tol)
      (< (abs (- (cadr a) (cadr b))) tol)
      (< (abs (- (caddr a) (caddr b))) tol)
    )
  )

  ;; verifica se tem ponto duplicado na lista
  (defun has-duplicate (lst / p rest found)
    (setq found nil)
    (while (and lst (not found))
      (setq p (car lst))
      (setq rest (cdr lst))

      (if (vl-some '(lambda (x) (pt-eq p x)) rest)
        (setq found T)
      )

      (setq lst rest)
    )
    found
  )

  (setq ss (ssget '((0 . "POLYLINE"))))

  (if ss
    (progn
      (setq count 0)

      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))

        ;; só 3DPOLY
        (if (= (cdr (assoc 70 (entget ent))) 8)
          (progn
            (setq pts (get-3dpoly-points ent))

            (if (has-duplicate pts)
              (progn
                ;; pinta de verde
                (setq ed (entget ent))
                (if (assoc 62 ed)
                  (entmod (subst (cons 62 3) (assoc 62 ed) ed))
                  (entmod (append ed (list (cons 62 3))))
                )
                (setq count (1+ count))
              )
            )
          )
        )
      )

      (prompt (strcat "\nPolylines com vertices duplicados: " (itoa count)))
    )
    (prompt "\nNada selecionado.")
  )

  (princ)
)