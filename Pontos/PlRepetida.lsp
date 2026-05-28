(defun c:CheckDup3DPoly ( / ss i ent ptsList dupCount tol)

  (vl-load-com)
  (setq tol 0.001)

  ;; pega pontos corretamente (para no SEQEND)
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

  ;; compara dois pontos com tolerância
  (defun pt-eq (a b)
    (and
      (< (abs (- (car a) (car b))) tol)
      (< (abs (- (cadr a) (cadr b))) tol)
      (< (abs (- (caddr a) (caddr b))) tol)
    )
  )

  ;; compara listas de pontos
  (defun pts-equal (l1 l2 / ok)
    (if (/= (length l1) (length l2))
      nil
      (progn
        (setq ok T)
        (while (and l1 ok)
          (if (not (pt-eq (car l1) (car l2)))
            (setq ok nil)
          )
          (setq l1 (cdr l1))
          (setq l2 (cdr l2))
        )
        ok
      )
    )
  )

  ;; considera ordem invertida também
  (defun poly-equal (a b)
    (or
      (pts-equal a b)
      (pts-equal a (reverse b))
    )
  )

  (setq ss (ssget '((0 . "POLYLINE"))))

  (if ss
    (progn
      (setq ptsList '())
      (setq dupCount 0)

      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))

        (if (= (cdr (assoc 70 (entget ent))) 8)
          (progn
            (setq pts (get-3dpoly-points ent))

            (if (vl-some '(lambda (p) (poly-equal p pts)) ptsList)
              (progn
                ;; pinta de verde (3)
                (setq ed (entget ent))
                (if (assoc 62 ed)
                  (entmod (subst (cons 62 3) (assoc 62 ed) ed))
                  (entmod (append ed (list (cons 62 3))))
                )
                (setq dupCount (1+ dupCount))
              )
              (setq ptsList (cons pts ptsList))
            )
          )
        )
      )

      (prompt (strcat "\nDuplicadas encontradas: " (itoa dupCount)))
    )
    (prompt "\nNada selecionado.")
  )

  (princ)
)