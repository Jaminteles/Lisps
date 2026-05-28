(defun c:PontosA2
 ( / ss i obj pts used curr dir next cand ang d
       angLim distLim latLim poly start remaining
       dx dy vx vy lat)

  (vl-load-com)

  (setq angLim (/ (* 70 pi) 180.0)) ; 45°
  (setq distLim 22.0)               ; 22 m
  (setq latLim 4.5)                 ; <<< LIMITE LATERAL

  (princ "\nSelecione os COGO Points...")
  (setq ss (ssget '((0 . "AECC_COGO_POINT"))))

  (if (or (null ss) (< (sslength ss) 2))
    (progn
      (princ "\nSelecione pelo menos dois COGO Points.")
      (princ)
      (return)
    )
  )

  ;; Coleta coordenadas
  (setq pts '())
  (setq i 0)
  (while (< i (sslength ss))
    (setq obj (vlax-ename->vla-object (ssname ss i)))
    (setq pts
      (cons
        (list
          (vlax-get obj 'Easting)
          (vlax-get obj 'Northing)
          (vlax-get obj 'Elevation)
        )
        pts
      )
    )
    (setq i (1+ i))
  )

  (setq used '())

  ;; Enquanto ainda houver pontos livres
  (while (< (length used) (length pts))

    ;; ponto inicial mais ao sul
    (setq remaining (vl-remove-if '(lambda (x) (member x used)) pts))
    (setq start
      (car (vl-sort remaining '(lambda (a b) (< (cadr a) (cadr b)))))
    )

    (setq curr start)
    (setq dir '(0 1 0))
    (setq poly (list curr))
    (setq used (cons curr used))

    ;; cresce a linha
    (while
      (progn
        (setq next nil)

        ;; normaliza direção
        (setq dx (car dir))
        (setq dy (cadr dir))
        (setq mag (sqrt (+ (* dx dx) (* dy dy))))
        (if (> mag 0)
          (progn (setq dx (/ dx mag)) (setq dy (/ dy mag)))
        )

        (foreach p pts
          (if (not (member p used))
            (progn
              (setq cand (mapcar '- p curr))
              (setq d (distance curr p))

              (if (<= d distLim)
                (progn
                  ;; ângulo
                  (setq ang
                    (abs
                      (- (angle '(0 0 0) cand)
                         (angle '(0 0 0) dir))
                    )
                  )
                  (if (> ang pi) (setq ang (- (* 2 pi) ang)))

                  ;; distância lateral
                  (setq vx (- (car p) (car curr)))
                  (setq vy (- (cadr p) (cadr curr)))
                  (setq lat (abs (- (* vx dy) (* vy dx))))

                  ;; critérios finais
                  (if (and (<= ang angLim)
                           (<= lat latLim)
                           (or (null next)
                               (< d (distance curr next))))
                    (setq next p)
                  )
                )
              )
            )
          )
        )
        next
      )

      ;; atualiza
      (setq dir (mapcar '- next curr))
      (setq curr next)
      (setq poly (append poly (list curr)))
      (setq used (cons curr used))
    )

    ;; cria polyline
    (if (> (length poly) 1)
      (progn
        (command "_.3DPOLY")
        (foreach p poly (command p))
        (command "")
      )
    )
  )

  (princ "\n3DPOLYs criadas sem cruzar o eixo.")
  (princ)
)





















(defun orient (a b c)
  (- (* (- (car b) (car a)) (- (cadr c) (cadr a)))
     (* (- (cadr b) (cadr a)) (- (car c) (car a))))
)

(defun intersect-seg (p1 p2 q1 q2)
  (and
    (< (* (orient p1 p2 q1) (orient p1 p2 q2)) 0)
    (< (* (orient q1 q2 p1) (orient q1 q2 p2)) 0)
  )
)

(defun poly-segs (obj / pts segs i)
  (setq pts (vlax-safearray->list
              (vlax-variant-value
                (vla-get-Coordinates obj))))
  (setq segs '())
  (setq i 0)
  (while (< (+ i 3) (length pts))
    (setq segs
      (cons
        (list
          (list (nth i pts) (nth (+ i 1) pts))
          (list (nth (+ i 3) pts) (nth (+ i 4) pts))
        )
        segs))
    (setq i (+ i 3))
  )
  segs
)

(defun cruza-barreira-real (p1 p2 ss / i obj segs s cruzou)
  (setq cruzou nil)
  (setq i 0)
  (while (and (< i (sslength ss)) (not cruzou))
    (setq obj (vlax-ename->vla-object (ssname ss i)))
    (setq segs (poly-segs obj))
    (foreach s segs
      (if (intersect-seg
            (list (car p1) (cadr p1))
            (list (car p2) (cadr p2))
            (car s)
            (cadr s))
        (setq cruzou T)
      )
    )
    (setq i (1+ i))
  )
  cruzou
)

(defun ang-entre (v1 v2)
  (abs
    (atan
      (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
      (+ (* (car v1) (car v2)) (* (cadr v1) (cadr v2)))
    )
  )
)

(defun c:PONTOS_PE_CRISTA
 ( / ssPts ssBar pts used curr next poly
     i obj p distLim angLim remaining start dir cand ang)

  (vl-load-com)

  (setq distLim 22.0)
  (setq angLim (/ (* 30 pi) 180.0)) ;; 30°

  (princ "\nSelecione os COGO Points...")
  (setq ssPts (ssget '((0 . "AECC_COGO_POINT"))))
  (if (< (sslength ssPts) 2) (exit))

  (princ "\nSelecione as linhas que NÃO podem ser cruzadas...")
  (setq ssBar (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE"))))
  (if (null ssBar) (exit))

  ;; coleta pontos
  (setq pts '())
  (setq i 0)
  (while (< i (sslength ssPts))
    (setq obj (vlax-ename->vla-object (ssname ssPts i)))
    (setq pts
      (cons
        (list
          (vlax-get obj 'Easting)
          (vlax-get obj 'Northing)
          (vlax-get obj 'Elevation))
        pts))
    (setq i (1+ i))
  )

  (setq used '())

  (while (< (length used) (length pts))
    (setq remaining (vl-remove-if '(lambda (x) (member x used)) pts))
    (setq start (car (vl-sort remaining '(lambda (a b) (< (cadr a) (cadr b))))))

    (setq curr start)
    (setq poly (list curr))
    (setq used (cons curr used))
    (setq dir nil)

    (while
      (progn
        (setq next nil)
        (foreach p pts
          (if (and (not (member p used))
                   (<= (distance curr p) distLim)
                   (not (cruza-barreira-real curr p ssBar)))
            (progn
              (setq cand (list (- (car p) (car curr))
                               (- (cadr p) (cadr curr))))
              (if (or (null dir)
                      (<= (ang-entre dir cand) angLim))
                (if (or (null next)
                        (< (distance curr p) (distance curr next)))
                  (setq next p)
                )
              )
            )
          )
        )
        next
      )

      (setq dir (list (- (car next) (car curr))
                      (- (cadr next) (cadr curr))))
      (setq curr next)
      (setq poly (append poly (list curr)))
      (setq used (cons curr used))
    )

    (if (> (length poly) 1)
      (progn
        (command "_.3DPOLY")
        (foreach p poly (command p))
        (command "")
      )
    )
  )

  (princ "\n3DPOLYs criadas.")
  (princ)
)