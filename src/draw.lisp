(in-package #:chrono-labyrinth)

;; TODO figure out sketch figures?
(defun draw-clock (clock w h &aux (time (sc:time clock)))
  #+(or)  ; debug border
  (s+:with-color (s:+red+ :stroke)
    (s:rect 0 0 w h))
  (s+:with-fit (200 200 w h)
    (s:with-translate (100 100)
      (s:with-current-matrix
        (dotimes (_ 24)
          (s:rect 70 -7 30 14)
          (s:rotate (/ 360 24))))
      (s:with-pen (s:make-pen :stroke s:+yellow+ :weight 15)
        (s:with-rotate ((* 360 (/ time 24)))
          (s:line 20 0 60 0))))))

(defun draw-game (w h)
  (s+:with-fit (800 800 w h)
    (s:background s:+black+)
    (s:with-translate (200 200)
      (draw-clock *game-clock* 400 400))
    (s:with-font (s:make-font :size 80 :align :center :color s:+white+)
      (s:text "Chrono Labyrinth" 400 650))))
