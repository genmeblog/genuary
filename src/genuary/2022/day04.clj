;; The next next Fidenza.
(ns genuary.2022.day04
  (:require [clojure2d.core :as c2d]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.extra.utils :as utils]
            [clojure2d.color :as c]))

(defn iterator
  [[pos angle len]]
  [(v/add pos (v/from-polar (v/vec2 (* 0.1 len) angle))) ;; step
   (+ angle (m/norm (r/noise (pos 0) (pos 1)) 0.0 1.0 -0.8 0.8)) ;; rotation
   (r/vnoise (pos 0) (pos 1))]) ;; length

(def p (c/palette)) ;; constant palette

(c2d/with-canvas [c (c2d/canvas 800 800 :highest)]
  (c2d/set-background c (c/gray 20))
  (dotimes [_ 50]
    (let [init-pos (v/vec2 (r/grand 0.2) (r/grand 0.2)) ;; starting point
          init-len (r/vnoise (init-pos 0) (init-pos 1)) ;; initial length
          pal (r/randval #_(c/palette) p ;; colorful stripes
                         [(c/gray (r/randval 0.25 100 210))])] ;; grayish 
      (doseq [[id col [[x y] angle len]] (take 200 (map vector
                                                        (range) ;; segment
                                                        (cycle pal) ;; segment color
                                                        ;; next position, next angle, next length
                                                        (iterate iterator [init-pos (r/drand m/-PI m/PI) init-len]))) 
              :let [xx (m/norm x -1.5 1.5 0 800) ;; scale to screen
                    yy (m/norm y -1.0 2.0 0 800)
                    l (+ 10.0 (* 15.0 len)) ;; segment length
                    s (m/norm (m/cos (m/norm id 0 200 0.0 m/TWO_PI)) 1.0 -1.0 2 20) ;; segment width
                    ss (* 0.8 s) ;; interior size
                    b (* 0.1 s)]] ;; border size
        (-> c
            (c2d/push-matrix)
            (c2d/translate xx yy)
            (c2d/rotate angle)
            (c2d/set-color :black 100)
            (c2d/rect 0.0 0.0 (+ l b b) s)
            (c2d/set-color col 220)
            (c2d/rect b b l ss)
            (c2d/pop-matrix)))))
  #_(c2d/save c "results/2022/day04d.jpg")
  (utils/show-image c))

