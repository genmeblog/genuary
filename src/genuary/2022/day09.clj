;; Architecture.
;; https://archive.bridgesmathart.org/2014/bridges2014-79.pdf
(ns genuary.2022.day09
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [clojure2d.color :as c]            
            [clojure2d.extra.utils :as utils]
            [clojure2d.pixels :as p]
            [clojure2d.extra.overlays :as o]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def p1 [(v/vec2 0.0 0.5)
       (v/vec2 m/SQRT3_2 0.0)
       (v/vec2 m/SQRT3_2 1.0)
       (v/vec2 0.0 1.5)])

(def p2 [(v/vec2 0.0 0.0)
       (v/vec2 m/SQRT3_2 0.5)
       (v/vec2 m/SQRT3_2 1.5)
       (v/vec2 0.0 1.0)])

(def p3 [(v/vec2 0.0 0.5)
       (v/vec2 m/SQRT3_2 0.0)
       (v/vec2 m/SQRT3 0.5)
       (v/vec2 m/SQRT3_2 1.0)])

#_(def pal (c/palette :guell))

(def pal [:black :red :white])

(def p [[(pal 0) p1]
      [(pal 1) p2]
      [(pal 2) p3]])

(defn overlap?
  [p1 p2]
  (some true? (map (fn [c1 c2]
                     (and (c/not-black? c1)
                          (c/not-black? c2))) p1 p2)))

(defn maybe-draw
  [c ^long t]
  (let [size (max 1 (m/norm (m/sq (m/norm t 0.0 2400.0 1.0 0.0)) 0.0 1.0 2.0 100.0))
        x (r/irand 800)
        y (r/irand 800)
        [col p] (rand-nth p)
        s (c2d/path-shape (map #(v/mult % size) p))
        [_ _ w h] (c2d/bounding-box s)
        w (m/ceil w)
        h (m/ceil h)
        p1 (c2d/with-canvas-> (c2d/canvas w h :low)
             (c2d/set-background :black)
             (c2d/set-color :white)
             (c2d/shape s)
             (p/to-pixels))]
    (if (or
         (>= (+ x w) 800)
         (>= (+ y h) 800)
         (overlap? p1 (p/to-pixels c x y w h)))
      (recur c (inc t))
      (-> c
          (c2d/set-color col)
          (c2d/push-matrix)
          (c2d/translate x y)
          (c2d/shape s)
          (c2d/pop-matrix)))))

(c2d/with-canvas [c (c2d/canvas 800 800 :highest)]
  (c2d/set-background c :black)
  (dotimes [t 1200]
    (maybe-draw c (* 2 t)))
  (c2d/image c (o/noise-overlay 800 800 {:alpha 60}))
  #_(c2d/save c "results/2022/day09_d.jpg")
  (utils/show-image c))



(c2d/with-canvas [c (c2d/canvas 800 800)]
  (c2d/set-background c :black)
  (c2d/set-color c :white)
  (c2d/set-font-attributes c 200)
  (let [s (c2d/font-shape c "ABC" 400 400)]
    (c2d/shape c s true))
  (utils/show-image c))
