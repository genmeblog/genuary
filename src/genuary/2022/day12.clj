;; Packing (squares, circles, any shape…)
(ns genuary.2022.day12
  (:require [genuary.common :refer [divide-square]]
            [fastmath.random :as r]
            [clojure2d.core :as c2d]
            [clojure2d.color :as c]
            [clojure2d.extra.utils :as utils]
            [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [clojure2d.extra.overlays :as o]))

(def n (r/random-noise-fn))
(def points (m/slice-range -20 820 (r/irand 10 100)))
(def lines (->> (m/slice-range 0 4 50)
              (map (fn [y] (let [s (r/irand 3 200)]
                            (map #(* s (- (n (/ % 150.0) y) 0.1)) points))))))

(def stack (reductions (fn [buf l] (map + buf l)) (map #(- % 50) (first lines)) (rest lines)))

(def p (r/randval c2d/path c2d/path-bezier))

(def pattern (c2d/with-canvas [c (c2d/canvas 800 800)]
             (c2d/flip-y c)
             (c2d/translate c 0 -800)
             (c2d/set-background c [50 50 50])
             (doseq [ys (reverse stack)
                     :let [ps (conj (vec (conj (map vector points ys) [0 0])) [800 0])]]
               (c2d/set-color c (c/random-color) 180)
               (p c ps true false))
             #_(utils/show-image c)
             c))

(def no (o/noise-overlay 800 800 {:alpha 40}))

(c2d/with-canvas [c (c2d/canvas 800 800 :highest)]
  (c2d/set-background c [50 50 50])
  (doseq [[x1 y1 x2 y2] (divide-square 8 50 50 750 750 {:sq? true :quick-stop 0.2})
          :let [x (inc x1)
                y (inc y1)
                w (int (- x2 x 1))
                h (int (- y2 y 1))]]
    (if (and (pos? w) (pos? h) (> (+ w h) 4))
      (do
        (c2d/image c (c2d/subimage pattern x y w h) x y)
        (c2d/set-color c (c/random-color) 80)
        (c2d/rect c x y w h))
      (when (and (r/brand 0.3) (< (+ w h) 3))
        (c2d/set-color c (p/get-color pattern x y) 200)
        (c2d/point c x y))))
  (let [res (o/render-noise c no)]
    #_(c2d/save res "results/2022/day12d.jpg")
    (utils/show-image res)))
