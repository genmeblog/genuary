;; Destroy a square.
(ns genuary.2022.day05
  (:require [fastmath.random :as r]
            [clojure2d.core :as c2d]
            [clojure2d.color :as c]
            [clojure2d.extra.utils :as utils]
            [fastmath.core :as m]
            [fastmath.fields :as f]
            [fastmath.vector :as v]
            [clojure2d.extra.overlays :as o]
            [genuary.common :refer [divide-square]]))

(def in (m/make-norm 200 800 -2.0 2.0))
(def out (m/make-norm -1.0 1.0 100 900))

(def s (f/field :sinusoidal))
(def f (f/combine))
(def g (c/gradient))

(def no (o/noise-overlay 1000 1000 {:alpha 100}))

(c2d/with-canvas [c (c2d/canvas 1000 1000 :highest)]
  (c2d/set-background c (c/gray 20))
  (doseq [sq (divide-square 7 200 200 800 800)
          :let [[x1 y1 x2 y2] (map in sq)
                colpos (r/randval 0.1 (r/drand) (m/norm (+ (m/abs x1) (m/abs y1)) 0.0 4.0 0.0 1.0))
                p (->> [(v/vec2 x1 y1) (v/vec2 x2 y1) (v/vec2 x2 y2) (v/vec2 x1 y2)]
                       (mapcat (comp s f))
                       (map out)
                       (partition 2)
                       (map vec))]]
    (c2d/set-color c (g colpos) 200)
    (c2d/path c p true (r/randval 0.1)))
  (let [img (o/render-noise c no)]
    ;; (c2d/save img "results/2022/day05.jpg")
    (utils/show-image img)))
