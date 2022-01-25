;; Perspective.
(ns genuary.2022.day25
  (:require [fastmath.vector :as v]
            [clojure2d.core :as c2d]
            [clojure2d.extra.utils :as utils]
            [clojure2d.pixels :as p]
            [fastmath.random :as r]
            [clojure2d.color :as c]))

(defn project
  [[x y z]]
  (v/mult (v/vec2 (/ x z) (/ y z)) 3.0))

(def n (r/random-noise-fn))

(defn in-noise?
  [[^double x ^double y ^double z]]
  (> (n (/ x 30.0) (/ y 30.0) z) 0.5))

(def g (c/gradient [:white (c/set-alpha :cyan 128) (c/set-alpha :red 0)]))

(defn grid []
  (r/grand 50))

;; blobs
#_(let [buffer (p/renderer 800 800)]
    (doseq [v3 (take 1000000 (filter in-noise? (repeatedly (fn [] (v/generate-vec3 grid grid #(r/drand 1 7))))))
            :let [z (/ (dec (v3 2)) 6.0)
                  [x y] (project v3)]]
      (p/set-color! buffer (+ x 400) (+ y 400) (g z)))
    (let [img (c2d/get-image (p/to-pixels buffer {:saturation 1.4}))]
      #_(c2d/save img "results/2022/day25_b.jpg")
      (utils/show-image img)))

(defn generate-plane
  []
  (let [x (grid)
        z (r/drand 1 7)
        m (r/randval -1.0 1.0)
        y (+ (* m 100.0) (* m 50.0 (n (/ x 30.0) z m)))]
    (r/randval 0.2 (v/vec3 x y z) (v/vec3 y x z))))

(let [buffer (p/renderer 800 800)]
  (doseq [v3 (take 1000000 (repeatedly generate-plane))
          :let [z (/ (dec (v3 2)) 6.0)
                [x y] (project v3)]]
    (p/set-color! buffer (+ x 400) (+ y 400) (g z)))
  (let [img (c2d/get-image (p/to-pixels buffer {:saturation 1.4}))]
    #_(c2d/save img "results/2022/day25_d.jpg")
    (utils/show-image img)))
