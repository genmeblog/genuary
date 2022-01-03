;; Dithering.
(ns genuary.2022.day02
  (:require [clojure2d.pixels :as p]
            [clojure2d.core :as c2d]
            [clojure2d.extra.utils :as utils]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [fastmath.random :as r]
            [fastmath.core :as m])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def pimg (p/filter-channels p/normalize (p/load-pixels "resources/flickr.jpg")))
(def w (long (c2d/width pimg)))
(def h (long (c2d/height pimg)))

(def target-colors (c/reduce-colors pimg 32))

(def pimg-reduced (p/filter-colors #(c/nearest-color target-colors % v/dist) pimg))

(defn jump
  ^Vec2 [pos]
  (v/add pos (v/generate-vec2 #(r/grand 2.0))))

(defn iterator
  [col ^Vec2 pos]
  (let [npos (jump pos)
        cprev (v/dist-cos col (p/get-color pimg (.x pos) (.y pos)))
        cnext (v/dist-cos col (p/get-color pimg (.x npos) (.y npos)))]
    (if (or (<= cnext cprev) (< (r/drand) (/ cprev cnext)))
      npos pos)))

#_(let [buffer (p/renderer w h :sinc 2.0)]
    (dotimes [_ 40000]
      (let [col (rand-nth target-colors)]
        (doseq [[^double x ^double y] (take 1000 (iterate (partial iterator col) (v/vec2 (r/drand w) (r/drand h))))]
          (p/set-color! buffer (mod x ^long w) (mod y ^long h) col))))
    (->> {:brightness 1.2 :saturation 1.2 :contrast 1.2}
         (p/to-pixels buffer)
         (p/filter-channels p/normalize)
         (utils/show-image)))

(c2d/with-canvas [buffer (c2d/canvas w h)]
  (c2d/set-background buffer :black)
  (dotimes [_ 10000]
    (let [xx (r/drand w)
          yy (r/drand h)
          ;; col (rand-nth target-colors)
          col (p/get-color pimg-reduced xx yy)
          ]
      (c2d/set-color buffer col)
      (doseq [[^double x ^double y] (take 2000 (drop 20 (iterate (partial iterator col) (v/vec2 xx yy))))]
        (c2d/rect buffer (int (mod x ^long w)) (int (mod y ^long h)) 1 1))))
  (utils/show-image buffer)
  #_(c2d/save buffer "results/2022/day02c.jpg"))

