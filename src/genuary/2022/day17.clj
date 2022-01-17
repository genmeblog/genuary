;; 3 colors.
(ns genuary.2022.day17
  (:require [clojure2d.core :as c2d]
            [clojure2d.extra.utils :as utils]
            [fastmath.core :as m]
            [clojure2d.color :as c]
            [fastmath.random :as r]
            [clojure2d.extra.overlays :as o]))

(let [pal (->> {:angle (r/drand 20 70)
                :compl true
                :preset (rand-nth [:full :shiny])}
               (c/paletton :triad (r/drand 360))
               (drop 5)
               (take-nth 5))
      letters "typography"]
  (c2d/with-canvas [c (c2d/canvas 800 400)]
    (c2d/set-background c (last pal))
    (c2d/set-font c "Heavitas")
    (doseq [[col s] (map vector (cycle pal) (range 20000 90 -7))]
      (c2d/set-font-attributes c s :bold-italic)
      (let [[x1 y1 x2 y2] (c2d/text-bounding-box c letters)]
        (-> c
            (c2d/push-matrix)
            (c2d/translate (- 400 (m/lerp x1 x2 0.5)) (+ (/ s 2) (- 200 (m/lerp y1 y2 0.5))))
            (c2d/set-color col)
            (c2d/text letters 0 0)
            (c2d/pop-matrix))))
    (let [res (-> c
                  (c2d/image (o/noise-overlay 800 400 {:alpha 70}))
                  (o/render-rgb-scanlines))]
      #_(c2d/save res (str "results/2022/day17_" letters ".jpg"))
      (utils/show-image res))))
