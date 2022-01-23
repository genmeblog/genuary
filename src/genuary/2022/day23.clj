(ns genuary.2022.day23
  (:require [fastmath.vector :as v]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [clojure2d.core :as c2d]
            [clojure2d.extra.utils :as utils]
            [clojure2d.color :as c]
            [clojure2d.extra.overlays :as o]))

(defn sprout []
  (let [v (r/drand 0.01 0.1)]
    {:vel (v/generate-vec3 #(r/drand (- v) v))
     :n (r/drand 0.35 0.6)
     :noise (r/random-noise-fn)
     :g (r/drand)
     :shift-y (r/drand 0.3 0.6)}))

(def gradient (c/gradient [[29 36 27] [170 254 1] [100 170 1]]))

#_(utils/show-gradient gradient)

(defn init []
  {:tr (v/generate-vec3 r/drand)
   :y 0
   :x (r/drand 50 750)
   :ang 0})

(defn iterator
  [{:keys [vel n noise shift-y]} {:keys [tr x y ang]}]
  (let [new-ang (+ ang (* 0.9 (- (noise (tr 0) (tr 1) (tr 2)) n)))
        new-tr (v/add tr vel)
        new-x (mod (+ x (m/cos new-ang)) 800)
        new-y (+ y (m/sin new-ang) shift-y)]
    {:tr new-tr
     :x new-x
     :y new-y
     :ang new-ang}))

(c2d/with-canvas [c (c2d/canvas 800 800)]
  (c2d/set-background c 29 36 27)
  (dotimes [t 80]
    (let [s (sprout)]
      (c2d/set-color c (gradient (:g s)) (* 3 t))
      (doseq [{:keys [x y ang]} (take-while #(< (:y %) 800) (iterate (partial iterator s) (init)))
              :let [y (- 800.0 y)]]
        (-> c
            (c2d/push-matrix)
            (c2d/translate x y)
            (c2d/rotate ang)
            (c2d/ellipse 0 0 (* 3.0 (/ y 800.0)) 2)
            (c2d/pop-matrix)))))
  (c2d/image c (o/noise-overlay 800 800 {:alpha 10}))
  #_(c2d/save c "results/2022/day23_b.jpg")
  (utils/show-image c))

