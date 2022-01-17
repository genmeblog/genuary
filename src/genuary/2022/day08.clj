;; Single curve only.
(ns genuary.2022.day08
  (:require [fastmath.core :as m]
            [clojure2d.core :as c2d]
            [clojure2d.extra.utils :as utils]
            [fastmath.random :as r]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [fastmath.curves :as cv])
  (:import [fastmath.vector Vec2]))

(m/use-primitive-operators)

#_(defn pdj
    [^double a ^double b ^double c ^double d]
    (fn [^double t]
      (Vec2. (- (m/cos (* a t)) (m/cos (* b t)))
             (- (m/sin (* c t)) (m/sin (* d t))))))

#_(let [typ :hypotrochoid
        params (cv/parametrization typ)
        curve (cv/curve typ params)]
    (println params)
    (c2d/with-canvas [c (c2d/canvas 800 800 :highest)]
      (c2d/set-background c :black)
      (c2d/set-color c :white 50)
      (c2d/translate c 400 400)
      (doseq [t (range 0.0 (* 15.0 m/TWO_PI) (/ m/TWO_PI 100000.0))
              :let [[^double x ^double y] (curve t)]]
        (c2d/rect c (* 100 x) (* 100 y) 1 1))
      (utils/show-image c)))

#_(cv/parametrization :astroid-pedal-curve)
