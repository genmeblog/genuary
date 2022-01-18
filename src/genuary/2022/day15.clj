;; Sand.
(ns genuary.2022.day15
  (:require [clojure2d.core :as c2d]
            [clojure2d.extra.utils :as utils]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [clojure2d.pixels :as p]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn distort
  [v ^double weight]
  (-> (v/vec2 (/ weight 2.0) (r/drand m/TWO_PI))
      (v/from-polar)
      (v/add v)
      (v/shift (r/drand (* -1.5 weight)))))

(def vb (v/vec2 -500 -500))
(def ^:const ^double maxdist (m/dist -500 -500 500 500))

(c2d/with-canvas [c (c2d/canvas 1800 1800)]
  (c2d/set-background c [240 216 168])
  (c2d/translate c 900 750)
  (c2d/rotate c (+ m/PI m/QUARTER_PI))
  (dotimes [_ 12000000]
    (let [x (r/drand -500 500)
          y (r/drand -500 500)
          v (v/vec2 x y)
          d (- 150.0 (* 150.0 (m/sqrt (/ (v/dist vb v) maxdist))))
          pos (distort v d)
          d2 (/ (v/dist pos v) 1.5)]
      (c2d/set-color c (+ 60.0 d2) (- 30.0 (/ d2 6.0)) 5 80)
      (c2d/ellipse c (pos 0) (pos 1) 0.8 0.8)))
  (let [res (c2d/resize (->> (p/to-pixels c)
                             (p/filter-channels p/gaussian-blur-1)) 800 800)]
    ;; (c2d/save res "results/2022/day15.jpg")
    (utils/show-image res)))
