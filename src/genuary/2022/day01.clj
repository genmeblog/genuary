;; Draw 10,000 of something.
(ns genuary.2022.day01
  (:require [clojure2d.core :as c2d]
            [clojure2d.extra.utils :as utils]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.color :as c]))

(defn iterator [x curr] (+ 4.0 curr (* 18.0 (r/noise x (/ curr 5.0)))))

(def p (c/palette 88))

(c2d/with-canvas [c (c2d/canvas 1400 900 :highest)]
  (c2d/set-background c [10 20 30])
  (c2d/set-stroke c 5.0)
  (c2d/set-color c (p 0) 220)
  (c2d/translate c 30 50)
  (doseq [y (m/slice-range 0.0 4.0 100)
          [xx yy] (->> (* y 7.0)
                       (r/noise)
                       (* 50.0)
                       (iterate (partial iterator y))
                       (partition 2 1)
                       (take 100))
          :let [ny (* y 200)]]
    (cond
      (< (r/drand) 0.01) (c2d/set-color c (p 1) 220)
      (< (r/drand) 0.02) (c2d/set-color c (p 2) 220)
      (< (r/drand) 0.03) (c2d/set-color c (p 3) 220)
      (< (r/drand) 0.02) (c2d/set-color c (p 4) 220)
      (< (r/drand) 0.3) (c2d/set-color c (p 0) 220))
    (c2d/line c xx
              (+ (* 20.0 (- (r/noise y (/ xx 20.0)) 0.5)) ny)
              (- yy 7)
              (+ (* 20.0 (- (r/vnoise y (/ xx 20.0)) 0.5)) ny)))
  ;; (c2d/save c "results/2022/day01.jpg")
  (utils/show-image c))
