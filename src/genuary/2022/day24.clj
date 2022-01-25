;; Create your own pseudo-random number generator and visually check the results.
(ns genuary.2022.day24
  (:require [fastmath.random :as r]
            [clojure2d.core :as c2d]
            [clojure2d.extra.utils :as utils]
            [clojure2d.color :as c]
            [clojure2d.extra.overlays :as o]
            [fastmath.core :as m]))

(defn make-rand
  []
  (let [x (r/irand)
        y (r/irand)
        z (r/irand)]
    (fn
      ([] (r/irand))
      ([prev]
       #_(bit-and 0x7fffffff (unchecked-add z (bit-xor y (unchecked-remainder-int (bit-not prev) x))))
       (bit-and 0x7fffffff (unchecked-add z (unchecked-add y (bit-xor prev x))))))))

(def d (/ 800.0 (double 0x7fffffff)))
;; (def pal (c/paletton :tetrad (r/drand 360) {:angle (r/drand 20 30) :compl true}))
(def pal (c/random-palette))

(c2d/with-canvas [c (c2d/canvas 800 800)]
  (c2d/set-background c :black)
  (c2d/set-stroke c 0.5)
  (dotimes [_ 50]
    (c2d/set-color c (rand-nth pal) 130)
    (let [r (make-rand)]
      (doseq [[x y w h] (take 10000 (partition 4 (r/irand 1 5) (map #(* d %) (iterate r (r)))))]
        (-> c
            (c2d/push-matrix)
            (c2d/translate x y)
            (c2d/rotate m/QUARTER_PI)
            (c2d/rect 0 0 (inc (bit-and 1 (long w))) (inc (bit-and 1 (long h))))
            (c2d/pop-matrix)))))
  (c2d/image c (o/noise-overlay 800 800 {:alpha 80}))
  #_(c2d/save c "results/2022/day24_c.jpg")
  (utils/show-image c))
