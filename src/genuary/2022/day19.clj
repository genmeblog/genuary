;; Use text/typography.
(ns genuary.2022.day19
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [clojure2d.extra.utils :as utils]
            [fastmath.vector :as v]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.random :as r]
            [clojure2d.extra.overlays :as o]))

(defn make-letter
  [txt fontname]
  (c2d/with-canvas [c (c2d/canvas 800 800)]
    (c2d/set-font c fontname)
    (c2d/set-font-attributes c 600 :bold)
    (let [[x1 y1 x2 y2] (c2d/text-bounding-box c txt)]
      (-> c
          (c2d/set-background :black)
          (c2d/set-color :white)
          (c2d/translate (- 400 (m/lerp x1 x2 0.5)) (+ 300 (- 400 (m/lerp y1 y2 0.5))))
          (c2d/text txt 0 0)))))

(def mid (v/vec2 400 400))

(defn iterator
  [img [x y :as pos] step]
  (let [l (p/get-value img 0 x y)]
    (if (< l 128)
      (recur img (v/add pos step) step)
      pos)))

(defn scan-letter-out
  [img quality init-angle]
  (map #(v/mult (v/sub % mid) 0.06)
       (for [angle (butlast (m/slice-range 0.0 m/TWO_PI quality))
             :let [v (v/vec2 (m/cos (+ init-angle angle)) (m/sin (+ init-angle angle)))
                   start (v/add (v/mult v 400) mid)]]
         (iterator img start (v/sub v)))))

(let [r (m/slice-range 0.0 1.0 10)
      r (map (comp (m/make-norm 0.0 1.0 5 30) m/sq) (concat (butlast r) [4.0] (reverse r)))]
  (c2d/with-canvas [c (c2d/canvas 800 800)]
    (c2d/set-background c :black)
    (c2d/set-color c :white)
    (c2d/translate c 35 50)
    (doseq [[line letter col] (map vector (range) "this is CRAPPY alphabet" (cycle (c/palette 0)))]
      (when-not (= letter \space)
        (c2d/push-matrix c)
        (c2d/translate c 0 (* line 32))
        (let [img (p/to-pixels (make-letter (str letter) "Dialog"))]
          (doseq [s r]
            (-> c
                (c2d/translate 35 0)
                (c2d/set-color col)
                (c2d/path (scan-letter-out img s (r/drand m/TWO_PI)) true (r/randval 0.1)))))
        (c2d/pop-matrix c)))
    (c2d/reset-matrix c)
    (c2d/image c (o/noise-overlay 800 800))
    ;; (c2d/save c "results/2022/day19.jpg")
    (utils/show-image c)))

(def grid (for [y (range 23)
              x (range 20)]
          [(+ 70 (* x 35))
           (+ 50 (* y 32))]))

(def s "Here is the answer for GENUARY 2022 prompt by Piter Pasma (Use text/typography) / First let's sample letters' and create closed path / Second let's draw letters on the grid with random sampling accuracy filled or outline only / Now repeat! / ")

(c2d/with-canvas [c (c2d/canvas 800 800)]
  (c2d/set-background c :black)
  (c2d/set-color c :white)
  (doseq [[[x y] letter col] (map vector grid (cycle s) (cycle (c/palette 1)))]
    (when-not (= letter \space)
      (c2d/push-matrix c)
      (c2d/translate c x y)
      (let [img (p/to-pixels (make-letter (str letter) "Times New Roman"))]
        (c2d/set-color c col)
        (c2d/path c (scan-letter-out img (r/randval 0.15 (rand-nth [5 6 7 9 10 11]) (rand-nth [20 30 40 50 60 70 80 90]))
                                     (r/drand m/TWO_PI)) true (r/randval 0.2)))
      (c2d/pop-matrix c)))
  (c2d/image c (o/noise-overlay 800 800))
  ;; (c2d/save c "results/2022/day19_b.jpg")
  (utils/show-image c))
