;; Airport carpet.
(ns genuary.2022.day26
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.core :as c2d]
            [clojure2d.extra.utils :as utils]
            [clojure2d.color :as c]
            [clojure2d.pixels :as p]
            [fastmath.vector :as v]
            [clojure2d.extra.overlays :as o]))

(def pal (c/resample (c/palette) 6))
(def extra1 (c/from-HSB [(+ 180 (c/hue (pal 0))) 255 255]))
(def extra2 (c/from-HSB [(+ 180 (c/hue (pal 5))) 255 255]))

(defn draw-rectangles
  [canvas]
  (c2d/push-matrix canvas)
  (c2d/translate canvas 100.0 100.0)
  (let [cnt (r/irand 2 20)
        step (/ m/TWO_PI cnt)
        size (r/irand 10 100)
        col (r/randval extra2 (r/randval 0.8 (pal 1) (pal 2)))]
    (dotimes [_ cnt]
      (c2d/rotate canvas step)
      (c2d/set-color canvas col (r/randval 255.0 200.0))
      (c2d/rect canvas 20 20 size size (r/brand 0.2))))
  (c2d/pop-matrix canvas))

(defn draw-lines
  [canvas]
  (c2d/push-matrix canvas)
  (c2d/translate canvas 100.0 100.0)
  (c2d/rotate canvas (r/drand m/TWO_PI))
  (let [len (r/irand 50 200)
        w (r/irand 5 20)
        col (r/randval extra1 (r/randval 0.8 (pal 3) (pal 4)))]
    (dotimes [_ (r/irand 3 20)]
      (c2d/set-color canvas col (r/randval 255.0 100.0))
      (c2d/rect canvas -100 (- len) w (* 2 len) (r/brand 0.4))
      (c2d/translate canvas (* 2 w) 0.0)))
  (c2d/pop-matrix canvas))

(def carpet (p/to-pixels (let [s (r/drand 10 200)]
                         (c2d/with-canvas [c (c2d/canvas 200 200)]
                           (c2d/set-background c (pal 0))
                           (draw-rectangles c)
                           (draw-lines c)
                           (c2d/set-color c (pal 5) (r/drand 20 120))
                           (c2d/ellipse c (r/drand 200) (r/drand 200) s s)))))

(defn mult-
  [q v]
  (if (even? q) v (inc (- v))))

(let [img (binding [p/*pixels-edge* :wrap]
            (c2d/with-canvas [c (c2d/canvas 1350 1350)]
              (c2d/set-background c (pal 0))
              (doseq [[x y] (repeatedly 1500000 (fn [] (v/generate-vec2 #(r/drand 1350))))
                      :let [xi (int (quot x 200))
                            yi (int (quot y 200))
                            xc (mult- xi (rem x 200))
                            yc (mult- yi (rem y 200))]]
                (c2d/set-color c (p/get-color carpet xc yc) (r/irand 80 255))
                (c2d/ellipse c x y (r/drand 0.5 5.0) (r/drand 0.5 5.0)))
              c))]
  (c2d/with-canvas [c (c2d/canvas 800 800)]
    (c2d/push-matrix c)
    (c2d/translate c 400 400)
    (c2d/rotate c (r/drand m/TWO_PI))
    (c2d/image c img -600 -600)
    (c2d/pop-matrix c)
    (c2d/image c (o/noise-overlay 800 800 {:alpha 120}))
    #_(c2d/save c "results/2022/day26_c.jpg")
    (utils/show-image c)))
