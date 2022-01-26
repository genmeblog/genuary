;; Airport carpet.
(ns genuary.2022.day26
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.core :as c2d]
            [clojure2d.extra.utils :as utils]
            [clojure2d.color :as c]
            [clojure2d.pixels :as p]
            [fastmath.vector :as v]
            [clojure2d.extra.overlays :as o]
            [clojure.string :as str]))

(def pal (c/resample (c/palette) 6))
(def extra1 (c/from-HSB [(+ 180 (c/hue (pal 0))) 255 255]))
(def extra2 (c/from-HSB [(+ 180 (c/hue (pal 5))) 255 255]))

(defn draw-rectangles
  [canvas]
  (c2d/push-matrix canvas)
  (c2d/translate canvas (r/drand 50 150) (r/drand 50 150))
  (let [cnt (r/irand 2 20)
        step (/ m/TWO_PI cnt)
        size1 (r/irand 10 100)
        size2 (r/randval size1 (r/irand 10 100))
        col (r/randval extra2 (r/randval 0.8 (pal 1) (pal 2)))
        shift (r/drand 10 30)
        ang (r/drand m/TWO_PI)]
    (dotimes [_ cnt]
      (-> canvas
          (c2d/rotate step)
          (c2d/set-color col (r/randval 255.0 200.0))
          (c2d/push-matrix)
          (c2d/translate shift shift)
          (c2d/rotate ang)
          (c2d/rect 0 0 size1 size2 (r/brand 0.2))
          (c2d/pop-matrix))))
  (c2d/pop-matrix canvas))

(defn draw-lines
  [canvas]
  (c2d/push-matrix canvas)
  (c2d/translate canvas 100.0 100.0)
  (c2d/rotate canvas (r/drand m/TWO_PI))
  (let [len (r/irand 20 200)
        w (r/irand 1 20)
        col (r/randval extra1 (r/randval 0.8 (pal 3) (pal 4)))
        tmult (r/randval 0.0 (r/drand -6.0 6.0))]
    (dotimes [t (r/irand 1 30)]
      (c2d/set-color canvas col (r/randval 255.0 100.0))
      (c2d/rect canvas -100 (- len) w (m/abs (- (* 2.0 len) (* t tmult))) (r/brand 0.4))
      (c2d/translate canvas (* 2 w) 0.0)))
  (c2d/pop-matrix canvas))

(defn draw-circles
  [canvas]
  (let [s1 (r/drand 2 200)
        s2 (r/randval s1 (r/drand 2 200))]
    (c2d/set-color canvas (pal 5) (r/drand 20 120))
    (c2d/ellipse canvas (r/drand 200) (r/drand 200) s1 s2)
    (if (r/brand)
      (recur canvas)
      canvas)))

(defn draw-letter
  [canvas]
  (if (r/brand)
    (do
      (-> canvas
          (c2d/push-matrix)
          (c2d/set-color (c/gray (r/irand 50 220)) (r/drand 150 255))
          (c2d/translate (r/drand 200) (r/drand 200))
          (c2d/rotate (r/drand m/TWO_PI))
          (c2d/set-font (rand-nth (remove #(str/starts-with? % "N") c2d/fonts-list)))
          (c2d/set-font-attributes (r/drand 8 100))
          (c2d/text (str (rand-nth "1234567890qwertyuiopasdfghjklzxcvbnm-=[];',./!@#$%^&*()_+QWERTYUIOP{}ASDFGHJKL:ZXCVBNM<>?")) 0 0 :center)
          (c2d/pop-matrix))
      (recur canvas))
    canvas))

(def carpet (p/to-pixels (c2d/with-canvas-> (c2d/canvas 200 200)
                           (c2d/set-background (pal 0))
                           (draw-rectangles)
                           (draw-lines)
                           (draw-circles)
                           (draw-letter)
                           (c2d/translate (r/drand 200) (r/drand 200))
                           (c2d/rotate (r/drand m/TWO_PI))
                           (c2d/set-color (c/random-color))
                           (c2d/line -200 0 200 0))))

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
    (c2d/image c (o/noise-overlay 800 800 {:alpha 100}))
    #_(c2d/save c "results/2022/day26_e.jpg")
    (utils/show-image c)))
