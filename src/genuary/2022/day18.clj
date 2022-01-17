(ns genuary.2022.day18
  (:require [clojure2d.core :as c2d]
            [clojure2d.extra.utils :as utils]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.signal :as sig]
            [clojure2d.extra.signal :as sig2d]
            [clojure2d.extra.overlays :as o]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(def w 672)
(def h 504)

(def low-pass-effect (sig/effect :simple-lowpass {:rate 10000 :cutoff 2000}))
(def low-pass-effect2 (sig/effect :simple-lowpass {:rate 10000 :cutoff 1000}))
(def high-pass-effect (sig/effect :simple-highpass {:rate 100000 :cutoff 3}))
(def low-pass (sig2d/effects-filter low-pass-effect w))
(def low-pass3 (sig2d/effects-filter (sig/compose-effects low-pass-effect2 low-pass-effect2 low-pass-effect2) w))
(def high-pass (sig2d/effects-filter (sig/compose-effects high-pass-effect high-pass-effect) w))
(def delay-i (sig2d/effects-filter (sig/effect :echo {:rate w :delay 0.03 :decay 0.6}) w))
(def delay-q (sig2d/effects-filter (sig/effect :echo {:rate w :delay 0.04 :decay 0.6}) w))

(def n (o/noise-overlay w h {:alpha 60}))

(defn make-stripes
  [canvas]
  (c2d/push-matrix canvas)
  (c2d/translate canvas (/ w 2) (+ 20 (/ h 2)))
  (doseq [angle (range -0.555 (+ 0.5432 m/PI) 0.09)]
    (c2d/push-matrix canvas)
    (c2d/rotate canvas angle)
    (c2d/line canvas 0 0 w 0)
    (c2d/pop-matrix canvas))
  (c2d/pop-matrix canvas))

(def g (c/gradient [:cyan :lightblue :white :indigo :black]))

(defn add-errors
  [canvas]
  (doseq [x (range w)
          y (range h)
          :let [n (r/noise (/ x 800.0) (/ y 15.0))]]
    (when (< (r/drand) (m/pow n 8.0))
      (c2d/set-color canvas (g (r/drand)) 200)
      (c2d/ellipse canvas x y (r/drand 0.5 3) (r/drand 0.5 3))))
  canvas)

(defn unsharp-luma
  [pixels]
  (let [p (p/clone-pixels pixels)]
    (p/set-channel! p 1 (p/get-channel p 0))
    (p/set-channel! p 2 (p/get-channel p 0))
    (let [res (p/to-pixels (c2d/convolve p :unsharp))]
      (p/set-channel! pixels 0 (p/get-channel res 0)))))

(c2d/with-canvas-> (c2d/canvas w h :highest)
  (c2d/set-background :black)
  (c2d/set-color :darkred)
  (c2d/set-stroke 2.3)
  (make-stripes)
  (c2d/set-color :chocolate)
  (c2d/push-matrix)
  (c2d/translate 0 350)
  (c2d/flip-y)
  (make-stripes)
  (c2d/pop-matrix)
  (c2d/ellipse (/ w 2) 70 50 50)
  (c2d/gradient-mode (/ w 2) 300 (c/darken :indigo) (/ w 2) 480 :white)
  (c2d/triangle (* 0.2 w) 330 (* 0.8 w) 330 (* 0.5 w) 450)
  (c2d/gradient-mode (/ w 2) 300 :white (/ w 2) 480 :indigo)
  (c2d/triangle (* 0.2 w) 480 (* 0.8 w) 480 (* 0.5 w) 300)
  (c2d/set-font "Heavitas")
  (c2d/set-font-attributes 100 :bold-italic)
  (c2d/set-color :magenta)
  (c2d/text "GENUARY" 35 205)
  (c2d/set-color [240 220 240])
  (c2d/gradient-mode (/ w 2) 150 [255 235 240] (/ w 2) 200 :darkcyan)
  (c2d/text "GENUARY" 25 195)
  (c2d/set-font-attributes 50)
  (c2d/set-color :magenta)
  (c2d/text "2022" 505 255)
  (c2d/set-color [240 220 240])
  (c2d/text "2022" 495 245)
  (c2d/image n 0 0 w h)
  (add-errors)
  (p/to-pixels)
  (->> (p/filter-colors c/to-YIQ*)
       (p/filter-channels high-pass low-pass3 low-pass3 nil)
       (unsharp-luma)
       (p/filter-channels nil delay-i delay-q nil)
       (p/filter-channels low-pass low-pass3 low-pass3 nil)
       (p/filter-colors c/from-YIQ*)
       (p/filter-channels p/normalize))
  (c2d/get-image)
  (o/render-rgb-scanlines {:scale 1.1})
  #_(c2d/save "results/2022/day18.jpg")
  (utils/show-image))
