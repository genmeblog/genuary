;; Space.
(ns genuary.2022.day03
  (:require [clojure2d.core :as c2d]
            [fastmath.random :as r]
            [clojure2d.pixels :as p]
            [clojure2d.extra.utils :as utils]
            [fastmath.core :as m]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [fastmath.fields :as f]))

(def field1 (f/field :disc2 0.07 {:rot 1.0 :twist -6.0}))
(def field2 (f/field :cross 0.04))

(def field-conf (f/random-configuration))
(println field-conf)

;; b
#_(def field-conf {:type :operation, :name :add, :var1 {:type :operation, :name :mult, :var1 {:type :variation, :name :julia, :amount -0.8163708336638389, :config {}}, :var2 {:type :variation, :name :cpow, :amount 1.5998816679930976, :config {:r 0.297055874770511, :i -1.1418833569805504, :power 5.641100848773301}}, :amount 0.3512692618557276}, :var2 {:type :variation, :name :taurus, :amount 0.205099119129176, :config {:r 3.5941080319241188, :n -1.0828374078481695, :inv -0.38311692138729025}}, :amount 1.7973702930956708})
;; c
#_(def field-conf {:type :operation, :name :comp, :var1 {:type :operation, :name :deriv, :var {:type :variation, :name :foci, :amount 1.0, :config {}}, :amount 1.0, :step 0.5736634009018813}, :var2 {:type :variation, :name :horseshoe, :amount 1.0, :config {}}, :amount 1.0})
;; d
#_(def field-conf {:type :variation, :name :juliascope, :amount 1.0, :config {:power 3.636593139685998, :dist -2.3823688291295726}})

(def field (f/combine field-conf))

(defn nebula
  [buffer cnt gradient mn]
  (doseq [[x y] (r/->seq mn cnt)
          :let [vin (v/vec2 x y)
                ;; f1 (field1 vin)
                ;; f2 (field2 vin)
                f (v/mult (field vin) 0.5)
                ;; [^double x ^double y] (v/add vin (v/add f1 f2))
                [^double x ^double y] (v/add vin f)
                sx (* 0.7 (r/vnoise x y))
                sy (* 0.7 (r/noise y x))
                t (m/sqrt (* 2.0 sx sy))
                xx (+ 400.0 (* 150.0 (+ x sx -0.35)))
                yy (+ 400.0 (* 150.0 (+ y sy -0.35)))]]
    (p/set-color! buffer xx yy (gradient t)))
  buffer)

(defn get-star []
  (let [x (r/grand 3.0)
        y (r/grand 3.0)]
    [(+ 400 (* 150 (+ x -0.5 (r/noise x y))))
     (+ 400 (* 150 (+ y -0.5 (r/vnoise y x))))
     (r/drand 0.1 0.3)]))

(defn stars
  [buffer]
  (let [g (c/gradient [:pink :lightblue :yellow :gray])]
    (doseq [[^double sx ^double sy ^double s] (repeatedly 30000 get-star)
            :let [col (g (r/drand))
                  sc (r/drand 10.0)]
            [^double x ^double y] (repeatedly (r/randval 0.001 (r/irand 500 1000) (r/irand 1 50))
                                              (fn [] (v/generate-vec2 #(r/grand s))))]
      (p/set-color! buffer (+ x sx (* sc (r/noise x y))) (+ y sy (* sc (r/noise y x -0.2))) col)))
  buffer)

(-> (p/renderer 800 800 :gaussian 2.5)
    (stars)
    (nebula 10000000 (c/gradient
                      ;; :jjg_ccolo_sugar/Need_I_Say_More
                      ;; :hult/gr59_hult
                      :pals/parula)
            (r/distribution :multi-normal {:means [0 0]
                                           :covariances [[0.5 0.4]
                                                         [0.4 0.5]]}))
    (p/to-pixels {:saturation 1.3 :brightness 1.1 :contrast 1.3})
    #_(c2d/save "results/2022/day03d.jpg")
    (utils/show-image))

