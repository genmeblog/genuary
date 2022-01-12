(ns genuary.common
  (:require [fastmath.random :as r]))

(defn divide-square-internal
  [depth x1 y1 x2 y2 {:keys [sq? quick-stop]
                      :or {sq? false quick-stop 0.1}
                      :as conf}]
  (if (or (and quick-stop (r/randval quick-stop))
          (zero? depth)
          (< (- x2 x1) 4)
          (< (- y2 y1) 4))
    [x1 y1 x2 y2]
    (let [mid-x (if sq? (+ x1 (/ (- x2 x1) 2)) (r/irand (+ x1 2) (dec x2)))
          mid-y (if sq? (+ y1 (/ (- y2 y1) 2)) (r/irand (+ y1 2) (dec y2)))]
      [(divide-square-internal (dec depth) x1 y1 mid-x mid-y conf)
       (divide-square-internal (dec depth) mid-x mid-y x2 y2 conf)
       (divide-square-internal (dec depth) x1 mid-y mid-x y2 conf)
       (divide-square-internal (dec depth) mid-x y1 x2 mid-y conf)])))

(defn divide-square
  ([depth x1 y1 x2 y2] (divide-square depth x1 y1 x2 y2 {}))
  ([depth x1 y1 x2 y2 conf]
   (partition 4 (flatten (divide-square-internal depth x1 y1 x2 y2 conf)))))
