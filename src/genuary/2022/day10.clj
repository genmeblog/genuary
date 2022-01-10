;; Machine learning, wrong answers only.
(ns genuary.2022.day10
  (:require [clojure2d.core :as c2d]
            [clojure2d.color :as c]
            [clojure2d.extra.utils :as utils]
            [clojure2d.pixels :as p]
            [fastmath.clustering :as cl]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [fastmath.distance :as d])
  (:import [smile.regression RBFNetwork]
           [smile.base.rbf RBF]))

(def config {:col-no (r/irand 2 20) ;; number of target colors (knots)
           :vlen (r/irand 50 500) ;; number of samples
           ;; distance for RBF clustering
           :dist (rand-nth [d/manhattan d/euclidean d/euclidean-sq d/earth-movers d/chebyshev d/canberra d/cosine])
           ;; number of RBF kernels
           :rbf-no (r/irand 2 20)
           ;; size of source window
           :r (r/irand 1 6)
           :f (rand-nth [+ * - (constantly 1)])})

(println config)

;;https://www.flickr.com/photos/thenationalmuseumofdenmark/10797597574/in/photolist-hs9vzq-6t7gqc-5HyBq2-G3untQ-5JL8La-55As3t-55AMhZ-5JL8rB-9h88Si-55EY5A-2mvgpMA-5JQpC3-5JQpkj-55EWBA-5JQp5m-55F1ZS-59qMf7-9LV8Qq-55ANx8-hs7YmA-65C4Ah-55ANnr-5HCUjN-apxabt-hs7rQ7-hs7ZZA-m9Smbh-8i8vRy-hs8XEW-2mQimX1-65C5k5-55ALM2-5FWg3S-2mJe5ko-55EYMW-hs9w27-2mJjqD5-doZ1YP-9P7Q3W-hs7kqx-pzACC9-8XPS49-2mk7HPA-hs7rBG-8GV8wp-55AMSx-55ANjF-55ALi6-hs8xKP-9udzbe
(def img (p/load-pixels "resources/flickr4.jpg"))

(def clusters (cl/x-means img (:col-no config)))

(def g (c/gradient (:representatives clusters)))

(def surrounding-idxs
  (let [r (:r config)]
    (vec (for [x (range (- r) (inc r))
               y (range (- r) (inc r))
               :when (not (zero? ((:f config) x y)))]
           [x y]))))

(defn luma-vector
  [img ^long x ^long y]
  (map (fn [[^long xx ^long yy]] (c/luma (p/get-color img (+ x xx) (+ y yy)))) surrounding-idxs))

(defn pixel-vectors
  [img]
  (let [div (double (dec (:col-no config)))
        vs (take (:vlen config) (map (fn [[^double x ^double y]]
                                       (let [x (int (* x (c2d/width img)))
                                             y (int (* y (c2d/height img)))
                                             p (p/get-color img x y)
                                             l (/ (clusters p) div)]
                                         [(luma-vector img x y) l])) (r/jittered-sequence-generator :r2 2 0.5)))]
    {:x (m/seq->double-double-array (map first vs))
     :y (m/seq->double-array (map second vs))}))

(defn make-regressor
  [{:keys [x y]}]
  (RBFNetwork/fit x y (RBF/fit x (:dist config) (:rbf-no config))))

(let [^RBFNetwork r (make-regressor (pixel-vectors img))
      target (p/clone-pixels img)]
  (dotimes [y (c2d/height target)]
    (dotimes [x (c2d/width target)]
      (let [col (g (.predict r (m/seq->double-array (luma-vector target x y))))]
        (p/set-color! target x y col))))
  (let [res (p/filter-channels p/normalize target)]
    #_    (c2d/save res "results/2022/day10d.jpg")
    (utils/show-image res)))

