(ns oskarkv.utils.math
  (:require
   [oskarkv.utils.base :as b]
   [oskarkv.utils.general :as g]
   [oskarkv.utils.impl :as impl]))

(defn floor
  "Returns the floor of `x` as a long."
  [x]
  (long (Math/floor x)))

(defn ceil
  "Returns the ceil of `x` as a long."
  [x]
  (long (Math/ceil x)))

(defn parse-int
  "Parses a long with Long/parseLong."
  ([x] (#?(:clj Long/parseLong :cljs js/parseInt) x))
  ([x radix] (#?(:clj Long/parseLong :cljs js/parseInt) x radix)))

(defn sum
  "Sums the numbers in `nums`."
  [nums]
  (reduce + nums))

(defn product
  "Returns the product of the numbers in `nums`."
  [nums]
  (reduce * nums))

(defn normalize-by-sum
  "Returns a sequence where each number in `nums` has been divided by
   the sum of the elements in `nums`."
  [nums]
  (mapv #(/ % (sum nums)) nums))

(defn sign
  "Returns 1 for positive numbers, -1 for negative numbers, and 0 for 0."
  [x]
  (b/condf x
    pos? 1
    neg? -1
    0))

(defn avg
  "Returns the average of `nums`."
  [nums]
  (/ (sum nums) (count nums)))

;; Could perhaps be more efficient in Java by using Random.nextGaussian
(defn- rand-gaussian*
  "Returns random Gaussian distributed number with mean 0 and standard
   deviation 1."
  []
  ;; These math functions exist in both Java and JS.
  ;; Could generate 2 values from u and u2, by also using (f Math/cos).
  (let [u (rand) u2 (rand)
        ;; (log 0) is undefined
        u (if (zero? u) (first (drop-while zero? (repeatedly rand))) u)]
    (* (Math/sqrt (* -2 (Math/log u)))
       (Math/sin (* 2 Math/PI u2)))))

(defn rand-gaussian
  "Returns a random number from a Gaussian distribution with the given
   standard deviation `sd` and `mean`, which default to 1 and 0
   respectively."
  ([] (rand-gaussian*))
  ([sd] (rand-gaussian sd 0))
  ([sd mean] (+ (* sd (rand-gaussian*) mean))))

(defn rand-binomial
  "Returns a random number from the binomial distribution with parameters
   `n` and `p`, i.e. the number of random tests among `n` that were
   successful, with each test having success probability `p`."
  [n p]
  (apply + (repeatedly n #(if (g/chance p) 1 0))))

(defn rand-uniform
  "Returns a random number between `min` and `max`, with `min` and `max`
   defaulting to 0 and 1 respectively."
  ([] (rand))
  ([max] (rand max))
  ([min max] (+ min (rand (- max min)))))

(defn rand-uniform-int
  "Returns a random integer between `min` and `max`, both inclusive.
   If called with one argument, `min` defaults to 0."
  ([max] (rand-int (inc max)))
  ([min max] (+ min (rand-int (inc (- max min))))))

(impl/make-v-and-str-fns []
                         normalize-by-sum)
