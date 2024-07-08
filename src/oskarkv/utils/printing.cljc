(ns oskarkv.utils.printing
  (:require
   [fipp.edn :as pp]
   [oskarkv.utils.base :as b]))

(b/defalias pprint pp/pprint)

(defn printit
  "Prints `x` with `println` and returns `x`."
  [x]
  (println x) x)

(defn tapit
  "Calls `tap>` on `x` and returns `x`."
  [x]
  (tap> x) x)

(defn pprintit
  "Pprints `x` and returns `x`."
  [x]
  (pp/pprint x) x)

(defn pprintlnit
  "Pprints `x` followed by a blank line and returns `x`."
  [x]
  (pp/pprint x) (println) x)
