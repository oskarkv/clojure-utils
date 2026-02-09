(ns oskarkv.utils.printing
  (:require
   [fipp.edn :as pp]
   [oskarkv.utils.base :as b #?@(:cljs [:include-macros true])]))

(b/defalias pprint pp/pprint)

(defn pprint-str
  "Pprint to string."
  [x]
  (with-out-str
    (pprint x)))

(defn printit
  "Prints `x` with `println` and returns `x`."
  [x]
  (println x) x)

(defn printlnit
  "Prints `x` with `println` plus a blank line and returns `x`."
  [x]
  (println x) (println) x)

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

(defn print-ex [e]
  (binding [*out* *err*]
    (println (ex-message e))
    (pp/pprint (ex-data e))
    (.printStackTrace e)))
