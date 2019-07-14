(ns oskarkv.utils.impl
  (:require
   [clojure.walk :as walk]
   [clojure.string :as str]))

(defn unqualify-syms*
  "Return code but where any occurrence of the symbols in syms has been
   unqualified."
  [syms code]
  (walk/postwalk
   #(if (and (symbol? %)
             (some #{(name %)} (set (map name syms))))
      (symbol (name %))
      %)
   code))

(defmacro reversed-reductions [base-fn]
  (unqualify-syms*
   '[f init coll]
   `(fn
      ([f coll] (reverse (~base-fn f (reverse coll))))
      ([f init coll] (reverse (~base-fn f init (reverse coll)))))))

(defn private-symbol [sym]
  (with-meta sym (assoc (meta sym) :private true)))

(defmacro defprivatedef [name deffer]
  `(defmacro ~name [inner-name# & rest#]
     (list* ~deffer (private-symbol inner-name#) rest#)))
