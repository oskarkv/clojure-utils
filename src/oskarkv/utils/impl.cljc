(ns oskarkv.utils.impl
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [com.rpl.specter :as s]))

;; WARNING: Definitions of the macros in this `do` block exist in both
;; src/utils.cljc and src/utils/impl.cljc , because they are both used
;; in functions in src/utils.cljc, and should be themselves defined in
;; src/utils.cljc for easy access to users of the utils namespace.
(do
  (defmacro ignore-exception
    "Evaluates `forms` and if an exception is thrown, instead return `nil`."
    {:style/indent 0}
    [& forms]
    `(try ~@forms (catch ~(if (:ns &env) :default 'Exception) ~'e nil)))
  (defmacro defalias
    "Create a new var with the value of evaluating `target-symbol` in the
     current namespace, and copies the metadata of `target-symbol`'s var."
    [alias-symbol target-symbol]
    `(let [var# (var ~target-symbol)
           target-thing# @var#
           meta# (meta var#)]
       (def ~alias-symbol target-thing#)
       (reset-meta! (var ~alias-symbol) meta#)))
  (defmacro condf
    "Takes an object `obj` and zero or more test-fn/expr `pairs`.
     Evaluates (test-fn `obj`) for each pair in order, and returns the
     expr of the pair if the test-fn returns logical true. A single
     default expression can follow the pairs, and its value will be
     returned if no clause matches. If no pair matches and there is no
     default expression, returns `nil`."
    {:style/indent 1}
    [obj & pairs]
    (when pairs
      (if (= (count pairs) 1)
        (first pairs)
        `(if (~(first pairs) ~obj)
           ~(second pairs)
           (condf ~obj ~@(next (next pairs)))))))
  (defmacro recursive-search-path [args must-arg result-path]
    `(s/recursive-path ~args p#
       (s/cond-path [#((every-pred ifn? coll?) %) (s/must ~must-arg)]
                  (s/multi-path ~result-path [s/ALL p#])
                  coll? [s/ALL p#]
                  s/STOP))))

(defmacro reversed-reductions
  "Returns a function that is like a `reductions`-like `base-fn`, but
   reduces from right to left."
  [base-fn]
  (unqualify-syms
   '[f init coll]
   `(fn
      ([f coll] (reverse (~base-fn f (reverse coll))))
      ([f init coll] (reverse (~base-fn f init (reverse coll)))))))

(defn private-symbol
  "Adds :private true to the metadata of `sym`."
  [sym]
  (with-meta sym (assoc (meta sym) :private true)))

(defmacro defprivatedef
  "Defines a private version of `deffer` with the name `name`."
  [name deffer]
  `(defmacro ~name [inner-name# & rest#]
     (list* ~deffer (private-symbol inner-name#) rest#)))

;; (resolve sym) does not work in ClojureScript
#?(:clj
   (defmacro make-v-and-str-fns
     "Defines v and -str versions of already existing functions in `syms`,
      like `mapv` for `map`, that return vectors and strings instead of
      seqs. Symbols ending in v or -str can be given in `exceptions` (a
      collection) to skip defining that function."
     [exceptions & syms]
     (letfn [(make-fns [sym]
               (let [ex (set exceptions)
                     m (meta (resolve sym))
                     args (:arglists m)
                     sym-fn #(symbol (str (name sym) %))
                     meta-fn #(with-meta (sym-fn %) {:arglists `'~args})
                     v-sym (meta-fn "v")
                     str-sym (meta-fn "-str")]
                 `(do
                    ~(when-not (ex v-sym)
                       `(def ~v-sym
                          ~(str "Like `" sym "`, but calls `vec` on the result.")
                          (comp vec ~sym)))
                    ~(when-not (ex str-sym)
                       `(def ~str-sym
                          ~(str "Like `" sym "`, but applies `str` to the result.")
                          (comp #(apply str %) ~sym))))))]
       `(do ~@(map make-fns syms)))))

(defmacro define-ordinal-functions
  "Defines functions third, fourth, ..., tenth to get those elements from
   a collection. Also defines firstv, secondv, ..., tenthv to get
   elements out of vectors, faster than using first, second, etc."
  []
  (let [regular (range 2 10)
        vs (range 0 10)
        ordinal #(pp/cl-format nil "~:R" (inc %))]
    `(do
       ~@(for [n regular :let [ord (ordinal n)]]
           `(defn ~(symbol ord)
              ~(str "Gets the " ord " element from a collection.")
              [~'coll]
              (nth ~'coll ~n)))
       ~@(for [n vs :let [ord (ordinal n)]]
           `(defn ~(symbol (str ord "v"))
              ~(str "Gets the " ord " element from a vector. Faster than `"
                    ord "`.")
              [~'v]
              (~'v ~n))))))
