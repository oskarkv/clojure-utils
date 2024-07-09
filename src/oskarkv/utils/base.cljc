(ns oskarkv.utils.base
  (:require
   [oskarkv.utils.impl :as impl]))

(impl/define-ordinal-functions)

(impl/defprivatedef def- `def)

(impl/defprivatedef defmacro- `defmacro)

(defmacro ignore-exception
  "Evaluates `forms` and if an exception is thrown, instead return `nil`."
  {:style/indent 0}
  [& forms]
  `(try ~@forms (catch ~(if (:ns &env) :default 'Exception) ~'e nil)))

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

(defmacro defalias
  "Create a new var, with the value of derefing the var named by
   `target-symbol`, in the current namespace, and copies the metadata of
   `target-symbol`'s var."
  [alias-symbol target-symbol]
  `(let [var# (var ~target-symbol)
         target-thing# @var#
         meta# (meta var#)]
     (def ~alias-symbol target-thing#)
     (reset-meta! (var ~alias-symbol) meta#)))

(defmacro alias-everything
  "Alias everything, except for symbols in `exclude` from the given
   namespace into the current namespace."
  {:style/indent 1}
  [namespace-symbol & exclude]
  `(do
     ~@(for [sym (remove (set exclude)
                         (keys (ns-publics (the-ns namespace-symbol))))]
         (list `defalias sym (symbol (str namespace-symbol) (str sym))))))
