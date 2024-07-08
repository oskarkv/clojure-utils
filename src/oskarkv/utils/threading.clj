(ns oskarkv.utils.threading)

(letfn [(contains-$? [form]
          (some #{'$} (tree-seq coll? seq form)))
        (insert-$-in-one [arrow form]
          (if (contains-$? form) form (list arrow '$ form)))
        (insert-$-in-many [arrow forms]
          (map #(insert-$-in-one arrow %) forms))
        (simple-body [x arrow forms]
          `(as-> ~x ~'$
                 ~@(insert-$-in-many arrow forms)))
        (cond-body [x arrow pairs]
          `(-> ~x
             ~@(map (fn [[tst form]]
                      `(as-> ~'$
                         (if ~tst ~(insert-$-in-one arrow form) ~'$)))
                    (partition 2 pairs))))
        (some-body [x arrow forms]
          `(some-> ~x
             ~@(map (fn [form] `(as-> ~'$ ~(insert-$-in-one arrow form)))
                    forms)))]
  (defmacro ->$
    "Acts like (`as->` x $ form) when a form contains $, otherwise acts like
     `->`."
    {:style/indent 1}
    [x & forms]
    (simple-body x '-> forms))
  (defmacro ->>$
    "Acts like (`as->` x $ form) when a form contains $, otherwise acts like
     `->>`."
    {:style/indent 1}
    [x & forms]
    (simple-body x '->> forms))
  (defmacro cond->$
    "Acts like `cond->` except that when the symbol $ exists in a test or
     an expression, it will be replaced by the current value being
     threaded, and the normal `->` rules will not apply to the
     expression if the expression contains $."
    {:style/indent 1}
    [x & pairs]
    (cond-body x '-> pairs))
  (defmacro cond->>$
    "Acts like `cond->>` except that when the symbol $ exists in a test or
     an expression, it will be replaced by the current value being
     threaded, and the normal `->>` rules will not apply to the
     expression if the expression contains $."
    {:style/indent 1}
    [x & pairs]
    (cond-body x '->> pairs))
  (defmacro some->$
    "Acts like (`some->` x (as-> $ form) ...) when a form contains $,
     otherwise acts like `some->`."
    {:style/indent 1}
    [x & forms]
    (some-body x '-> forms))
  (defmacro some->>$
    "Acts like (`some->` x (as-> $ form) ...) when a form contains $,
     otherwise acts like `some->>`."
    {:style/indent 1}
    [x & forms]
    (some-body x '->> forms)))
