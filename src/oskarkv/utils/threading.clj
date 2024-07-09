(ns oskarkv.utils.threading
  (:require
   [clojure.core.match :refer [match]]
   [oskarkv.utils.base :refer :all]))

(defmacro when-lets
  "Like `when-let` but can take multiple bindings. Evaluates body if every
   binding is truthy."
  {:style/indent 1}
  [bindings & body]
  (if (seq bindings)
    `(when-let ~(subvec bindings 0 2)
       (when-lets ~(subvec bindings 2)
         ~@body))
    `(do ~@body)))

(defmacro when-somes
  "Like `when-some` but can take multiple bindings. Evaluates body if
   every binding is truthy."
  {:style/indent 1}
  [bindings & body]
  (if (seq bindings)
    `(when-some ~(subvec bindings 0 2)
       (when-somes ~(subvec bindings 2)
         ~@body))
    `(do ~@body)))

(defmacro if-lets
  "Like `if-let` but can take multiple bindings. Evaluates the `then`
   branch if every binding is truthy, else the `else` branch."
  {:style/indent 1}
  ([bindings then]
   `(if-lets ~bindings ~then))
  ([bindings then else]
   (if (seq bindings)
     `(if-let ~(subvec bindings 0 2)
        (if-lets ~(subvec bindings 2)
          ~then
          ~else)
        ~else)
     then)))

(defmacro if-somes
  "Like `if-some` but can take multiple bindings. Evaluates the `then`
   branch if every binding is nonnil, else the `else` branch."
  {:style/indent 1}
  ([bindings then]
   `(if-somes ~bindings ~then))
  ([bindings then else]
   (if (seq bindings)
     `(if-some ~(subvec bindings 0 2)
        (if-somes ~(subvec bindings 2)
          ~then
          ~else)
        ~else)
     then)))

(defn- contains-$? [form]
  (some #{'$} (tree-seq coll? seq form)))

(defn- insert-$-in-one [arrow form]
  (if (contains-$? form) form (list arrow '$ form)))

(defn- insert-$-in-many [arrow forms]
  (map #(insert-$-in-one arrow %) forms))

(defn- simple-body [x arrow forms]
  `(as-> ~x ~'$
         ~@(insert-$-in-many arrow forms)))

(defn- cond-body [x arrow pairs]
  `(-> ~x
     ~@(map (fn [[tst form]]
              `(as-> ~'$
                 (if ~tst ~(insert-$-in-one arrow form) ~'$)))
            (partition 2 pairs))))

(defn- some-body [x arrow forms]
  `(some-> ~x
     ~@(map (fn [form] `(as-> ~'$ ~(insert-$-in-one arrow form)))
            forms)))

(defmacro ->$
  "Acts like (`as->` x $ form) when a form contains $, otherwise acts like
   `->`."
  {:style/indent 1}
  [x & forms]
  (simple-body x `-> forms))

(defmacro ->>$
  "Acts like (`as->` x $ form) when a form contains $, otherwise acts like
   `->>`."
  {:style/indent 1}
  [x & forms]
  (simple-body x `->> forms))

(defmacro cond->$
  "Acts like `cond->` except that when the symbol $ exists in a test or
   an expression, it will be replaced by the current value being
   threaded, and the normal `->` rules will not apply to the
   expression if the expression contains $."
  {:style/indent 1}
  [x & pairs]
  (cond-body x `-> pairs))

(defmacro cond->>$
  "Acts like `cond->>` except that when the symbol $ exists in a test or
   an expression, it will be replaced by the current value being
   threaded, and the normal `->>` rules will not apply to the
   expression if the expression contains $."
  {:style/indent 1}
  [x & pairs]
  (cond-body x `->> pairs))

(defmacro some->$
  "Acts like (`some->` x (as-> $ form) ...) when a form contains $,
   otherwise acts like `some->`."
  {:style/indent 1}
  [x & forms]
  (some-body x `-> forms))

(defmacro some->>$
  "Acts like (`some->` x (as-> $ form) ...) when a form contains $,
   otherwise acts like `some->>`."
  {:style/indent 1}
  [x & forms]
  (some-body x `->> forms))

(def- if? '#{if if-not})
(def- if-let? '#{if-let if-some if-lets if-somes})
(def- when? '#{when when-not})
(def- when-let? '#{when-let when-some when-lets when-somes})
(def- when->if '{when if
                 when-not if-not
                 when-let if-let
                 when-some if-some})

(defn- normalize [x]
  (or ('{when-let when-lets
         when-some when-somes
         if-let if-lets
         if-some if-somes} x)
      x))

(defn- catch-clause?
  [clause]
  (and (list? clause)
       (or (= 'catch (first clause))
           (= 'finally (first clause)))))

(defn- thread-first
  [value form]
  (apply list (first form) value (rest form)))

(defn- thread-last
  [value form]
  (concat form [value]))

(defn thread
  [func value form]
  (let [value-symbol (gensym)
        thread* (partial thread func)
        thread-if$ (fn [val form]
                     (if (contains-$? form)
                       (thread* val form)
                       form))
        handle-bindings (fn [bindings]
                          (->> (partition 2 bindings)
                            (map (fn [[sym form]]
                                   [sym (thread-if$ value-symbol form)]))
                            (apply concat)
                            vec))]
    (match form
      (['case expr & clauses] :seq)
      (let [expr (thread-if$ value-symbol expr)
            has-default? (odd? (count clauses))
            default (when has-default?
                      (thread* value-symbol (last clauses)))
            clauses (->> clauses
                      (partition 2)
                      (mapcat (fn [[test then]]
                                [test (thread* value-symbol then)])))
            clauses (if has-default?
                      (concat clauses [default])
                      (concat clauses [value-symbol]))]
        `(let [~value-symbol ~value]
           (case ~expr ~@clauses)))

      (['cond & clauses] :seq)
      (let [clauses (vec clauses)
            has-else? (= :else (clauses (- (count clauses) 2)))
            clauses (->> clauses
                      (partition 2)
                      (mapcat (fn [[test then]]
                                [(thread-if$ value-symbol test)
                                 (thread* value-symbol then)])))
            clauses (cond-> clauses
                      (not has-else?) (concat [:else value-symbol]))]
        `(let [~value-symbol ~value]
           (cond ~@clauses)))

      (['try & body] :seq)
      (let [[body extra-clauses] (split-with (complement catch-clause?) body)
            body (reduce thread* value-symbol body)
            extra-clauses (mapv (fn [clause]
                                  (println "clause" clause (class clause))
                                  (match clause
                                    (['catch kind name & clause-body] :seq)
                                    (let [body (reduce thread* value-symbol clause-body)]
                                      `(catch ~kind ~name ~body))
                                    :else
                                    ;; It does not make much sense to
                                    ;; thread through finally.
                                    clause))
                                extra-clauses)]
        (do
          (println extra-clauses)
          `(let [~value-symbol ~value]
             (try ~body ~@extra-clauses))))

      ([if :guard if? cond then else] :seq)
      (let [cond (thread-if$ func value-symbol cond)
            then (thread* value-symbol then)
            else (thread* value-symbol else)]
        `(let [~value-symbol ~value]
           (~if ~cond ~then ~else)))

      ([if :guard if-let? bindings then else] :seq)
      (let [bindings (handle-bindings bindings)
            then (thread* value-symbol then)
            else (thread* value-symbol else)
            ifs (normalize if)]
        `(let [~value-symbol ~value ~@bindings]
           (~ifs ~bindings ~then ~else)))

      ([if :guard if? cond then] :seq)
      (let [cond (thread-if$ func value-symbol cond)
            then (thread* value-symbol then)]
        `(let [~value-symbol ~value]
           (~if ~cond ~then ~value-symbol)))

      ([if :guard if-let? bindings then] :seq)
      (let [bindings (handle-bindings bindings)
            then (thread* value-symbol then)
            ifs (normalize if)]
        `(let [~value-symbol ~value ~@bindings]
           (~ifs ~bindings ~then ~value-symbol)))

      ([when :guard when? cond & body] :seq)
      (let [cond (thread-if$ func value-symbol cond)
            body (reduce thread* value-symbol body)
            if (when->if when)]
        `(let [~value-symbol ~value]
           (~if ~cond ~body ~value-symbol)))

      ([when :guard when-let? bindings & body] :seq)
      (let [bindings (handle-bindings bindings)
            body (reduce thread* value-symbol body)
            if (when->if when)
            ifs (normalize if)]
        `(let [~value-symbol ~value ~@bindings]
           (~ifs ~bindings ~body ~value-symbol)))

      (['do & body] :seq)
      (reduce thread* value body)

      (['let bindings & body] :seq)
      (let [bindings (handle-bindings bindings)
            body (reduce thread* value-symbol body)]
        `(let [~value-symbol ~value ~@bindings]
           ~body))

      (f :guard list?)
      (if (contains-$? form)
        `(as-> ~value ~'$ ~form)
        (func value form))

      '$
      value

      :else
      (list form value))))

(defmacro +>$
  {:style/indent 1}
  [value & forms]
  (reduce (partial thread thread-first) value forms))

(defmacro +>>$
  {:style/indent 1}
  [value & forms]
  (reduce (partial thread thread-last) value forms))

(defn make-threading-fn [arrow [fst & rst :as args] &form]
  (let [arg (gensym)
        [argsv body] (if (and (vector? fst)
                              (every? symbol? fst))
                       [fst rst]
                       [[arg] args])
        first-arg (first argsv)]
    `(with-meta
       (fn ~argsv
         (~arrow ~first-arg ~@body))
       ~(meta &form))))

(defmacro fn-> [& args]
  (make-threading-fn '->$ args &form))

(defmacro fn->> [& args]
  (make-threading-fn '->>$ args &form))

(defmacro fn+> [& args]
  (make-threading-fn '+>$ args &form))

(defmacro fn+>> [& args]
  (make-threading-fn '+>>$ args &form))
