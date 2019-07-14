(ns oskarkv.utils
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [clojure.set :as set]
   #?(:clj [oskarkv.utils.impl :as impl]
      :cljs [oskarkv.utils.impl :as impl :include-macros true]))
  #?(:clj (:import (java.util Random))))

;;; Using macros defined here in later function definitions does not work when
;;; requiring from cljs. If you want to use a macro in a later function
;;; definition you have to move the macro to the impl namespace.

(impl/defprivatedef def- `def)

(impl/defprivatedef defmacro- `defmacro)

(defmacro deftype- [name & decls]
  (let [constructor (symbol (str "->" name))]
    `(do (deftype ~name ~@decls)
         ;; The metadata map will be evaluated by def,
         ;; so we have to quote the arglists which can
         ;; contain arbitrary symbols.
         (let [meta# (meta #'~constructor)
               args# (:arglists meta#)
               meta# (assoc meta# :arglists `'~args#)
               sym# (with-meta '~constructor
                      (assoc meta# :private true))]
           (eval `(def ~sym# ~~constructor))))))

(defmacro unqualify-syms
  "Returns `code` but where any occurrence of the symbols in `syms` has
   been unqualified."
  [syms code]
  (impl/unqualify-syms* syms code))

(defmacro with-gensyms
  "Let binds the symbols in `syms` to gensyms around `body`."
  [syms & body]
  `(let [~@(apply concat (for [sym syms]
                           [sym `(gensym ~(str sym))]))]
     ~@body))

(defn flatten-all
  "Like `flatten`, but also flattens sets and maps."
  [x]
  (remove coll? (tree-seq coll? seq x)))

(defn flatten-more
  "Like `flatten`, but also flattens sets."
  [x]
  (let [pred (some-fn sequential? set?)]
    (remove pred (tree-seq pred seq x))))

(defn- x!-sym? [sym x]
  (and (symbol? sym)
       (> (count (str sym)) 2)
       (str/starts-with? (str sym) (str x "!"))))

(defn- o!-sym? [sym]
  (x!-sym? sym "o"))

(defn- g!-sym? [sym]
  (x!-sym? sym "g"))

(defn- o!-sym-to-g!-sym [sym]
  (symbol (str "g!" (subs (str sym) 2))))

(defmacro ^:private defmacro-g!
  "Like defmacro, but symbols starting with g! in the body will be
   let-bound to gensyms."
  [name args & body]
  (let [syms (distinct (filter g!-sym? (flatten-all body)))]
    `(defmacro ~name ~args
       (let [~@(apply concat (for [s syms]
                               [s `(gensym ~(subs (str s) 2))]))]
         ~@body))))

(defmacro defmacro!
  "Like defmacro-g!, but symbols starting with o! in the arguments vector
   of the defined macro will be evaluated and the corresponding
   g!-symbols will be let-bound to the results, making it easy to
   evaluate the arguments only once."
  [name args & body]
  (let [os (filter o!-sym? (flatten-all args))
        gs (map o!-sym-to-g!-sym os)]
    `(defmacro-g! ~name ~args
       `(let [~~@(interleave gs os)]
          ~~@body))))

(defn printit
  "Prints `x` with println and returns `x`."
  [x]
  (println x)
  x)

(defn tapit
  "Calls `tap>` on `x` and returns `x`."
  [x]
  (tap> x)
  x)

(defn pprintit [x]
  (pp/pprint x)
  x)

(defn pprintlnit [x]
  (pp/pprint x)
  (println)
  x)

(defn sum
  "Sums the numbers in `nums`."
  [nums]
  (apply + nums))

(defn normalize
  "Returns a sequence where each number in `nums` has been divided by
   the sum of `nums`."
  [nums]
  (map * nums (repeat (/ 1 (sum nums)))))

(letfn [(rec [parents-fn xs]
          (when-let [ps (seq (mapcat parents-fn xs))]
            (concat ps (rec parents-fn ps))))]
  (defn ancestors-list
    "Returns a list of `x` and the ancestors of `x` in `h`, starting with
     the immediate parents of `x`, then the parents of them, and so
     on. `h` must be a hierarchy and defaults to the global hierarchy."
    ([x] (cons x (rec parents [x])))
    ([h x] (cons x (rec #(parents h %) [x])))))

(defn dissoc-in
  ([m [k & ks]]
   (letfn [(polymorphic-dissoc [m k]
             (if (map? m)
               (dissoc m k)
               (assoc m k nil)))]
     (if-let [inner (and ks (get m k))]
       (assoc m k (dissoc-in inner ks))
       (polymorphic-dissoc m k))))
  ([m path & more]
   (reduce dissoc-in (dissoc-in m path) more)))

(defn assoc-ins
  "Like assoc-in, but can take more key-seq-value pairs."
  [m & ks-val-pairs]
  (if-let [[ks val] (seq (take 2 ks-val-pairs))]
    (apply assoc-ins (assoc-in m ks val) (drop 2 ks-val-pairs))
    m))

(defn lastv
  "Returns the last element of `v`, a vector. More efficient than last for
   vectors."
  [v]
  (v (dec (count v))))

(defn repeatv
  "Like `repeat`, but returns a vector."
  [n x]
  (vec (repeat n x)))

(defn reductions*
  "Like `reductions`, but returns nil if coll is empty, instead of a seq
   of length 1."
  ([f coll] (if (empty? coll) nil (reductions f coll)))
  ([f init coll] (reductions f init coll)))

(def reductionsr
  "Like `reductions`, but reduces from right to left. In the returned seq,
   the first element is the last reduction."
  (impl/reversed-reductions reductions))

(def reductionsr*
  "Like `reductions*`, but reduces from right to left. In the returned seq,
   the first element is the last reduction."
  (impl/reversed-reductions reductions*))

(defn fmap
  "Applies `f` to the vals in `m` and returns a map. If `colls` are
   provided,applies `f` to the vals in `m` and the items from `colls` in
   parallel, as with `clojure.core/map`, but keep in mind that the order
   of the map elements is unreliable."
  ([f m] (into {} (map (fn [[k v]] [k (f v)])) m))
  ([f m & colls] (zipmap (keys m) (apply map f (vals m) colls))))

(defn keeps
  "Like `keep`, but can take more than one coll, similar to `map`."
  ([f] (keep f))
  ([f coll] (keep f coll))
  ([f coll & colls]  (remove nil? (apply map f coll colls))))

(defn call-times
  "Calls `f` on `args`, then again on the result, and so on, `n` times."
  [n f & args]
  (let [n (int n)]
    (if-not (pos? n)
      (first args)
      (loop [i (dec n) result (apply f args)]
        (if (zero? i)
          result
          (recur (dec i) (f result)))))))

(defn rec==
  "Returns true if the elements of two or more nested data structures are
   equal, and uses `==` if the elements are numbers."
  ([x] true)
  ([x y]
   (cond (and (number? x) (number? y)) (== x y)
         (and (coll? x) (coll? y)) (and (== (count x) (count y))
                                        (every? true? (map rec== x y)))
         :else (= x y)))
  ([x y & more]
   (if (rec== x y)
     (apply rec== x (first more) (rest more))
     false)))

(def not== (complement ==))

(defn flip
  "Returns a new function that like `f` but has the order of the first and
   second argument flipped."
  [f]
  (fn
    ([x y] (f y x))
    ([x y & more] (apply f y x more))))

(defn flips
  "Returns a new function that like `f` but has the order the arguments
   reversed."
  [f]
  (comp (partial apply f) reverse list))

(defn conj-some
  "Conjes `x` onto `coll` if `x` is not nil."
  [coll x]
  (if (some? x) (conj coll x) coll))

(defn random-pick
  "Randomly picks a key in `chance-map`, where each value is the relative
   chance for the key."
  [chance-map]
  (let [total (apply + (vals chance-map))]
    (reduce (fn [left [k v]]
              (if (> left v)
                (- left v)
                (reduced k)))
            (rand total)
            chance-map)))

(defn chance
  "Returns true with the given probability."
  [probability]
  (< (rand) probability))

(defn move-in
  "Moves the value in an associative structure `m` at `from-path` (a
   sequence of keys) to `to-path` (a sequences of keys)."
  [m from-path to-path]
  (-> m
    (assoc-in to-path (get-in m from-path))
    (dissoc-in from-path)))

(defn swap-in
  "Swaps the values in an associative structure `m` at `path1` (a sequence
   of keys) and `path2` (a sequences of keys)."
  [m path1 path2]
  (-> m
    (assoc-in path1 (get-in m path2))
    (assoc-in path2 (get-in m path1))))

(defn swap
  "Swaps the values at keys `k1` and `k2` in `m`."
  [m k1 k2]
  (-> m (assoc k1 (m k2)) (assoc k2 (m k1))))

(defn remove-map-nils
  "Dissoces the keys that are associated with nil from `m`."
  [m]
  (into {} (filter (comp some? val)) m))

(defn domap
  "Like `map`, but also calls doall on the result."
  [& args]
  (doall (apply map args)))

(defn runmap
  "Like `map`, but also calls dorun on the result."
  [& args]
  (dorun (apply map args)))

(defn iterate-some
  "Like `iterate`, but stops when `f` returns nil."
  [f x]
  (take-while some? (iterate #(when % (f %)) x)))

(defn vectorize
  "Turns all sequences in `form` into vectors."
  [form]
  (walk/postwalk (fn [form] (if (seq? form) (vec form) form)) form))

(defn repeat-str
  "Returns a string of `s` repeated `n` times."
  [n s]
  (apply str (repeat n s)))

(defn pair-cycle
  "Returns ((n1 n2) (n2 n3) ... (nn n1)) for input (n1 n2 ... nn)."
  [coll]
  (take (count coll) (partition 2 1 (cycle coll))))

(defn zip
  "Returns a lazy sequence of vectors, where the i:th vector contains the
   i:th elements of the arguments, in the order the arguments were
   given."
  ([coll1 coll2] (map vector coll1 coll2))
  ([coll1 coll2 & more]
   (apply map vector coll1 coll2 more)))

(defn reject-indices
  "Returns a vector that is like `coll` but with the elements at `indices`
   removed."
  [coll indices]
  (let [iset (set indices)]
    (vec (keep-indexed (fn [i x] (if-not (iset i) x)) coll))))

(defn reject-keys
  "Removes the keys in `keyseq` from `m`."
  [m keyseq]
  (select-keys m (remove (set keyseq) (keys m))))

(defn select-random-keys
  "Returns a map with `num` mappings from `m`, selected randomly."
  [m num]
  (select-keys m (take num (shuffle (keys m)))))

(defn derive-many
  "Derive all elements of `coll` from `parent` in hierarchy `h`, or the
   global hierarchy if `h` is not provided."
  ([coll parent] (runmap #(derive % parent) coll))
  ([h coll parent]
   (reduce (fn [h elem] (derive h elem parent))
           h coll)))

(defn fdefault
  "Takes a function `f` and a default value `default`, and returns a
   function that is like `f` but returns `default` when `f` returns nil."
  [f default]
  (fn [& args]
    (let [r (apply f args)]
      (if (nil? r)
        default
        r))))

(defn first-some
  "Returns the first non-nil element of `coll`."
  [coll]
  (first (remove nil? coll)))

(defn +some
  "Like `+`, but ignores nil arguments."
  [& args]
  (apply + (remove nil? args)))

(defn *some
  "Like `*`, but ignores nil arguments."
  [& args]
  (apply * (remove nil? args)))

(defn avg
  "Returns the average of `nums`."
  [& nums]
  (/ (apply + nums) (count nums)))

(defn difference
  "Like `set/difference`, but works for arguments that are not sets."
  [& colls]
  (apply set/difference (map set colls)))

(defn union
  "Like `set/union`, but works for arguments that are not sets."
  [& colls]
  (apply set/union (map set colls)))

(defn intersection
  "Like `set/intersection`, but works for arguments that are not sets."
  [& colls]
  (apply set/intersection (map set colls)))

(defn infinite-shuffle [coll]
  (lazy-cat (shuffle coll) (infinite-shuffle coll)))

(defn takes
  [[n & ns] coll]
  (when n
    (cons (take n coll)
          (takes ns (drop n coll)))))

(defn- interleave-runs
  "Returns the lengths of runs of elements from seq-a if one were to
   interleave seq-a with seq-b as evenly as possible, given that seq-a
   has length `a` and seq-b has length `b`, `a` >= `b`. The imagined
   interleaving starts with a run of elements from seq-a."
  [a b]
  (let [ib (inc b)
        n (int (/ a ib))
        left (rem a ib)]
    (concat (repeat left (inc n))
            (repeat (- ib left) n))))

(defn uneven-interleave
  "Interleaves the two given sequences, which may be of different lengths,
   as evenly as possible."
  [s1 s2]
  (letfn [(interleave* [s1 s2 rs]
            (when (seq s1)
              (lazy-cat
               (take (first rs) s1)
               (take 1 s2)
               (interleave* (drop (first rs) s1) (rest s2) (rest rs)))))]
    (if (< (count s1) (count s2))
      (uneven-interleave s2 s1)
      (interleave* s1 s2 (interleave-runs (count s1) (count s2))))))

(letfn [(all-pairs* [f]
          (fn [coll]
            (loop [pairs #{} left coll]
              (if-let [rst (next left)]
                (recur (into pairs (map #(f (first left) %)) rst) (rest left))
                pairs))))]
  (defn all-pairs
    "Returns a set of all possible pairs (vectors) of items in `coll`."
    [coll]
    ((all-pairs* vector) coll))
  (defn all-set-pairs
    "Returns a set of all possible pairs (sets) of items in `coll`."
    [coll]
    ((all-pairs* hash-set) coll)))

(defn indexed [coll]
  (map-indexed vector coll))

(defn invert-map
  "Returns a new map that has the vals of `m` mapped to the keys of `m`."
  [m]
  (reduce (fn [m [k v]] (assoc m v k)) {} m))

(defn pred-union
  "Returns a function that returns true if and only if at least one
   predicate given to `pred-union` returns true for the same
   arguments. In other words, `pred-union` is similar to `some-fn`, but
   the predicates get as input all arguments passed to the returned
   function at once."
  [& ps]
  (fn [& args]
    (some #(apply % args) ps)))

(defn pred-intersection
  "Returns a function that returns true if and only if all predicates
   given to `pred-intersection` return true for the same arguments. In
   other words, `pred-intersection` is similar to `every-pred`, but the
   predicates get as input all arguments passed to the returned function
   at once."
  [& ps]
  (fn [& args]
    (every? #(apply % args) ps)))

(defn bfs-waves
  "Returns a lazy sequence of the waves of successors in a breadth-first
   search from `start` using the given function `successors` to generate
   the successors of a node."
  [start successors]
  (lazy-seq
   (letfn [(bfs* [prevs visited]
             (lazy-seq
              (let [ss (set/difference (set (mapcat successors prevs)) visited)]
                (if (seq ss)
                  (cons ss (bfs* ss (set/union visited ss)))))))]
     (cons #{start} (bfs* [start] #{start})))))

(defn bfs
  "Returns a lazy sequence of nodes in a breadth-first search from `start`
   using the given function `successors` to generate the successors of a
   node."
  [start successors]
  (apply concat (bfs-waves start successors)))

(defn dfs
  "Returns a lazy sequence of nodes in a depth-first search from `start`
   using the given function `successors` to generate the successors of a
   node."
  [start successors]
  (let [visited (volatile! #{})
        dfs* (fn dfs* [curr]
               (lazy-seq
                (when-not (@visited curr)
                  (vswap! visited conj curr)
                  (cons curr (mapcat dfs* (successors curr))))))]
    (dfs* start)))

(defn lazy-seq? [x]
  (instance?
   #?(:cljs cljs.core/LazySeq
      :clj clojure.lang.LazySeq) x))

(defn current-time-ms []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

(defmacro while-let
  "Syntax: (while-let [form expr] body). Evaluate expr each iteration. If
   it's truthy, evaluate body, with form, which can be destructured,
   let-bound to the result of evaluating expr."
  [bindingvec & body]
  (let [form (bindingvec 0) expr (bindingvec 1)]
    `(loop [result# ~expr]
       (when result#
         (let [~form result#]
           ~@body
           (recur ~expr))))))

(defmacro dotimes*
  "Like `dotimes`, but allows more than one binding,
   e.g. (dotimes* [i (range 10) j (range 10)] body)."
  [bindsvec & body]
  (if (> (count bindsvec) 2)
    `(dotimes ~(vec (take 2 bindsvec))
       (dotimes* ~(vec (drop 2 bindsvec))
         ~@body))
    `(dotimes ~bindsvec ~@body)))

(letfn [(const [x]
          (with-meta x {:const true}))]
  (defmacro defconst
    ([sym value]
     (list `def (const sym) value))
    ([sym docstring value]
     (list `def (const sym) docstring value)))
  (defmacro defconsts [& pairs]
    (let [pairs (partition 2 pairs)]
      `(do ~@(map (fn [[sym val]] `(def ~(const sym) ~val))
                  pairs)))))

(defmacro make-map
  "Returns a map mapping keywords with the names of the symbols in `syms`
   to their values in the current context."
  [& syms]
  (zipmap (map keyword syms) syms))

(defmacro condf
  "Takes an object `obj` and a set of test-fn/expr `pairs`. It evaluates
   (test-fn `obj`) for each pair in order, and returns the expr of the
   pair if the test-fn returns logical true. If no pair matches, returns
   nil."
  [obj & pairs]
  (when pairs
    `(if (~(first pairs) ~obj)
       ~(second pairs)
       (condf ~obj ~@(next (next pairs))))))

(defmacro cond-pairs
  "Like `cond`, but expects each test-expr pair to be wrapped in a
   vector. Useful for readability sometimes."
  [& vs]
  `(cond ~@(apply concat vs)))

(defmacro when-lets [bindings & body]
  (if (seq bindings)
    `(when-let ~(subvec bindings 0 2)
       (when-lets ~(subvec bindings 2)
         ~@body))
    `(do ~@body)))

(defmacro if-lets
  "If every binding is truthy, do the `then` branch, else the `else`
   branch."
  [bindings then else]
  (if (seq bindings)
    `(if-let ~(subvec bindings 0 2)
       (if-lets ~(subvec bindings 2)
         ~then
         ~else)
       ~else)
    then))

;; alter-var-root does not exist in ClojureScript
(defmacro log-fn-io
  "Replace the function named in its var with a function that wraps it,
   and prints its input and output."
  [fn-sym]
  `(alter-var-root
    #'~fn-sym
    (fn [f#]
      (fn [& args#]
        (println "Input to" '~fn-sym)
        (pp/pprint args#)
        (let [r# (apply f# args#)]
          (println "Output from" '~fn-sym)
          (pp/pprint r#)
          r#)))))

(letfn [(contains-$? [form]
          (some #{'$} (tree-seq coll? seq form)))
        (insert-$-in-one [arrow form]
          (cond (contains-$? form) form
                :else (list arrow '$ form)))
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
    "Acts like (`as->` x $ form) if a form contains $, otherwise acts like
     `->`."
    [x & forms]
    (simple-body x '-> forms))
  (defmacro ->>$
    "Acts like (`as->` x $ form) if a form contains $, otherwise acts like
     `->>`."
    [x & forms]
    (simple-body x '->> forms))
  (defmacro cond->$
    "Acts like `cond->` except that if the symbol $ exists in a test or an
     expression, it will be replaced by the current value being threaded,
     and the normal `->` rules will not apply to the expression."
    [x & pairs]
    (cond-body x '-> pairs))
  (defmacro cond->>$
    "Acts like `cond->>` except that if the symbol $ exists in a test or an
     expression, it will be replaced by the current value being threaded,
     and the normal `->>` rules will not apply to the expression."
    [x & pairs]
    (cond-body x '->> pairs))
  (defmacro some->$
    "Acts like (`some->` x (as-> $ form) ...) if a form contains $,
     otherwise acts like `some->`."
    [x & forms]
    (some-body x '-> forms))
  (defmacro some->>$
    "Acts like (`some->` x (as-> $ form) ...) if a form contains $,
     otherwise acts like `some->>`."
    [x & forms]
    (some-body x '->> forms)))

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
  ([] (rand-gaussian 1 0))
  ([sd] (rand-gaussian sd 0))
  ([sd mean] (+ (* sd (rand-gaussian*) mean))))

(defn rand-binomial
  "Returns a random number from the binomial distribution with parameters
   `n` and `p`, i.e. the number of random tests among `n` that were
   successful, with each test had success probability `p`."
  [n p]
  (apply + (repeatedly n #(if (chance p) 1 0))))

(defn rand-uniform
  "Returns a random number between `min` and `max`, with `min` and `max`
   defaulting to 0 and 1 respectively."
  ([] (rand))
  ([max] (* max (rand)))
  ([min max] (+ min (* (rand) (- max min)))))

(defn rand-uniform-int
  "Returns a random integer between `min` and `max`, both inclusive.
   If called with one argument, `min` defaults to 0."
  ([max] (rand-int (inc max)))
  ([min max] (+ min (rand-int (inc (- max min))))))

#?(:clj
   (do
     (defmacro defmemoized [& args]
       `(do (defn ~@args)
            (alter-var-root (var ~(first args)) memoize)))

     (defmacro defs
       "Like `def`, but can take several symbol-value pairs, and can
        destructure like `let`."
       [& bindings]
       (let [let-expr (macroexpand `(let ~(vec bindings)))
             new-let-bindings (vec (mapcat
                                    (fn [[sym expr]]
                                      (if (str/includes? (str sym) "__")
                                        [sym expr]
                                        [sym expr '_ `(def ~sym ~sym)]))
                                    (partition 2 (second let-expr))))]
         `(let* ~(vec new-let-bindings))))

     (def empty-queue clojure.lang.PersistentQueue/EMPTY)

     (defmacro error-printing-future
       "Like `future`, but prints stack traces to `*err*`."
       [& body]
       `(future (try ~@body (catch Exception e# (.printStackTrace e# *err*)))))

     (defmacro start-new-thread
       "Start a new Java thread the given `name` and `body`."
       [name & body]
       `(.start (Thread. (fn [] ~@body) ~name)))

     (defn current-thread-id [msg]
       (.getId (Thread/currentThread)))

     (defn current-thread-name []
       (.getName (Thread/currentThread)))

     (defn throw-error [& msg]
       (throw (Error. (apply str msg))))

     (defn throw-ex [& msg]
       (throw (Exception. (apply str msg))))

     (defmacro take-at-least-ms
       "Execute `body`. If it takes less than `ms` ms, sleep for the remaining
        time. If it takes more, print a warning."
       [ms & body]
       `(let [start# (current-time-ms)
              result# (do ~@body)
              stop# (current-time-ms)
              took# (- stop# start#)]
          (if (>= ~ms took#)
            (Thread/sleep (- ~ms took#))
            (println "WARNING:" (current-thread-name)
                     "took longer than expected to execute" '~body))
          result#))))
