(ns oskarkv.utils
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [clojure.set :as set]
   [clojure.math.numeric-tower :as math]
   #?(:clj [oskarkv.utils.impl :as impl]
      :cljs [oskarkv.utils.impl :as impl :include-macros true]))
  #?(:clj (:import (java.util Random)))
  (:use com.rpl.specter))

;;; Using macros defined here in later function definitions does not
;;; work when requiring from cljs. If you want to use a macro in a later
;;; function definition you have to move the macro to the impl
;;; namespace. If you want to use a macro in a macro definition you can
;;; move the later definition into a #?(:clj ...) block. All macros
;;; could be in #?(:clj ...) blocks, but the extra indendation is
;;; annoying, so I avoid it if not necessary.

;; WARNING: Definitions of the macros in this `do` block exist in both
;; src/utils.cljc and src/utils/impl.cljc , because they are both used
;; in functions in src/utils.cljc, and should be themselves defined in
;; src/utils.cljc for easy access to users of the utils namespace.
(do
  (defmacro ignore-exception
    "Evaluates `forms` and if an exception is thrown, instead return `nil`."
    {:style/indent 0}
    [& forms]
    `(try ~@forms (catch ~(if (:ns &env) :default 'Exception) ~'e nil))))

(impl/define-ordinal-functions)

(impl/defprivatedef def- `def)

(impl/defprivatedef defmacro- `defmacro)

(defn walker*
  "Like `com.rpl.specter/walker`, but ignores exceptions in the given
   function."
  [f]
  (walker #(ignore-exception (f %))))

(defmacro recursive-map-search-path [args must-arg result-path]
  `(recursive-path ~args p#
     (cond-path [map? (must ~must-arg)] (multi-path ~result-path [ALL p#])
                coll? [ALL p#]
                STOP)))

(def find-vals
  "A specter navigator to recursively find the values of the given key in
   a nested data structure."
  (recursive-map-search-path [k] k k))

(def maps-with
  "A specter navigator to recursively find maps that contain the given key in
   a nested data structure."
  (recursive-map-search-path [k] k STAY))

(defmacro defalias
  "Create a new var with the value of evaluating `target-symbol` in the
   current namespace, and copies the metadata of `target-symbol`'s var."
  [alias-symbol target-symbol]
    `(do (let [target-thing# @(var ~target-symbol)]
       (def ~alias-symbol target-thing#)
         (reset-meta! (var ~alias-symbol) (meta (var ~target-symbol))))))

(defmacro use-names [ns-sym pred]
  (let [pred (eval pred)]
    `(do
       ~@(map (fn [[sym v]]
                (list `defalias sym (symbol (name ns-sym) (name sym))))
              (filter (fn [[sym v]] (pred (name sym))) (ns-publics ns-sym))))))

(defalias invert-map set/map-invert)

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

(defmacro condp*
  "Like `condp` but the predicate takes the arguments in the reverse
   order."
  {:style/indent 2}
  [pred & args]
  `(condp (flip ~pred) ~@args))

(defn ffilter
  "Like (first (filter `pred` `coll`)."
  [pred coll]
  (first (filter pred coll)))

(defn floor
  "Returns a the floor of `x` as a long."
  [x]
  (long (math/floor x)))

(defn ceil
  "Returns a the ceil of `x` as a long."
  [x]
  (long (math/ceil x)))

(defn parse-int
  "Parses a long with Long/parseLong."
  ([x] (Long/parseLong x))
  ([x radix] (Long/parseLong x radix)))

(defn sum
  "Sums the numbers in `nums`."
  [nums]
  (reduce + nums))

(defn normalize
  "Returns a sequence where each number in `nums` has been divided by
   the absolute value of the sum of `nums`."
  [nums]
  (map * nums (repeat (/ (abs (sum nums))))))

(defn fmap
  "Applies `f` to the vals in `m` and returns a map. If `colls` are
   provided,applies `f` to the vals in `m` and the items from `colls` in
   parallel, as with `clojure.core/map`, but keep in mind that the order
   of the map elements is unreliable."
  ([f m] (into {} (map (fn [[k v]] [k (f v)])) m))
  ([f m & colls] (zipmap (keys m) (apply map f (vals m) colls))))

(defn kmap
  "Applies `f` to the keys in `m` and returns a map. If `colls` are
   provided,applies `f` to the keys in `m` and the items from `colls` in
   parallel, as with `clojure.core/map`, but keep in mind that the order
   of the map elements is unreliable."
  ([f m] (into {} (map (fn [[k v]] [(f k) v])) m))
  ([f m & colls] (zipmap (apply map f (keys m) colls) (vals m))))

(defn keeps
  "Like `keep`, but can take more than one coll, similar to `map`."
  ([f] (keep f))
  ([f coll] (keep f coll))
  ([f coll & colls]  (remove nil? (apply map f coll colls))))

(defn flatten-all
  "Like `flatten`, but also flattens sets and maps."
  [x]
  (remove coll? (tree-seq coll? seq x)))

(defn flatten-more
  "Like `flatten`, but also flattens sets."
  [x]
  (let [pred (some-fn sequential? set?)]
    (remove pred (tree-seq pred seq x))))

(defn indexed
  "Returns ([0 a] [1 b] ...) for input (a b ...)."
  [coll]
  (map-indexed vector coll))

(defn zip
  "Returns a lazy sequence of vectors, where the i:th vector contains the
   i:th elements of the arguments, in the order the arguments were
   given."
  ([coll1 coll2] (map vector coll1 coll2))
  ([coll1 coll2 & more]
   (apply map vector coll1 coll2 more)))

(defn iterate-some
  "Like `iterate`, but stops when `f` returns nil."
  [f x]
  (take-while some? (iterate #(when % (f %)) x)))

(defn pair-cycle
  "Returns ((n1 n2) (n2 n3) ... (nn n1)) for input (n1 n2 ... nn)."
  [coll]
  (take (count coll) (partition 2 1 (cycle coll))))

(defn takes
  "Like `take`, but the first argument is a seq of numbers. Returns a seq
   of seqs of elements from `coll`."
  [[n & ns] coll]
  (when n
    (cons (take n coll)
          (takes ns (drop n coll)))))

(defn take-while-pairs
  "Returns a lazy sequence of successive items from `coll` while (`pred`
   previous-item item) returns true. The returned sequence will have at
   least one item if `coll` is not empty. If `keyfn` is provided,
   `keyfn` is applied to the items before using `pred`."
  ([pred coll]
   (lazy-seq
    (when-let [[a b :as s] (seq coll)]
      (if (and (not (empty? (rest s)))
               (pred a b))
        (cons a (take-while-pairs pred (rest s)))
        (list a)))))
  ([keyfn pred coll]
   (take-while-pairs #(pred (keyfn %) (keyfn %2)) coll)))

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

(impl/make-v-and-str-fns
 [filterv
  mapv]
 butlast
 concat
 dedupe
 distinct
 drop
 drop-last
 drop-while
 filter
 flatten
 flatten-all
 flatten-more
 indexed
 interleave
 interpose
 iterate-some
 keep
 keeps
 map
 map-indexed
 mapcat
 normalize
 pair-cycle
 range
 remove
 repeat
 replace
 rest
 reverse
 shuffle
 sort
 sort-by
 take
 take-last
 take-nth
 take-while
 take-while-pairs
 uneven-interleave
 zip)

(defn lastv
  "Returns the last element of `v`, a vector. More efficient than `last`
   for vectors."
  [v]
  (v (dec (count v))))

(defn transpose
  "Transposes `matrix`. `matrix` can be any collection of collections."
  [matrix]
  (apply zipv matrix))

(defn printit
  "Prints `x` with `println` and returns `x`."
  [x]
  (println x) x)

(defn tapit
  "Calls `tap>` on `x` and returns `x`."
  [x]
  (tap> x) x)

(defalias pprint pp/pprint)

(defn pprintit
  "Pprints `x` and returns `x`."
  [x]
  (pp/pprint x) x)

(defn pprintlnit
  "Pprints `x` followed by a blank line and returns `x`."
  [x]
  (pp/pprint x) (println) x)

(defn sign
  "Returns 1 for positive numbers, -1 for negative numbers, and 0 for 0."
  [x]
  (condf x
    pos? 1
    neg? -1
    0))

(defn avg
  "Returns the average of `nums`."
  [& nums]
  (/ (sum nums) (count nums)))

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
  "Dissociates the value from an associative structure `m` at the given
   path. If the last structure is not a map, `nil` will be associated
   with the index that should be dissociated. Can take multiple path
   vectors to dissociate multiple values."
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
  "Like `assoc-in`, but can take more path-value pairs."
  [m & path-val-pairs]
  (if-let [[path val] (seq (take 2 path-val-pairs))]
    (apply assoc-ins (assoc-in m path val) (drop 2 path-val-pairs))
    m))

(defn move-in
  "Moves the value in an associative structure `m` at `from-path` (a
   sequence of keys) to `to-path` (a sequences of keys)."
  [m from-path to-path]
  (-> m
    (assoc-in to-path (get-in m from-path))
    (dissoc-in from-path)))

(defn move
  "Moves the value in an associative structure `m` at key `from` to key
   `to`."
  [m from to]
  (-> m
    (assoc to (m from))
    (dissoc from)))

(defn swap-in
  "Swaps the values in an associative structure `m` at `path1` and
   `path2` (sequenceses of keys)."
  [m path1 path2]
  (-> m
    (assoc-in path1 (get-in m path2))
    (assoc-in path2 (get-in m path1))))

(defn swap
  "Swaps the values at keys `k1` and `k2` in `m`."
  [m k1 k2]
  (-> m (assoc k1 (m k2)) (assoc k2 (m k1))))

(defn vector*
  "Like `list*`, but returns a vector."
  [& args]
  (apply vector args))

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

(defn first-index
  "Returns the index of the first item in `coll` that satisfies `pred`."
  [pred coll]
  (first (keep-indexed (fn [i val] (when (pred val) i)) coll)))

(defn infinite-shuffle
  "Returns the concatenation of (shuffle `coll`) and (infinite-shuffle
   `coll`)."
  [coll]
  (lazy-cat (shuffle coll) (infinite-shuffle coll)))

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

(defn not==
  "The complement of `==`."
  [& args]
  (not (apply == args)))

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
  "Conjes `x` onto `coll` iff `x` is not nil."
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

(defn remove-vals
  "Removes entries from `m` (a map) where the value satisfies `pred`."
  [pred m]
  (into {} (remove (comp pred val)) m))

(defn remove-map-nils
  "Removes the entries from `m` that have the value `nil`."
  [m]
  (remove-vals nil? m))

(defn domap
  "Like `map`, but also calls `doall` on the result."
  [& args]
  (doall (apply map args)))

(defn runmap
  "Like `map`, but also calls `dorun` on the result."
  [& args]
  (dorun (apply map args)))

(defn vectorize
  "Turns all sequences in `form` into vectors."
  [form]
  (walk/postwalk (fn [form] (if (seq? form) (vec form) form)) form))

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

(defn replace-keys
  "For any key in `m`, replaces it with (`rmap` key) if it exists in `rmap`."
  [rmap m]
  (let [ks (keys m) vs (vals m)]
    (zipmap (replace rmap ks) vs)))

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

(defn some-multiarg-fn
  "Returns a function f that returns the first logical true value of
   applying the given functions `fs`, in order, to the args of f. Short
   circuits like `some`."
  [& fs]
  (fn [& args]
    (some #(apply % args) fs)))

(defn every-multiarg-pred
  "Returns a function f that applies the given predicates `ps`, in order,
   to the args of f and returns false if any of them returns logical
   false, otherwise returns true. Short curcuits like `every?`."
  [& ps]
  (fn [& args]
    (every? #(apply % args) ps)))

(defn some-in
  "Like (some identity `coll`)."
  [coll]
  (some identity coll))

(defn every-in?
  "Like (every? identity `coll`)."
  [coll]
  (every? identity coll))

(defn bfs-waves
  "Returns a lazy sequence of the waves of successors in a breadth-first
   search from `start` using the given function `successors-fn` to generate
   the successors of a node."
  [start successors-fn]
  (lazy-seq
   (letfn [(bfs* [prevs visited]
             (lazy-seq
              (let [ss (set (remove visited (mapcat successors-fn prevs)))]
                (if (seq ss)
                  (cons ss (bfs* ss (set/union visited ss)))))))]
     (cons #{start} (bfs* #{start} #{start})))))

(defn bfs
  "Returns a lazy sequence of nodes in a breadth-first search from `start`
   using the given function `successors-fn` to generate the
   successors-fn of a node."
  [start successors-fn]
  (apply concat (bfs-waves start successors-fn)))

(defn dfs
  "Returns a lazy sequence of nodes in a depth-first search from `start`
   using the given function `successors-fn` to generate the
   successors-fn of a node."
  [start successors-fn]
  (let [visited (volatile! #{})
        dfs* (fn dfs* [curr]
               (lazy-seq
                (when-not (@visited curr)
                  (vswap! visited conj curr)
                  (cons curr (mapcat dfs* (successors-fn curr))))))]
    (dfs* start)))

(defn lazy-seq?
  "Returns `true` if `x` is a LazySeq."
  [x]
  (instance?
   #?(:cljs cljs.core/LazySeq
      :clj clojure.lang.LazySeq) x))

(defn current-time-ms
  "Returns the current time in ms."
  []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

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

(defn partition-by-pairs
  "Partitions `coll` by applying `pred` to consecutive pairs of `coll`,
   and when `pred` returns false, split `coll` between those
   pairs. Returns a lazy sequence of partitions. If `keyfn` is provided,
   calls `keyfn` on the elements before using `pred`."
  ([pred coll]
   (lazy-seq
    (when-let [s (seq (take-while-pairs pred coll))]
      (cons s (partition-by-pairs pred (drop (count s) coll))))))
  ([keyfn pred coll]
   (partition-by-pairs #(pred (keyfn %) (keyfn %2)) coll)))

;; Combining other structures besides maps would be strange, then one
;; couldn't, for example, concat seqs that are values in map entries.
(defn merge-nested
  "Returns a map where each nested element at a position p (a path into
   the nested maps) is the result of (reduce `f` elements) on all the
   elements of all input maps that are in position p. Does not call
   `f` if there would only be one argument."
  [f & maps]
  (let [combine-entry (fn [m e]
                        (let [[k v] e]
                          (if (contains? m k)
                            (if (or (map? v) (nil? v))
                              (assoc m k (merge-nested f (get m k) v))
                              (assoc m k (f (get m k) v)))
                            (assoc m k v))))
        combine2 (fn [m m2]
                   (reduce combine-entry m (seq m2)))]
    (when (seq maps)
      (reduce combine2 maps))))

(defn merge-nested-when
  "Returns a data structure where each nested element at a position p (a
   path into the nested data structures) is the result of (reduce `f`
   elements) on all the elements, of all the input data structures, that
   are in position p and satisfy `pred`. Considers maps and sequential
   data structures to be data structures to merge, but tests `pred`
   first, i.e. maps and sequential data structures can be merged with
   `f` if they satisfy `pred`. If only some elements (of all the data
   structures) satisfy `pred` at a position, all the elements that do
   not are discarded. If no elements (that are not maps or sequential)
   at a position p satisfy `pred`, the element of the last input data
   structure will be used at position p. If `pred` would throw and
   exception when used on an element, instead consider `pred`
   unsatisfied."
  [pred f & structs]
  (let [combine-entry (fn [struct e]
                        (let [[k v] e]
                          (if (contains? struct k)
                            (assoc struct k (merge-nested-when
                                             pred f (get struct k) v))
                            (assoc struct k v))))
        combine-seqs (fn [s s2]
                       (let [[shortest longest] (sort-by count [s s2])]
                         (cond-> (concat (map #(merge-nested-when pred f % %2)
                                              s s2)
                                         (drop (count shortest) longest))
                           (vector? s) vec)))
        combine2 (fn [struct struct2]
                   (let [pred* (fn [x]
                                 (impl/ignore-exception (pred x)))
                         sat (pred* struct)
                         sat2 (pred* struct2)]
                     (cond
                       (and sat sat2) (f struct struct2)
                       sat struct
                       sat2 struct2
                       (sequential? struct) (combine-seqs struct struct2)
                       (map? struct) (reduce combine-entry struct struct2)
                       :else struct2)))]
    (when (seq structs)
      (reduce combine2 structs))))

(defn- map-paths
  "Returns a seq of [path value] items for each value nested inside `m`
   that is not a map."
  [m]
  (letfn [(paths* [prev-path x]
            (if (map? x)
              (mapcat (fn [[k v]]
                        (paths* (conj prev-path k) v))
                      x)
              [[prev-path x]]))]
    (paths* [] m)))

(defn combine
  "Returns a map where each element at a position p (a path into the map)
   is the result of (f x1 x2 ...) where x1, x2, etc. are values from the
   input maps at position p. Never considers a (nested) map to be a
   value, and instead always considers a map to be more nesting."
  [f & maps]
  (->> (mapcat map-paths maps)
    (group-by firstv)
    (fmap (fn [xs] (apply f (map secondv xs))))
    (reduce (fn [m [path v]] (assoc-in m path v)) {})))

(defn- struct-paths
  "Returns a seq of [path value] items for each value nested inside
   `structure` that satisfies `pred`."
  [pred structure]
  (letfn [(pred* [x] (impl/ignore-exception (pred x)))
          (f [prev-path x] (mapcat (fn [[k v]]
                                     (paths* (conj prev-path k) v))
                                   x))
          (paths* [prev-path x]
            (cond
              (pred* x) [[prev-path x]]
              (map? x) (f prev-path x)
              (sequential? x) (f prev-path (indexed x))
              :else [[prev-path x]]))]
    (paths* [] structure)))

(defn- assoc-in*
  "Like `assoc-in`, but if the data structure being assoced in is a
   vector, and the key (index) does not exist in the vector, then pad
   the vector with nils before associng."
  [struct [k & ks] v]
  (let [struct* (if (vector? struct)
                  (vec (concat struct (repeat (- k (count struct)) nil)))
                  struct)]
    (if ks
      (assoc struct* k (assoc-in* (get struct* k) ks v))
      (assoc struct* k v))))

(defn- struct-kinds
  "Returns a data structure that is like `struct` but all maps and
   sequential collections have been emptied of everything that is not a
   map or seq. If any maps or sequential collections satisfies `pred`,
   they are also removed. The returned data structure will have vectors
   where `struct` had sequential collections."
  [pred struct]
  (letfn [(pred* [x] (impl/ignore-exception (pred x)))]
    (when (and (not (pred* struct))
               ((some-fn map? sequential?) struct))
      (reduce (fn [st [path v]] (assoc-in* st [path] v))
              (if (map? struct) {} [])
              (->> (map (fn [[k v]]
                          (when-let [v* (struct-kinds pred v)]
                            [k v*]))
                        (if (sequential? struct) (indexed struct) struct))
                (remove nil?))))))

(defn combine-when
  "Returns a data structure where each nested element at a
   position p (a path into the data structure) is the result of
   (f x1 x2 ...) where x1, x2, etc. are values at position p that
   satisfy `pred` from the input data structures. Considers maps and
   sequential data structures to be data structures to combine, but
   tests `pred` first, i.e. maps and sequential data structures can be
   combined with `f` if they satisfy `pred`. If only some elements at a
   position in the input data structures satisfy `pred`, the ones that
   do not are discarded. If no elements (that are not maps or
   sequential) at a position satisfy `pred` then the last element will
   be included in the returned data structure. If `pred` would throw and
   exception when used on any element, it is instead considered
   unsatisfied."
  [pred f & structs]
  (letfn [(pred* [x] (impl/ignore-exception (pred x)))]
    (->> (mapcat #(struct-paths pred %) structs)
      (group-by firstv)
      (fmap (fn [xs]
              (let [values (map secondv xs)]
                (if-let [s (seq (filter pred* values))]
                  (apply f s)
                  (last values)))))
      (reduce (fn [m [path v]]
                (assoc-in* m path v))
              (->> (map #(struct-kinds pred %) structs)
                (apply combine #(first %&)))))))

(defmacro while-let
  "Syntax: (while-let [form expr] body). Evaluate expr each iteration. If
   it's truthy, evaluate body, with form, which can be a destructuring,
   let-bound to the result of evaluating expr."
  {:style/indent 1}
  [bindingvec & body]
  (let [form (bindingvec 0) expr (bindingvec 1)]
    `(loop [result# ~expr]
       (when result#
         (let [~form result#]
           ~@body
           (recur ~expr))))))

(defmacro dotimes*
  "Like `dotimes`, but allows more than one binding, e.g.
   (dotimes* [i 5 j 6] body)."
  {:style/indent 1}
  [bindsvec & body]
  (if (> (count bindsvec) 2)
    `(dotimes ~(vec (take 2 bindsvec))
       (dotimes* ~(vec (drop 2 bindsvec))
         ~@body))
    `(dotimes ~bindsvec ~@body)))

(letfn [(const [x]
          (with-meta x {:const true}))]
  (defmacro defconst
    "Like `def` but defines a constant (:const true in metadata)."
    {:style/indent :defn}
    ([sym value]
     (list `def (const sym) value))
    ([sym docstring value]
     (list `def (const sym) docstring value)))
  (defmacro defconsts
    "Defines constants (:const true in metadata). The arguments should be
     pairs of symbols and values, e.g. (defconsts a 1 b 2)."
    {:style/indent 0}
    [& pairs]
    (let [pairs (partition 2 pairs)]
      `(do ~@(map (fn [[sym val]] `(def ~(const sym) ~val))
                  pairs)))))

(defmacro make-map
  "Returns a map from keywords with the names of the symbols in `syms`
   to their values in the current context."
  {:style/indent 0}
  [& syms]
  (zipmap (map keyword syms) syms))

(letfn [(expand [[x :as s]]
          (when (seq s)
            (if (vector? x)
              (let [[a b] x]
                (cons a (cons b (expand (rest s)))))
              (concat (take 2 s) (expand (drop 2 s))))))]
  (defmacro cond*
    "Like `cond`, but each test-expr pair may, but doesn't have to, be
     wrapped in a vector. Useful for readability sometimes."
    {:style/indent 0}
    [& args]
    `(cond ~@(expand args))))

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

(defmacro if-lets
  "Like `if-let` but can take multiple bindings. Evaluates the `then`
   branch if every binding is truthy, else the `else` branch."
  {:style/indent 1}
  [bindings then else]
  (if (seq bindings)
    `(if-let ~(subvec bindings 0 2)
       (if-lets ~(subvec bindings 2)
         ~then
         ~else)
       ~else)
    then))

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
    "Acts like `cond->` except that when the symbol $ exists in a test or an
     expression, it will be replaced by the current value being threaded,
     and the normal `->` rules will not apply to the expression."
    {:style/indent 1}
    [x & pairs]
    (cond-body x '-> pairs))
  (defmacro cond->>$
    "Acts like `cond->>` except that when the symbol $ exists in a test or
     an expression, it will be replaced by the current value being
     threaded, and the normal `->>` rules will not apply to the
     expression."
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

#?(:clj
   (do
     (defmacro deftype-
       "Like `deftype`, but the constructor, ->`name`, will be private."
       {:style/indent [2 nil nil [:defn]]}
       [name & decls]
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
       {:style/indent 1}
       [syms code]
       (impl/unqualify-syms syms code))

     (defmacro with-gensyms
       "Let binds the symbols in `syms` to gensyms around `body`."
       {:style/indent 1}
       [syms & body]
       `(let [~@(apply concat (for [sym syms]
                                [sym `(gensym ~(str sym))]))]
          ~@body))

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

     (defn- defmacro!-args-fix
       "Handles optional arguments for defmacro!. Given a set of arguments to
        `defmacro`, with optional docstring and attr-map, return a vector
        where nils have been inserted for missing optional arguments, and
        docstring and attr-map, if they exist, have been wrapped in
        vector (for splicing)."
       [& [name docstring attr-map args & body :as all]]
       (if-not (string? docstring)
         (apply defmacro!-args-fix name "" (drop 1 all))
         (if-not (map? attr-map)
           (apply defmacro!-args-fix name docstring {} (drop 2 all))
           [name
            (when (seq docstring) [docstring])
            (when (seq attr-map) [attr-map])
            args
            body])))

     (defmacro- defmacro-g!
       "Like defmacro, but symbols starting with g! in the body will be
        let-bound to gensyms."
       {:style/indent :defn}
       [& args]
       (let [[name docstring attr-map args body]
             (apply defmacro!-args-fix args)
             syms (distinct (filter g!-sym? (flatten-all body)))]
         `(defmacro ~name ~@docstring ~@attr-map ~args
            (let [~@(apply concat (for [s syms]
                                    [s `(gensym ~(subs (str s) 2))]))]
              ~@body))))

     (defmacro defmacro!
       "Like defmacro, but symbols starting with g! in the body will be
        let-bound to gensyms, and symbols starting with o! in the arguments
        vector of the defined macro will be evaluated and the corresponding
        g!-symbols will be let-bound to the results, making it easy to
        evaluate the arguments only once."
       {:style/indent :defn
        :arglists '([name docstring? attr-map? args & body])}
       [& args]
       (let [[name docstring attr-map args body]
             (apply defmacro!-args-fix args)
             os (filter o!-sym? (flatten-all args))
             gs (map o!-sym-to-g!-sym os)]
         `(defmacro-g! ~name ~@docstring ~@attr-map ~args
            `(let [~~@(interleave gs os)]
               ~~@body))))

     (defmacro! with-indented-printlns
       "Modifies `defn` and `fn` forms under this macro so that `println` in
        the `defn` and `fn` forms prints with an indentation depending on how
        deep the call is, i.e. recursive calls will print with a greater
        indent, for example."
       {:style/indent 0}
       [& code]
       (letfn [(handle-form [n form]
                 (let [before (take n form)
                       body (drop n form)]
                   (concat before
                           `((swap! ~g!depth inc)
                             (let [r# (do ~@body)]
                               (swap! ~g!depth dec)
                               r#)))))]
         (unqualify-syms [args]
           `(let [~g!depth (atom -1)
                  ~'println (fn [& args]
                              (println
                               (apply str (apply str (repeat @~g!depth "  "))
                                      (interpose " " args))))]
              ~@(walk/postwalk
                 (fn [form]
                   (if (list? form)
                     (condp = (first form)
                       'defn (handle-form
                              (if (string? (third form))
                                4 3)
                              form)
                       'fn (handle-form
                            (if (symbol? (second form))
                              3 2)
                            form)
                       form)
                     form))
                 code)))))

     (defmacro log-fn-io
       "Replace the function named in its var with a function that wraps it,
        and prints its input and output."
       {:style/indent 0}
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

     (defmacro defmemoized
       "Like `defn` but the function will be memoized with `memoize`."
       {:style/indent :defn}
       [& args]
       `(do (defn ~@args)
            (alter-var-root (var ~(first args)) memoize)))

     ;; Can't use macroexpand like this in ClojureScript, that's why
     ;; it's only defined in Clojure
     (defmacro defs
       "Like `def`, but can take several symbol-value pairs, and can
        destructure like `let`."
       {:style/indent 0}
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
       {:style/indent 0}
       [& body]
       `(future (try ~@body (catch Exception e# (.printStackTrace e# *err*)))))

     (defmacro start-new-thread
       "Start a new Java thread the given `name` and `body`."
       {:style/indent 1}
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
       {:style/indent 1}
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
