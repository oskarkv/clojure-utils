(ns oskarkv.utils.general
  (:require
   [clojure.math :as math]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [com.rpl.specter :as s]
   [oskarkv.utils.base :as b]
   [oskarkv.utils.impl :as impl]
   [oskarkv.utils.specter :as us]
   [oskarkv.utils.general-bb-unfriendly :as g]
   [oskarkv.utils.threading :refer :all]))

(impl/define-ordinal-functions)

(impl/defprivatedef def- `def)

(impl/defprivatedef defmacro- `defmacro)

(b/defalias invert-map set/map-invert)

(b/defalias postwalk walk/postwalk)

(b/defalias prewalk walk/prewalk)

(defn pcomp
  "Like `clojure.core/comp`, but if an argument is a vector with `ifn?`
   elements, then applies `juxt` on the collection, and applies the next
   (left) function to the result. Example: ((pcomp + [inc dec]) 2) is
   essentially (+ (inc 2) (dec 2)). A vector can, of course, be used as
   a function itself, so if you want to use a vector (that has `ifn?`
   elements) as a function you can wrap it in another vector."
  ([] identity)
  ([f] (if (and (vector? f) (every? ifn? f))
         (apply juxt f)
         f))
  ([f g & hs]
   (let [pcomp2 (fn [f g]
                  (let [v (and (vector? g) (every? ifn? g))
                        g (if v (apply juxt g) g)
                        f (if v #(apply f %) f)]
                    (fn
                      ([] (f (g)))
                      ([x] (f (g x)))
                      ([x y] (f (g x y)))
                      ([x y z] (f (g x y z)))
                      ([x y z & args] (f (apply g x y z args))))))]
     (reduce pcomp2 (pcomp f) (list* g hs)))))

(defn scalar?
  "The complement of `coll?`."
  [x]
  (not (coll? x)))

(defn array?
  "Returns `true` if `x` is a Java array."
  [x]
  (.isArray (class x)))

(defn empty*
  "Like `clojure.core/empty`, but returns a vector instead of a list for
   MapEntries."
  [coll]
  (if (instance? clojure.lang.MapEntry coll) [] (empty coll)))

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

(defn fmap
  "Applies `f` to the vals in `m` and returns a map. If `colls` are
   provided, applies `f` to the vals in `m` and the items from `colls` in
   parallel, as with `clojure.core/map`, but keep in mind that the order
   of the map elements is unreliable."
  ([f m] (update-vals m f))
  ([f m & colls] (zipmap (keys m) (apply map f (vals m) colls))))

(defn kmap
  "Applies `f` to the keys in `m` and returns a map. If `colls` are
   provided,applies `f` to the keys in `m` and the items from `colls` in
   parallel, as with `clojure.core/map`, but keep in mind that the order
   of the map elements is unreliable."
  ([f m] (update-keys m f))
  ([f m & colls] (zipmap (apply map f (keys m) colls) (vals m))))

(defn keeps
  "Like `keep`, but can take more than one coll, similar to `map`."
  ([f] (keep f))
  ([f coll] (keep f coll))
  ([f coll & colls] (apply sequence (comp (map f) (remove nil?)) coll colls)))

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

(let [t (fn [f]
          #(try (f %)
             (catch #?(:clj Exception :cljs js/Error) e %)))]
  (defn postwalk*
    "Like `clojure.walk/postwalk` but when `f` throws an error, `f` instead
     acts like `identity`."
    [f form]
    (walk/postwalk (t f) form))
  (defn prewalk*
    "Like `clojure.walk/prewalk` but when `f` throws an error, `f` instead
     acts like `identity`."
    [f form]
    (walk/prewalk (t f) form)))

(let [reducer (fn [walker f state tree]
                (reduce
                 (fn [[state replacement] node]
                   (let [[state* node*] (walker f state node)]
                     [state* (conj replacement node*)]))
                 [state (empty* tree)]
                 tree))]
  (defn postwalk-with-state
    "Like `clojure.walk/postwalk` except f takes two args, a state and a tree,
     and should output a new state and a new tree. As the tree is
     walked, each time f is called the last state is passed to it. `state`
     is the initial state."
    [f state tree]
    (if (coll? tree)
      (let [[s t] (reducer postwalk-with-state f state tree)]
        (f s (if (seq? t) (reverse t) t)))
      (f state tree)))
  (defn prewalk-with-state
    "Like `clojure.walk/prewalk` except `f` takes two args, a state and a tree,
     and should output a new state and a new tree. As the tree is
     walked, each time f is called the last state is passed to it. `state`
     is the initial state."
    [f state tree]
    (let [[new-state new-tree] (f state tree)]
      (if (coll? new-tree)
        (reducer prewalk-with-state f new-state new-tree)
        [new-state new-tree]))))

(defn removev
  "Returns a vector of the items in coll for which (pred item) returns
   logical false. pred must be free of side-effects."
  [pred coll]
  (filterv (complement pred) coll))

(defn maps
  "Like mapv, but returns a set."
  ([f coll]
   (into #{} (map f) coll))
  ([f c1 c2]
   (into #{} (map f c1 c2)))
  ([f c1 c2 & more]
   (into #{} (apply map f c1 c2 more))))

(defn filters
  "Returns a set of the items in coll for which (pred item) returns
   logical true. pred must be free of side-effects."
  ([pred coll]
   (into #{} (filter pred) coll)))

(defn removes
  "Returns a set of the items in coll for which (pred item) returns
   logical false. pred must be free of side-effects."
  ([pred coll]
   (into #{} (remove pred) coll)))

(defn mapcatv
  "Like mapcat, but returns a vector."
  ([f coll]
   (into [] (mapcat f) coll))
  ([f coll & more]
   (apply g/into* [] (mapcat f) coll more)))

(defn mapcats
  "Like mapcat, but returns a set."
  ([f coll]
   (into #{} (mapcat f) coll))
  ([f coll & more]
   (apply g/into* #{} (mapcat f) coll more)))

(defn zip
  "Returns a lazy sequence of vectors, where the i:th vector contains the
   i:th elements of the arguments, in the order the arguments were
   given."
  ([coll1 coll2] (map vector coll1 coll2))
  ([coll1 coll2 & more]
   (apply map vector coll1 coll2 more)))

(defn zipv
  "Returns a vector of vectors, where the i:th vector contains the
   i:th elements of the arguments, in the order the arguments were
   given."
  ([coll1 coll2] (mapv vector coll1 coll2))
  ([coll1 coll2 & more]
   (apply mapv vector coll1 coll2 more)))

(defn zips
  "Returns a set of vectors, where the i:th vector contains the
   i:th elements of the arguments, in the order the arguments were
   given."
  ([coll1 coll2] (maps vector coll1 coll2))
  ([coll1 coll2 & more]
   (apply maps vector coll1 coll2 more)))

(defn keysv
  "Returns a vector of the keys of m."
  [m]
  (into [] (map firstv) m))

(defn valsv
  "Returns a vector of the values of m."
  [m]
  (into [] (map secondv) m))

(defn keyss
  "Returns a set of the keys of m."
  [m]
  (into #{} (map firstv) m))

(defn valss
  "Returns a set of the values of m."
  [m]
  (into #{} (map secondv) m))

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
      (if (and (next s) (pred a b))
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
  keysv
  mapcatv
  mapv
  removev
  valsv
  zipv]
 butlast
 concat
 cons
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
 keys
 map
 map-indexed
 mapcat
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
 vals
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

(defn splitr
  "Like `clojure.string.split` but splits from the right."
  ([s re]
   (str/split s re))
  ([s re limit]
   (if (<= limit 0)
     (str/split s re)
     (let [joins (re-seq re s)
           splits (str/split s re)
           n (count splits)
           limit (min limit n)
           replace-count (- (count splits) limit)]
       (if (== limit n)
         (str/split s re)
         (into [(interleave-str (take (inc replace-count) splits)
                                (concat (take replace-count joins) [""]))]
               (take-last (dec limit) splits)))))))

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
  ([m ks v] (assoc-in m ks v))
  ([m ks v & kvs]
   (cond->$ (assoc-in m ks v)
     kvs (apply assoc-ins $ kvs))))

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
  (apply apply vector args))

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

(defn first-el-index
  "Returns the index of the first `x` in `coll`."
  [x coll]
  (first (keep-indexed (fn [i val] (when (= x val) i)) coll)))

(defn random-elements
  "Returns `ratio` of the elements in `elements`, randomly."
  [ratio elements]
  (takev (math/round (* ratio (count elements)))
         (shuffle elements)))

(defn infinite-shuffle
  "Returns the concatenation of (shuffle `coll`) and (infinite-shuffle
   `coll`)."
  [coll]
  (lazy-cat (shuffle coll) (infinite-shuffle coll)))

(defn infinite-random-seq
  "Returns a lazy seq of elements taken randomly from `coll`."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (repeatedly #(nth v (rand-int n)))))

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

(defn conjv
  "Like `conj`, but turns the collection into a vector first."
  ([] [])
  ([coll & args]
   (into (vec coll) args)))

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
  (s/transform (s/walker seq?) vec form))

(defn vectorize-all
  "Turns all collections in `form` into vectors."
  [form]
  (s/transform (s/walker coll?) vec form))

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
  [m rmap]
  (zipmap (replace rmap (keys m)) (vals m)))

(defn select-random-keys
  "Returns a map with `num` mappings from `m`, selected randomly."
  [m num]
  (select-keys m (take num (shuffle (keys m)))))

(defn keys-of-val
  "Returns a set of the keys in `m` that maps to `val`."
  [m val]
  (into #{} (comp (filter #(= (% 1) val)) (map first)) m))

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
                                 (b/ignore-exception (pred x)))
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
  (letfn [(pred* [x] (b/ignore-exception (pred x)))
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
  (letfn [(pred* [x] (b/ignore-exception (pred x)))]
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
  (letfn [(pred* [x] (b/ignore-exception (pred x)))]
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

(defn current-time-ms
  "Returns the current time in ms."
  []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

(defn unqualify-all
  "Returns `tree` but where every qualified symbol or keyword has been
   unqualified."
  [tree]
  (s/transform (us/codewalker* (some-fn qualified-symbol? qualified-keyword?))
               #((if (symbol? %) symbol keyword) (name %))
               tree))

(defmacro unqualify-syms
  "Returns `code` but where any occurrence of the symbols in `syms` has
   been unqualified."
  {:style/indent 1}
  [syms code]
  (impl/unqualify-syms* syms code))

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

(defmacro! print-intermediate-results [& body]
  (letfn [(printer [real-exp original-exp]
            `(let ~[g!r real-exp]
               (println '~original-exp " = "
                        (if (lazy-seq? ~g!r)
                          (apply list (take 20 ~g!r))
                          ~g!r))
               ~g!r))
          (print-sym [sym]
            (if (and (symbol? sym) (not (resolve sym)))
              (printer sym sym)
              sym))
          (walk-expr [exp]
            (cond (vector? exp) (mapv walk-expr exp)
                  (map? exp) (into {} (map walk-expr exp))
                  (sequential? exp)
                  (cond (some #{'recur} (flatten exp))
                        (map walk-expr exp)
                        (= 'case (first exp))
                        exp
                        :else (let [i (map walk-expr exp)]
                                (printer
                                 (cons (first i) (map print-sym (rest i)))
                                 exp)))
                  :else exp))]
    `(binding [*print-length* 5]
       (do ~@(map walk-expr body)))))

(defmacro! with-indented-printlns
  "Let binds `println` to a function that is like regular `println`, but
   prints with an indent. The indent increases each time a function f is
   called and decreases when f returns, iff f was created in the code
   wrapped by a call to this macro such that it eventually (after
   macroexpansion) turns into an `fn*` form."
  {:style/indent 0}
  [& code]
  (letfn [(handle-fn* [[_ & clauses]]
            (list* 'fn* (map (fn [[args & body]]
                               (cons args `((vswap! ~g!depth inc)
                                            (let [r# (do ~@body)]
                                              (vswap! ~g!depth dec)
                                              r#))))
                             clauses)))]
    (unqualify-syms [args]
      `(let [~g!depth (volatile! -1)
             ~'println (fn [& args]
                         (println
                          (apply str (apply str (repeat @~g!depth "  "))
                                 (interpose " " args))))]
         ~@(walk/postwalk
            (fn [form]
              (if (and (seq? form) (= 'fn* (first form)))
                (handle-fn* (if (vector? (second form))
                              (list 'fn* (rest form))
                              form))
                form))
            ;; Expand macros first, so we only have to deal with fn* forms.
            (walk/prewalk macroexpand code))))))

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
     result#))
