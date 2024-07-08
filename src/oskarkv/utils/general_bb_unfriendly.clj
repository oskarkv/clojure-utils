(ns oskarkv.utils.general-bb-unfriendly)

(defn transduce*
  "Like transduce, but can take multiple collections (and the xform are
   supposed to accept multiple values)."
  ([xform f coll] (transduce xform f (f) coll))
  ([xform f init coll]
   (let [f (xform f)
         ret (if (instance? clojure.lang.IReduceInit coll)
               (.reduce ^clojure.lang.IReduceInit coll f init)
               (clojure.core.protocols/coll-reduce coll f init))]
     (f ret)))
  ([xform f init coll & more]
   (let [iter (clojure.lang.TransformerIterator/createMulti
               xform
               (map #(clojure.lang.RT/iter %) (cons coll more)))]
     (loop [ret init]
       (if (.hasNext iter)
         (let [ret (f ret (.next iter))]
           (if (reduced? ret)
             @ret
             (recur ret)))
         ret)))))

(defn into*
  "Like into, but can take multiple collections (and the xform are
   supposed to accept multiple values)."
  ([] [])
  ([to] to)
  ([to from]
   (if (instance? clojure.lang.IEditableCollection to)
     (with-meta (persistent! (reduce conj! (transient to) from)) (meta to))
     (reduce conj to from)))
  ([to xform from]
   (if (instance? clojure.lang.IEditableCollection to)
     (let [tm (meta to)
           rf (fn
                ([coll] (-> (persistent! coll) (with-meta tm)))
                ([coll v] (conj! coll v)))]
       (transduce xform rf (transient to) from))
     (transduce xform conj to from)))
  ([to xform from & more]
   (let [iter (clojure.lang.TransformerIterator/createMulti
               xform
               (map #(clojure.lang.RT/iter %) (cons from more)))]
     (loop [trans (transient to)]
       (if (.hasNext iter)
         (recur (conj! trans (.next iter)))
         (persistent! trans))))))
