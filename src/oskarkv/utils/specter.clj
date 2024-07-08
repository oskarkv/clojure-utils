(ns oskarkv.utils.specter
  (:require
   [com.rpl.specter :as s]
   [oskarkv.utils.base :as b]))

(defmacro recursive-search-path [args must-arg result-path]
    `(s/recursive-path ~args p#
       (s/cond-path [#((every-pred ifn? coll?) %) (s/must ~must-arg)]
                    (s/multi-path ~result-path [s/ALL p#])
                    coll? [s/ALL p#]
                    s/STOP)))

(defn walker*
  "Like `com.rpl.specter/walker`, but ignores exceptions in the given
   function."
  [f]
  (s/walker #(b/ignore-exception (f %))))

(defn codewalker*
  "Like `com.rpl.specter/codewalker`, but ignores exceptions in the given
   function."
  [f]
  (s/codewalker #(b/ignore-exception (f %))))

(def find-vals
  "A specter navigator to recursively find the values of the given key in
   a nested data structure. The key can be a key into any collection
   that works as a function."
  (recursive-search-path [k] k k))

(def structs-with
  "A specter navigator to recursively find structures (any coll that works
   as a function) that contain the given key in a nested data
   structure."
  (recursive-search-path [k] k s/STAY))
