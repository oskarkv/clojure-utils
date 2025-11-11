(ns oskarkv.utils.specter-macros
  (:require
   [com.rpl.specter :as s]))

(defmacro recursive-search-path [args must-arg result-path]
    `(s/recursive-path ~args p#
       (s/cond-path [#((every-pred ifn? coll?) %) (s/must ~must-arg)]
                    (s/multi-path ~result-path [s/ALL p#])
                    coll? [s/ALL p#]
                    s/STOP)))
