(ns oskarkv.utils
  (:require
   [better-cond.core :as bc]
   [clojure.tools.macro]
   [com.rpl.specter]
   [oskarkv.utils.base :as b]
   [oskarkv.utils.general]
   [oskarkv.utils.math]
   [oskarkv.utils.printing]
   [oskarkv.utils.specter]
   [oskarkv.utils.threading]))

(b/defalias cond+ bc/cond)
(b/defalias defnc bc/defnc)
(b/defalias defnc- bc/defnc-)
(b/alias-everything clojure.tools.macro)
(b/alias-everything oskarkv.utils.base)
(b/alias-everything oskarkv.utils.general)
(b/alias-everything oskarkv.utils.math)
(b/alias-everything oskarkv.utils.printing)
(b/alias-everything oskarkv.utils.specter)
(b/alias-everything oskarkv.utils.threading)
