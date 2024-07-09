(ns oskarkv.utils.threading-test
  (:require [oskarkv.utils.threading :refer :all]
            [clojure.test :refer :all]))

(deftest thread-case
  (is (= 9 (+>$ 10 (case $ 2 inc 10 dec str))))
  (is (= 9 (+>$ 10 (case $ $ inc 10 dec str))))
  (is (= "10" (+>$ 10 (case 1 2 inc 10 dec str)))))

(deftest thread-cond
  (is (= 9 (+>$ 10 (cond false inc true dec :else str))))
  (is (= 9 (+>$ 10 (cond false inc :else dec))))
  (is (= 11 (+>$ 10 (cond (pos? $) inc :else dec)))))

(deftest thread-try
  (is (= 11 (+>$ 10 (try inc (catch Exception e dec)))))
  (is (= 9 (+>$ 10 (try inc (/ 0) (catch Exception e dec)))))
  ;; Finally is not threaded through.
  (is (= 9 (+>$ 10 (try inc (/ 0) (catch Exception e dec) (finally inc))))))

(deftest thread-if-lets
  (is (= 12 (+>$ 10 (if-lets [x 2 y 3] (+ x) (- x)))))
  (is (= 8 (+>$ 10 (if-lets [x 2 y false] (+ x) (- x)))))
  (is (= 25 (+>$ 10 (if-lets [x 2 y (+ $ 3)] (+ x y)))))
  (is (= 10 (+>$ 10 (if-lets [x nil] inc)))))

(deftest thread-when-some
  (is (= 15 (+>$ 10 (when-some [x 2 y 3] (+ x) (+ y)))))
  (is (= 13 (+>$ 10 (when-some [x 2 y false] (+ x) inc))))
  (is (= 10 (+>$ 10 (when-some [x 2 y nil] (+ x) inc)))))

(deftest thread-do
  (is (= 12 (+>$ 10 (do inc inc)))))

(deftest thread-let
  (is (= 15 (+>$ 10 (let [x 4] (+ x) inc))))
  (is (= 22 (+>$ 10 (let [x (inc $)] (+ x) inc)))))

(deftest thread-$
  (is (= 11 (+>$ 10 inc $))))

(deftest thread-nested
  (is (= 17 (+>$ 10 (let [x 4] inc (let [y 2] (+ x y)))))))
