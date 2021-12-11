(ns oskarkv.utils-test
  (:require [oskarkv.utils :as u])
  (:use clojure.test))

(deftest ordinal-functions
  (is (= (u/third (range 10)) 2))
  (is (= (u/fifthv (vec (range 10))) 4)))

(deftest dissoc-in
  (is (= (u/dissoc-in {:a {:b 1}} [:a :b]) {:a {}}))
  (is (= (u/dissoc-in {:a {:b 1 :c 2} :b [1 2]} [:a :b] [:b 0])
         {:a {:c 2} :b [nil 2]}))
  (is (= (u/dissoc-in {} [:a]) {})))

(let [sum-even? (fn [& args]
                  (even? (reduce + args)))
      sum-pos? (fn [& args]
                 (pos? (reduce + args)))
      union (u/pred-union sum-even? sum-pos?)
      intersection (u/pred-intersection sum-even? sum-pos?)]
  (deftest pred-union-and-intersection
    (is (union -2 -5 -5))
    (is (not (union -2 -5 -4)))
    (is (intersection 1 2 3))
    (is (not (intersection 2 2 3)))))

(letfn [(two-more [n]
          [(* 2 n) (* 3 n)])]
  (deftest bfs
    (is (= (set (take 6 (u/bfs 1 two-more))) #{1 2 3 4 6 9}))
    (is (= (u/bfs 3 (constantly nil)) '(3))))

  (deftest dfs
    (is (= (take 6 (u/dfs 1 two-more)) '(1 2 4 8 16 32)))
    (is (= (u/dfs 3 (constantly nil)) '(3)))))

(deftest merge-nested
  (is (= (u/merge-nested +) nil))
  (is (= (u/merge-nested + {:a 1 :b -1}) {:a 1 :b -1}))
  (is (= (u/merge-nested + {:a {:b 1} :c 1} {:a {:b 1}} {:c 1} {:d 1})
         {:a {:b 2} :c 2 :d 1}))
  (is (= (u/merge-nested vector {:a 1 :b 2} {:a 2})
         {:a [1 2] :b 2})))

(deftest merge-nested-when
  (is (= (u/merge-nested-when pos? +) nil))
  (is (= (u/merge-nested-when pos? + {:a 1 :b -1}) {:a 1 :b -1}))
  (is (= (u/merge-nested-when pos? + {:a -1 :b 1} {:a -1 :b 1})
         {:a -1 :b 2}))
  (is (= (u/merge-nested-when pos? + {:a 3 :b 1} {:a -1 :b 1})
         {:a 3 :b 2}))
  (is (= (u/merge-nested-when pos? + {:a [1 -1 1]} {:a [2 2]})
         {:a [3 2 1]}))
  (is (= (u/merge-nested-when pos? + {:a '(1 -1 1)} {:a [2 2]})
         {:a [3 2 1]}))
  (is (= (u/merge-nested-when pos? vector {:a '(1 -1 1)} {:a [2 2]})
         {:a [[1 2] 2 1]})))

(deftest combine
  (is (= (u/combine +) {}))
  (is (= (u/combine + {:a 1 :b -1}) {:a 1 :b -1}))
  (is (= (u/combine + {:a {:b 1} :c 1} {:a {:b 1}} {:c 1} {:d 1})
         {:a {:b 2} :c 2 :d 1}))
  (is (= (u/combine vector {:a 1 :b 2} {:a 2})
         {:a [1 2] :b [2]})))

(deftest combine-when
  (is (= (u/combine-when pos? +) {}))
  (is (= (u/combine-when pos? + {:a 1 :b -1}) {:a 1 :b -1}))
  (is (= (u/combine-when pos? + {:a -1 :b 1} {:a -1 :b 1})
         {:a -1 :b 2}))
  (is (= (u/combine-when pos? + {:a 3 :b 1} {:a -1 :b 1})
         {:a 3 :b 2}))
  (is (= (u/combine-when pos? + {:a [1 -1 1]} {:a [2 2]})
         {:a [3 2 1]}))
  (is (= (u/combine-when pos? + {:a '(1 -1 1)} {:a [2 2]})
         {:a [3 2 1]}))
  (is (= (u/combine-when pos? vector {:a '(1 -1 1)} {:a [2 2]})
         {:a [[1 2] [2] [1]]})))

(deftest v-and-str-fns
  (is (= (u/butlastv '(1 2 3)) [1 2]))
  (is (= (u/butlast-str '(1 2 3)) "12"))
  (is (= (u/rangev 3) [0 1 2]))
  (is (= (u/range-str 3) "012"))
  (is (nil? (find-var 'oskarkv.utils/filterv))))

(deftest threading-macros
  (is (= (u/cond->$ 1
           (pos? $) -
           true (vector :a)
           false (vector :b)
           true (vector :c $ :d $))
         [:c [-1 :a] :d [-1 :a]]))
  (is (= (u/some->>$ 1
           (vector 2))
         [2 1]))
  (is (= (u/some->$ nil
           (+ 1))
         nil)))
