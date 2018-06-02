(ns clara-specs.rules-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [schema.core :as sc]
            [clojure.spec.test.alpha :as stest]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clara-specs.rules :as r]))

(def sexpr '(= ?a 1))

(def constraints ['(= ?a 1) '(= ?b 2)])


(def req-fact-condition {:type        java.util.Date
                         :constraints constraints})

(def opt-fact-condition {:type                 java.util.Date
                         :constraints          constraints
                         :original-constraints ['(= 1 ?a) '(= 2 ?b)]
                         :args                 nil
                         :fact-binding         :?my-fact})

(def req-accumulator-condition {:accumulator '(acc/all)
                                :from        req-fact-condition})

(def opt-accumulator-condition {:accumulator    '(acc/all)
                                :from           opt-fact-condition
                                :result-binding :?my-result})

(def test-condition {:constraints constraints})

(def condition-fixtures
  [req-fact-condition opt-fact-condition
   req-accumulator-condition opt-accumulator-condition
   test-condition])

(def leaf-condition-generator
  (gen/elements condition-fixtures))





(deftest s-expr-test
  (is (= (s/conform ::r/s-expr sexpr)
         (sc/validate r/SExpr sexpr))))

(deftest fact-condition-test
  (testing "Required fields"
    (is (= (s/conform ::r/fact-condition req-fact-condition)
           (sc/validate r/FactCondition req-fact-condition))))
  (testing "Optional fields"
    (is (= (s/conform ::r/fact-condition opt-fact-condition)
           (sc/validate r/FactCondition opt-fact-condition)))))

(deftest accumulator-condition-test
  (testing "Required fields"
    (is (= (s/conform ::r/accumulator-condition req-accumulator-condition)
           (sc/validate r/AccumulatorCondition req-accumulator-condition))))
  (testing "Optional fields"
    (is (= (s/conform ::r/accumulator-condition opt-accumulator-condition)
           (sc/validate r/AccumulatorCondition opt-accumulator-condition)))))

(deftest test-condition-test
  (is (= (s/conform ::r/test-condition test-condition)
         (sc/validate r/TestCondition test-condition))))


(defspec leaf-condition-test 10
  (prop/for-all [leaf-condition leaf-condition-generator]
    (= (val (s/conform ::r/leaf-condition leaf-condition))
       (sc/validate r/LeafCondition leaf-condition))))





(run-tests)
