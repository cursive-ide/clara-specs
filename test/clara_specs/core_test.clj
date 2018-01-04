(ns clara-specs.core-test
  (:require [clojure.test :refer :all]
            [clara-specs.core :as clara]
            [clojure.spec.alpha :as s]))


(deftest defrule-examples
  (is (= (s/conform ::clara/defrule-form
                    (rest '(defrule is-important
                                    "Find important support requests."
                                    [SupportRequest (= :high level)]
                                    =>
                                    (println "High support requested!"))))
         '{:name is-important
           :docstring "Find important support requests."
           :lhs [[:condition {:type SupportRequest, :constraints [(= :high level)]}]]
           :implies =>
           :rhs [(println "High support requested!")]}))

  (is (= (s/conform ::clara/defrule-form
                    (rest '(defrule notify-client-rep
                                    "Find the client representative and send a notification of a support request."
                                    [SupportRequest (= ?client client)]
                                    [ClientRepresentative (= ?client client) (= ?name name)] ; Join via the ?client binding.
                                    =>
                                    (println "Notify" ?name "that" ?client "has a new support request!"))))
         '{:name notify-client-rep
           :docstring "Find the client representative and send a notification of a support request.",
           :lhs [[:condition {:type SupportRequest, :constraints [(= ?client client)]}]
                 [:condition {:type ClientRepresentative :constraints [(= ?client client) (= ?name name)]}]]
           :implies =>
           :rhs [(println "Notify" ?name "that" ?client "has a new support request!")]}))

  (is (= (s/conform ::clara/defrule-form
                    (rest '(defrule get-windspeeds-from-location
                                    [?temp <- [Temperature (= ?location location)]]
                                    [?wind <- (acc/all) :from [WindSpeed (= ?location location)]]
                                    =>
                                    ())))                   ;; do something
         '{:name get-windspeeds-from-location,
           :lhs [[:fact-binding-expr
                  {:fact-binding {:binding-var ?temp, :sep <-}
                   :condition {:type Temperature :constraints [(= ?location location)]}}]
                 [:accumulator-expr
                  {:result-binding {:binding-var ?wind
                                    :<- <-}
                   :accumulator (acc/all)
                   :from :from
                   :condition {:type WindSpeed :constraints [(= ?location location)]}}]],
           :implies =>
           :rhs [()]})))


(deftest defquery-examples
  (is (= (s/conform ::clara/defquery-form
                    (rest '(defquery get-promotions
                                     "Query to find promotions for the purchase."
                                     [:?type]
                                     [?promotion <- [Promotion (= ?type type)]])))
         '{:name      get-promotions
           :docstring "Query to find promotions for the purchase."
           :args      [:?type]
           :lhs       [[:fact-binding-expr
                        {:fact-binding {:binding-var ?promotion
                                        :sep         <-}
                         :condition    {:type Promotion :constraints [(= ?type type)]}}]]})))

;; Condition example
(deftest condition-spec
  (is (= (s/conform ::clara/condition '[Person (= first-name "Alice") (= ?last-name last-name)])
         '{:type   Person
           :constraints [(= first-name "Alice")
                         (= ?last-name last-name)]})))

(deftest destructuring
  (is (= (s/conform ::clara/destructured
                    '[{{city :city state :state} :address}])
         '{:form [:map {{city  :city
                         state :state}
                        :address}]}))
  (is (= (s/conform ::clara/destructured
                    '[person])
         '{:form [:sym person]})))

(deftest boolean-expressions
  (is (= (s/conform ::clara/boolean-expr
                    '[:or [Customer (= status :vip)]
                      [Promotion (= type :discount-month)]])
         '{:op  :or
           :conditions [[:condition
                         {:type   Customer
                          :constraints [(= status :vip)]}]
                        [:condition
                         {:type   Promotion
                          :constraints [(= type :discount-month)]}]]}))

  ;; Under the current scheme we expect this to be valid. Determining a valid fact-type requires access to the fact-type function
  ;; the user provides to `defsession`. The the default is just Clojure's `type` fn and works well for defrecords. Most users
  ;; probably use this
  (is (s/valid? ::clara/boolean-expr
                '[:and
                  [false (= type :discount-month)]
                  [:not [:and [Promotion (= type :discount-month)]
                              [Promotion (= type :discount-month)]]]])))


(deftest tests
  (is (= (s/conform ::clara/test-expr
                    '[:test (> ?age1 ?age2)])
         '{:expr [(> ?age1 ?age2)]
           :test :test})))

(deftest accumulators
  (is (= (s/conform ::clara/accumulator-expr
                    '[?wind <- (acc/all) :from [WindSpeed (= ?location location)]])
         '{:result-binding {:binding-var ?wind
                            :<-             <-}
           :accumulator    (acc/all)
           :from           :from
           :condition      {:type        WindSpeed
                            :constraints [(= ?location location)]}})))
