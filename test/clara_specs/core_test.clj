(ns clara-specs.core-test
  (:require [clojure.test :refer :all]
            [clara-specs.core :refer :all :as clara]
            [clojure.spec.alpha :as s]))

(deftest defrule-examples
  (is (= (s/conform ::clara/defrule-form
                    (rest '(defrule is-important
                                    "Find important support requests."
                                    [SupportRequest (= :high level)]
                                    =>
                                    (println "High support requested!"))))
         '{:name       is-important
           :docstring  "Find important support requests."
           :conditions [[:fact-expr
                         {:expressions [(= :high level)]
                          :fact-type   SupportRequest}]]
           :implies    =>
           :rhs        [(println "High support requested!")]}))
  (is (= (s/conform ::clara/defrule-form
                    (rest '(defrule notify-client-rep
                                    "Find the client representative and send a notification of a support request."
                                    [SupportRequest (= ?client client)]
                                    [ClientRepresentative (= ?client client) (= ?name name)] ; Join via the ?client binding.
                                    =>
                                    (println "Notify" ?name "that" ?client "has a new support request!"))))
         '{:name       notify-client-rep
           :docstring  "Find the client representative and send a notification of a support request."
           :conditions [[:fact-expr
                         {:expressions [(= ?client client)]
                          :fact-type   SupportRequest}]
                        [:fact-expr
                         {:expressions [(= ?client client)
                                        (= ?name name)]
                          :fact-type   ClientRepresentative}]]
           :implies    =>
           :rhs        [(println "Notify" ?name "that" ?client "has a new support request!")]}))
  (is (= (s/conform ::clara/defrule-form
                    (rest '(defrule get-windspeeds-from-location
                                    [?temp <- [Temperature (= ?location location)]]
                                    [?wind <- (acc/all) :from [WindSpeed (= ?location location)]]
                                    =>
                                    ())))                   ;; do something

         '{:name       get-windspeeds-from-location
           :conditions [[:fact-expr
                         {:binding   {:binding-var ?temp
                                      :sep         <-}
                          :fact-type [Temperature
                                      (= ?location location)]}]
                        [:accumulator
                         {:binding-var ?wind
                          :fact-expr   {:expressions [(= ?location location)]
                                        :fact-type   WindSpeed}
                          :fact-type   (acc/all)
                          :_           :from
                          :sep         <-}]]
           :implies    =>
           :rhs        [()]})))

(deftest defquery-examples
  (is (= (s/conform ::clara/defquery-form
                    (rest '(defquery get-promotions
                                     "Query to find promotions for the purchase."
                                     [:?type]
                                     [?promotion <- Promotion (= ?type type)])))
         '{:name       get-promotions
           :docstring  "Query to find promotions for the purchase."
           :args       [:?type]
           :conditions [[:fact-expr
                         {:binding     {:binding-var ?promotion
                                        :sep         <-}
                          :expressions [(= ?type type)]
                          :fact-type   Promotion}]]})))

(deftest fact-expression-examples
  (is (= (s/conform ::clara/fact-expr
                    '[?person <- Person (= first-name "Alice") (= ?last-name last-name)])
         '{:binding     {:binding-var ?person
                         :sep         <-}
           :fact-type   Person
           :expressions [(= first-name "Alice")
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
         '{:condition  :or
           :expression [[:fact-expr
                         {:expressions [(= status :vip)]
                          :fact-type   Customer}]
                        [:fact-expr
                         {:expressions [(= type :discount-month)]
                          :fact-type   Promotion}]]})))

(deftest tests
  (is (= (s/conform ::clara/test
                    '[:test (> ?age1 ?age2)])
         '{:expr [(> ?age1 ?age2)]
           :test :test})))

(deftest accumulators
  (is (= (s/conform ::clara/accumulator
                    '[?current-temp <- newest-temp :from [Temperature (= ?location location)]])
         '{:binding-var ?current-temp
           :sep         <-
           :fact-type   newest-temp
           :_           :from
           :fact-expr   {:fact-type   Temperature
                         :expressions [(= ?location location)]}}))
  (is (= (s/conform ::clara/accumulator
                    '[?wind <- (acc/all) :from [WindSpeed (= ?location location)]])
         '{:binding-var ?wind
           :sep         <-
           :fact-type   (acc/all)
           :_           :from
           :fact-expr   {:fact-type   WindSpeed
                         :expressions [(= ?location location)]}})))
