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
         '{:name      is-important
           :docstring "Find important support requests."
           :lhs       [[:fact-binding-expr {:type SupportRequest, :constraints [(= :high level)]}]]
           :implies   =>
           :rhs       [(println "High support requested!")]}))

  (is (= (s/conform ::clara/defrule-form
                    (rest '(defrule notify-client-rep
                                    "Find the client representative and send a notification of a support request."
                                    [SupportRequest (= ?client client)]
                                    [ClientRepresentative (= ?client client) (= ?name name)] ; Join via the ?client binding.
                                    =>
                                    (println "Notify" ?name "that" ?client "has a new support request!"))))
         '{:name      notify-client-rep
           :docstring "Find the client representative and send a notification of a support request.",
           :lhs       [[:fact-binding-expr {:type SupportRequest, :constraints [(= ?client client)]}]
                       [:fact-binding-expr {:type ClientRepresentative :constraints [(= ?client client) (= ?name name)]}]]
           :implies   =>
           :rhs       [(println "Notify" ?name "that" ?client "has a new support request!")]}))

  (is (= (s/conform ::clara/defrule-form
                    (rest '(defrule get-windspeeds-from-location
                                    [?temp <- Temperature (= ?location location)]
                                    [?wind <- (acc/all) :from [WindSpeed (= ?location location)]]
                                    =>
                                    ())))                   ;; do something
         '{:name    get-windspeeds-from-location,
           :lhs     [[:fact-binding-expr
                      {:fact-binding {:binding-var ?temp, :<- <-}
                       :type         Temperature
                       :constraints  [(= ?location location)]}]
                     [:accumulator-expr
                      {:result-binding {:binding-var ?wind
                                        :<-          <-}
                       :accumulator    (acc/all)
                       :from           :from
                       :condition      {:type WindSpeed :constraints [(= ?location location)]}}]],
           :implies =>
           :rhs     [()]})))


(deftest defquery-examples
  (is (= (s/conform ::clara/defquery-form
                    (rest '(defquery get-promotions
                                     "Query to find promotions for the purchase."
                                     [:?type]
                                     [?promotion <- Promotion (= ?type type)])))
         '{:name      get-promotions
           :docstring "Query to find promotions for the purchase."
           :args      [:?type]
           :lhs       [[:fact-binding-expr
                        {:fact-binding {:binding-var ?promotion
                                        :<-          <-}
                         :type         Promotion
                         :constraints  [(= ?type type)]}]]})))

(deftest expression-variants
  ;; Cases from https://github.com/cursive-ide/clara-specs/issues/4#issuecomment-355577971
  (is (= (s/conform ::clara/expression '[?a <- A (= ?id id)])
         '[:fact-binding-expr {:fact-binding {:<-          <-
                                              :binding-var ?a}
                               :type         A
                               :constraints  [(= ?id id)]}]))
  (is (= (s/conform ::clara/expression '[?b <- B])
         '[:fact-binding-expr {:fact-binding {:<-          <-
                                              :binding-var ?b}
                               :type         B}]))
  (is (= (s/conform ::clara/expression '[C])
         '[:fact-binding-expr {:type C}]))
  (is (= (s/conform ::clara/expression '[D (= ?id id)])
         '[:fact-binding-expr {:type        D
                               :constraints [(= ?id id)]}]))
  (is (= (s/conform ::clara/expression '[E (= ?id id) (< v 10)])
         '[:fact-binding-expr {:type        E
                               :constraints [(= ?id id)
                                             (< v 10)]}]))
  (is (= (s/conform ::clara/expression '[?fs <- (acc/all) :from [F]])
         '[:accumulator-expr {:result-binding {:<-          <-
                                               :binding-var ?fs}
                              :accumulator    (acc/all)
                              :from           :from
                              :condition      {:type F}}]))
  (is (= (s/conform ::clara/expression '[?gs <- (acc/all) :from [G (< v 10)]])
         '[:accumulator-expr {:result-binding {:<-          <-
                                               :binding-var ?gs}
                              :accumulator    (acc/all)
                              :from           :from
                              :condition      {:constraints [(< v 10)]
                                               :type        G}}]))
  (is (= (s/conform ::clara/expression '[:not [H]])
         '[:boolean-expr {:op         :not
                          :conditions [[:condition {:type H}]]}]))
  (is (= (s/conform ::clara/expression '[:not [I (< v 10)]])
         '[:boolean-expr {:op         :not
                          :conditions [[:condition {:constraints [(< v 10)]
                                                    :type        I}]]}]))
  (is (= (s/conform ::clara/expression '[:or [J] [:and [K] [L]]])
         '[:boolean-expr {:op         :or
                          :conditions [[:condition {:type J}]
                                       [:boolean-expr {:op         :and
                                                       :conditions [[:condition {:type K}]
                                                                    [:condition {:type L}]]}]]}]))
  (is (= (s/conform ::clara/expression '[:test (< 2 (count ?gs))])
         '[:test-expr {:expr [(< 2 (count ?gs))]
                       :test :test}]))
  (is (= (s/conform ::clara/expression '[:exists [A]])
         '[:boolean-expr {:op         :exists
                          :conditions [[:condition {:type A}]]}])))

;; Condition example
(deftest condition-spec
  (is (= (s/conform ::clara/condition '[Person (= first-name "Alice") (= ?last-name last-name)])
         '{:type        Person
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
         '{:op         :or
           :conditions [[:condition
                         {:type        Customer
                          :constraints [(= status :vip)]}]
                        [:condition
                         {:type        Promotion
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
                            :<-          <-}
           :accumulator    (acc/all)
           :from           :from
           :condition      {:type        WindSpeed
                            :constraints [(= ?location location)]}})))
