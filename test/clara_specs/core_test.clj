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
           :rhs [()]}))

  (is (= (s/conform ::clara/defrule-form
                    (rest '(defrule appreciation-day-and-valued-or-new
                                    [Order (= ?order-id order-id) (= ?customer-id customer-id)]
                                    [:or
                                     [:and
                                      [CurrentDay (= ?day day)]
                                      [CustomerAppreciationDay (= ?day day)]]
                                     [:and
                                      [NewCustomer (= ?customer-id customer-id)]
                                      [ValuedCustomer (= ?customer-id customer-id)]]]
                                    =>
                                    (insert! (->Discount ?order-id 20)))))

        '{:name appreciation-day-and-valued-or-new,
          :lhs
                [[:condition
                  {:type Order,
                   :constraints
                         [(= ?order-id order-id) (= ?customer-id customer-id)]}]
                 [:boolean-expr
                  {:op :or,
                   :conditions
                       [[:boolean-expr
                         {:op :and,
                          :conditions
                              [[:condition
                                {:type CurrentDay, :constraints [(= ?day day)]}]
                               [:condition
                                {:type CustomerAppreciationDay,
                                 :constraints [(= ?day day)]}]]}]
                        [:boolean-expr
                         {:op :and,
                          :conditions
                              [[:condition
                                {:type NewCustomer,
                                 :constraints [(= ?customer-id customer-id)]}]
                               [:condition
                                {:type ValuedCustomer,
                                 :constraints
                                       [(= ?customer-id customer-id)]}]]}]]}]],
          :implies =>,
          :rhs [(insert! (->Discount ?order-id 20))]}))

  (testing "No lhs/conditions"
    (is (= (s/conform ::clara/defrule-form
                     (rest '(defrule my-rule
                              =>
                              (insert! (->Discount ?order-id 20)))))
           '{:name my-rule,
             :implies =>,
             :rhs [(insert! (->Discount ?order-id 20))]})))

  (testing "Arbitrary fns in constraints"
    (is (= (s/conform ::clara/defrule-form
                      (rest '(defrule my-rule
                                      [Order (> ?order-id order-id) (my-pred ?customer-id customer-id)]
                                      =>
                                      (insert! (->Discount ?order-id 20)))))
           '{:name    my-rule,
             :lhs
                      [[:condition
                        {:type Order,
                         :constraints
                               [(> ?order-id order-id) (my-pred ?customer-id customer-id)]}]],
             :implies =>,
             :rhs     [(insert! (->Discount ?order-id 20))]})))

  (testing "Accumulator with no result binding"
      (is (= (s/conform ::clara/defrule-form
                        (rest '(defrule my-rule
                                        [(acc/sum) :from [Order (= ?customer-id customer-id)]]
                                        =>
                                        (insert! (->Discount ?order-id 20)))))
             '{:name    my-rule,
               :lhs     [[:accumulator-expr
                          {:accumulator (acc/sum),
                           :from        :from,
                           :condition
                                        {:type        Order,
                                         :constraints [(= ?customer-id customer-id)]}}],]
               :implies =>,
               :rhs     [(insert! (->Discount ?order-id 20))]}))))


(deftest invalid-defrule-forms
  (testing "No rule name"
    (is (false? (s/valid? ::clara/defrule-form
                          (rest '(defrule
                                   [Order (= ?order-id order-id) (= ?customer-id customer-id)]
                                   =>
                                   (do)))))))

  (testing "No RHS separator"
    (is (false? (s/valid? ::clara/defrule-form
                          (rest '(defrule my-rule
                                          [Order (= ?order-id order-id) (= ?customer-id customer-id)]
                                          (do)))))))

  (testing "No RHS"
    (is (false? (s/valid? ::clara/defrule-form
                          (rest '(defrule my-rule
                                          [Order (= ?order-id order-id) (= ?customer-id customer-id)]
                                          =>))))))

  (testing "Accumulator with > 1 condition"
    (is (false? (s/valid? ::clara/defrule-form
                          (rest '(defrule my-rule
                                          [fact <- (acc/sum) :from [Order (= ?customer-id customer-id)] [ValuedCustomer (= ?customer-id 42)]]
                                          =>
                                          (do)))))))

  (testing "Condition with only a fact-type not in a vector"
    (is (false? (s/valid? ::clara/defrule-form
                          (rest '(defrule my-rule
                                          Order
                                          =>
                                          (do))))))))


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


  ;; Under the current scheme we expect a fact type of `false` to be valid.
  ;; We can add (not boolean?) (not list?) and others to the spec to prevent this
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
