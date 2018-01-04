(ns clara-specs.core
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as core]
            [clojure.string :as str]))

(s/def ::variable-name (s/and simple-symbol? #(str/starts-with? % "?")))

(s/def ::destructured (s/and vector?
                             (s/cat :form ::core/binding-form)))


(s/def ::test-expr (s/and vector?
                       (s/cat :test #{:test}
                              :expr (s/* any?))))

(s/def ::ops #{:and 'and :or 'or :not 'not :exists 'exists})

(s/def ::constraint list?)

(s/def ::fact-type (s/and #(not (s/valid? ::variable-name %))
                          #(not (s/valid? ::ops %))
                          #(not (s/valid? ::constraint %))))

(s/def ::condition (s/and vector?
                          (s/cat :type ::fact-type
                                 :constraints (s/* ::constraint))))

(s/def ::accumulator-fn list?)

(s/def ::result-binding (s/cat :binding-var ::variable-name
                               :<- #{'<-}))

(s/def ::accumulator-expr (s/cat :result-binding (s/? ::result-binding)
                                 :accumulator ::accumulator-fn
                                 :from #{:from}
                                 :condition ::condition))

(s/def ::boolean-expr (s/and vector?
                             (s/cat :op ::ops
                                    :conditions (s/+ (s/or :boolean-expr ::boolean-expr
                                                           :condition ::condition)))))

(s/def ::fact-binding-expr (s/and vector?
                            (s/cat :fact-binding (s/? (s/cat :binding-var ::variable-name
                                                             :sep #{'<-}))
                                   :condition ::condition)))

(s/def ::expression (s/or :accumulator-expr ::accumulator-expr
                          :boolean-expr ::boolean-expr
                          :test-expr ::test-expr
                          :fact-binding-expr ::fact-binding-expr
                          :condition ::condition))

(s/def ::lhs (s/* ::expression))

(s/def ::query-arg (s/and keyword?
                          #(str/starts-with? (name %) "?")))


(s/def ::defquery-form
  (s/cat :name simple-symbol?
         :docstring (s/? string?)
         :args (s/and vector?
                      (s/* ::query-arg))
         ;:lhs ::lhs                                               ; TODO. Not sure why but these don't seem equivalent...
         :lhs (s/* ::expression)))

(s/def ::defrule-form
  (s/cat :name simple-symbol?
         :docstring (s/? string?)
         :props (s/? map?)                                  ; TODO don't know what props look like
         ;:lhs ::lhs
         :lhs (s/* ::expression)
         :implies #{'=>}
         :rhs (s/+ any?)))

(comment
  (defrule is-important
           "Find important support requests."
           [SupportRequest (= :high level)]
           =>
           (println "High support requested!"))

  (defrule notify-client-rep
           "Find the client representative and send a notification of a support request."
           [SupportRequest (= ?client client)]
           [ClientRepresentative (= ?client client) (= ?name name)] ; Join via the ?client binding.
           =>
           (println "Notify" ?name "that" ?client "has a new support request!"))

  (defquery get-promotions
            "Query to find promotions for the purchase."
            [:?type]
            [?promotion <- Promotion (= ?type type)])

  '(defrule large-job-delay
    "Large jobs must have at least a two week delay,
     unless it is a top-tier client"
    [WorkOrder (= ?clientid clientid)
               (= scale :big)
               (< (days-between requestdate duedate) 14)]

    [:not [ClientTier
           (= ?clientid id) ; Join to the above client ID.
           (= tier :top)]]
    =>
    (insert! (->ValidationError
              :timeframe
              "Insufficient time prior to due date of the large order."))))