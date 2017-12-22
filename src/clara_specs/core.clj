(ns clara-specs.core
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as core]
            [clojure.string :as str]))

(s/def ::variable-name (s/and simple-symbol? #(str/starts-with? % "?")))

(s/def ::destructured (s/and vector?
                             (s/cat :form ::core/binding-form)))

(s/def ::test (s/and vector?
                     (s/cat :test #{:test}
                            :expr (s/* any?))))

(s/def ::accumulator (s/and vector?
                            (s/cat :binding-var ::variable-name
                                   :sep #{'<-}
                                   :fact-type any?          ; TODO
                                   :_ #{:from}
                                   :fact-expr ::fact-expr)))

(s/def ::boolean-expr (s/and vector?
                             (s/cat :condition #{:and :or :not}
                                    :expression (s/+ (s/or :boolean-expr ::boolean-expr
                                                           :fact-expr ::fact-expr))))) ; TODO what do these look like?

(s/def ::fact-expr (s/and vector?
                          (s/cat :binding (s/? (s/cat :binding-var ::variable-name
                                                      :sep #{'<-}))
                                 :fact-type any?            ; TODO
                                 :destructured-fact (s/? ::destructured)
                                 :expressions (s/* any?))))

(s/def ::condition (s/or :accumulator ::accumulator
                         :fact-expr ::fact-expr
                         :boolean-expr ::boolean-expr
                         :test ::test))

(s/def ::defquery-form
  (s/cat :name simple-symbol?
         :docstring (s/? string?)
         :args (s/and vector?
                      (s/* any?))                           ; TODO not sure what args can be
         :conditions (s/+ ::condition)))                    ; TODO looks like multiple conditions inside single vector in the doc

(s/def ::defrule-form
  (s/cat :name simple-symbol?
         :docstring (s/? string?)
         :props (s/? map?)                                  ; TODO don't know what props look like
         :conditions (s/+ ::condition)
         :implies #{'=>}
         :rhs any?))                                        ; TODO can there be more than one expression here?

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
            [?promotion <- Promotion (= ?type type)]))
