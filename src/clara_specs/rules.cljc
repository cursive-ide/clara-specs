(ns clara-specs.rules
  "Schema definition of Clara data structures using clojure.spec. This includes structures for rules and queries, as well as the schema
   for the underlying Rete network itself. This can be used by tools or other libraries working with rules."
  (:require [schema.core :as sc]
            [clojure.spec.alpha :as s]))


(sc/defn condition-type :- (sc/enum :or :not :and :exists :fact :accumulator :test)
  "Returns the type of node in a LHS condition expression."
  [condition]
  (if (map? condition)                                      ; Leaf nodes are maps, per the schema

    (cond
      (:type condition) :fact
      (:accumulator condition) :accumulator
      :else :test)

    ;; Otherwise the node must a sequential that starts with the boolean operator.
    (first condition)))

(s/fdef condition-type :ret #{:or :and :exists :fact :accumulator :test})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule and query structure schema.

(s/def ::s-expr seq?)

(def SExpr
  (sc/pred seq? "s-expression"))

(comment
  (let [sexpr '(= ?a 1)]
    [(s/conform ::s-expr sexpr)
     (sc/validate SExpr sexpr)]))

(s/def :fact-condition/type any?)
(s/def ::constraints (s/coll-of ::s-expr :kind vector?))
(s/def :fact-condition/original-constraints ::constraints)
(s/def :fact-condition/args any?)
(s/def :fact-condition/fact-binding keyword?)

(s/def ::fact-condition
  (s/keys :req-un [:fact-condition/type ::constraints]
          :opt-un [:fact-condition/original-constraints :fact-condition/fact-binding :fact-condition/args]))

(comment
  (let [req-fact-condition {:type        java.util.Date
                            :constraints ['(= ?a 1) '(= ?b 2)]}
        opt-fact-condition {:type                 java.util.Date
                            :constraints          ['(= ?a 1) '(= ?b 2)]
                            :original-constraints ['(= 1 ?a) '(= 2 ?b)]
                            :args                 nil
                            :fact-binding         :?my-fact}]
    {:req [(s/conform ::fact-condition req-fact-condition)
           (sc/validate FactCondition req-fact-condition)]
     :opt [(s/conform ::fact-condition opt-fact-condition)
           (sc/validate FactCondition opt-fact-condition)]}))

(def FactCondition
  {:type                                   sc/Any           ;(sc/either sc/Keyword (sc/pred symbol?))
   :constraints                            [SExpr]
   ;; Original constraints preserved for tooling in case a transformation was applied to the condition.
   (sc/optional-key :original-constraints) [SExpr]
   (sc/optional-key :fact-binding)         sc/Keyword
   (sc/optional-key :args)                 sc/Any
   })

(s/def :accumulator-condition/accumulator any?)
(s/def :accumulator-condition/from ::fact-condition)
(s/def :accumulator-condition/result-binding keyword?)

(s/def ::accumulator-condition
  (s/keys :req-un [:accumulator-condition/accumulator :accumulator-condition/from]
          :opt-un [:accumulator-condition/result-binding]))

(def AccumulatorCondition
  {:accumulator                      sc/Any
   :from                             FactCondition
   (sc/optional-key :result-binding) sc/Keyword})

(s/def ::test-condition (s/keys :req-un [::constraints]))
(def TestCondition
  {:constraints [SExpr]})

(s/def :leaf-condition/type ::fact-condition)

(s/def ::leaf-condition
  (s/or
    :fact-condition ::fact-condition
    :accumulator-condition any?                             ; TODO
    :test-condition any?))                                  ; TODO

(def LeafCondition
  (sc/conditional
    :type FactCondition
    :accumulator AccumulatorCondition
    :else TestCondition))

(declare Condition)

(s/def ::boolean-operator #{:or :not :and :exists})

; TODO.
(s/def ::boolean-condition (s/cat :op ::ops
                                  :condition ::condition))
(def BooleanCondition
  [(sc/one (sc/enum :or :not :and :exists) "operator")
   (sc/recursive #'Condition)])

(s/def ::condition
  (s/or :boolean-condition (s/and sequential? ::boolean-condition)
        :leaf-condition (s/and map? ::leaf-condition)))

(def Condition
  (sc/conditional
    sequential? BooleanCondition
    map? LeafCondition))

(s/def ::ns-name symbol?)
(s/def ::name (s/or :string string? :keyword keyword?))
(s/def ::doc string?)
(s/def ::props (s/map-of keyword? any?))
(s/def ::env (s/map-of keyword? any?))
(s/def ::lhs (s/* ::condition))
(s/def ::rhs any?)
(s/def ::rule (s/keys
                :req-un [::lhs ::rhs]
                :opt-un [::ns-name ::name ::doc ::props ::env]))
(def Rule
  {;; :ns-name is currently used to eval the :rhs form of a rule in the same
   ;; context that it was originally defined in.  It is optional and only used
   ;; when given.  It may be used for other purposes in the future.
   (sc/optional-key :ns-name) sc/Symbol
   (sc/optional-key :name)    (sc/cond-pre sc/Str sc/Keyword)
   (sc/optional-key :doc)     sc/Str
   (sc/optional-key :props)   {sc/Keyword sc/Any}
   (sc/optional-key :env)     {sc/Keyword sc/Any}
   :lhs                       [Condition]
   :rhs                       sc/Any})

(s/def :query/params (s/coll-of keyword?))
(s/def ::query
  (s/keys
    :req-un [::lhs :query/params]
    :opt-un [::name ::doc ::props ::env]))

(def Query
  {(sc/optional-key :name)  (sc/cond-pre sc/Str sc/Keyword)
   (sc/optional-key :doc)   sc/Str
   (sc/optional-key :props) {sc/Keyword sc/Any}
   (sc/optional-key :env)   {sc/Keyword sc/Any}
   :lhs                     [Condition]
   :params                  #{sc/Keyword}})

(s/def ::production
  (s/or :rule ::rule
        :query ::query))

(def Production
  (sc/conditional
    :rhs Rule
    :else Query))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema for the Rete network itself.

(s/def :condition-node/node-type #{:join :negation :test :accumulator})
(s/def :condition-node/condition ::leaf-condition)
(s/def :condition-node/new-bindings (s/coll-of keyword?))
(s/def :condition-node/used-bindings (s/coll-of keyword?))
(s/def :condition-node/join-bindings (s/coll-of keyword?))
(s/def :condition-node/join-filter-expressions ::leaf-condition)
(s/def :condition-node/join-filter-join-bindings (s/coll-of keyword?))

(s/def ::condition-node
  (s/keys :req-un [:condition-node/node-type :condition-node/condition
                   :condition-node/used-bindings :condition-node/new-bindings]
          :opt-un [::env :condition-node/join-bindings :condition-node/join-filter-expressions
                   :condition-node/join-filter-join-bindings :accumulator-condition/accumulator
                   :accumulator-condition/result-binding]))

(def ConditionNode
  {:node-type                                   (sc/enum :join :negation :test :accumulator)
   :condition                                   LeafCondition

   ;; Captured environment in which the condition was defined, like closed variables.
   ;; Most rules (such as those defined by defrule) have no surrounding
   ;; environment, but user generated rules might.
   (sc/optional-key :env)                       {sc/Keyword sc/Any}

   ;; Variables used to join to other expressions in the network.
   (sc/optional-key :join-bindings)             #{sc/Keyword}

   ;; Variable bindings used by expressions in this node.
   :used-bindings                               #{sc/Keyword}

   ;; Variable bindings used in the constraints that are not present in the ancestors of this node.
   :new-bindings                                #{sc/Keyword}

   ;; An expression used to filter joined data.
   (sc/optional-key :join-filter-expressions)   LeafCondition

   ;; Bindings used to perform non-hash joins in the join filter expression.
   ;; this is a subset of :used-bindings.
   (sc/optional-key :join-filter-join-bindings) #{sc/Keyword}

   ;; The expression to create the accumulator.
   (sc/optional-key :accumulator)               sc/Any

   ;; The optional fact or accumulator result binding.
   (sc/optional-key :result-binding)            sc/Keyword})

(s/def :production-node/node-type #{:production :query})
(s/def :production-node/production ::rule)
(s/def :production-node/production ::query)
(s/def :production-node/bindings (s/coll-of keyword? :kind set?))
(s/def ::production-node
  (s/keys :req-un [:production-node/node-type]
          :opt-un [:production-node/production :production-node/query
                   :production-node/bindings]))

(def ProductionNode
  {:node-type                    (sc/enum :production :query)

   ;; Rule for rule nodes.
   (sc/optional-key :production) Rule

   ;; Query for query nodes.
   (sc/optional-key :query)      Query

   ;; Bindings used in the rule right-hand side.
   (sc/optional-key :bindings)   #{sc/Keyword}})

(s/def :alpha-node/condition ::fact-condition)
(s/def :alpha-node/beta-children (s/coll-of number? :kind vector?))
(s/def ::alpha-node
  (s/keys :req-un [:alpha-node/condition :alpha-node/beta-children]
          :opt-un [::env]))

;; Alpha network schema.
(def AlphaNode
  {:condition             FactCondition
   ;; Opional environment for the alpha node.
   (sc/optional-key :env) {sc/Keyword sc/Any}
   ;; IDs of the beta nodes that are the children.
   :beta-children         [sc/Num]})

(s/def :beta-graph/forward-edges (s/map-of int? (s/coll-of int? :kind set?)))
(s/def :beta-graph/backward-edges (s/map-of int? (s/coll-of int? :kind set?)))
(s/def :beta-graph/id-to-condition-node
  (s/map-of int?
            (s/or :root-node #(= :clara.rules.compiler/root-condition %)
                  :condition-node ::condition-node)))
(s/def :beta-graph/id-to-production-node (s/map-of int? ::production-node))
(s/def :beta-graph/id-to-new-bindings (s/map-of int? (s/coll-of keyword? :kind set?)))

(s/def ::beta-graph
  (s/keys :req-un [:beta-graph/forward-edges :beta-graph/backward-edges
                   :beta-graph/id-to-condition-node :beta-graph/id-to-production-node
                   :beta-graph/id-to-new-bindings]))

;; A graph representing the beta side of the rete network.
(def BetaGraph
  {;; Edges from parent to child nodes.
   :forward-edges         {sc/Int #{sc/Int}}

   ;; Edges from child to parent nodes.
   :backward-edges        {sc/Int #{sc/Int}}

   ;; Map of identifier to condition nodes.
   :id-to-condition-node  {sc/Int (sc/cond-pre (sc/eq :clara.rules.compiler/root-condition)
                                               ConditionNode)}

   ;; Map of identifier to query or rule nodes.
   :id-to-production-node {sc/Int ProductionNode}

   ;; Map of identifier to new bindings created by the corresponding node.
   :id-to-new-bindings    {sc/Int #{sc/Keyword}}})
