(ns macros.core)

(defmacro vars [& vars]
  `(do
     ~@(for [var vars]
         (if (list? var)
           `(def ~(first var) ~(second var))
           `(def ~var)))))

(defmacro += [obj num]
  `(set! ~obj (+ ~obj ~num)))

(defmacro -= [obj num]
  `(set! ~obj (- ~obj ~num)))

(defn make-declare [behaviors-map]
  (for [bh behaviors-map]
    (let [next-bh (symbol (:name bh))]
      `(declare ~next-bh))))

(defmacro my-cond
  [& clauses]
  (when clauses
    (list 'if (first (first clauses))
          (if (first clauses)
            (second (first clauses))
            (throw (IllegalArgumentException.
                    "cond requires an even number of forms")))
          (cons 'macros.core/my-cond (next clauses)))))

(defn make-cond [state-var multi-cnd]
  `(my-cond
    ~@(for [cnd multi-cnd]
        (let [end-cnd (:end-cnd cnd)
              next-bh (symbol (:next-bh cnd))]
          [`(~end-cnd) `(reset! ~state-var ~next-bh)]))))

(defn make-when [state-var end-cnd next-bh]
  `(when  (~end-cnd)
     (reset! ~state-var ~(symbol next-bh))))

(defn make-behaviors [args-vec state-var behaviors-map]
  (for [bh behaviors-map]
    (let [name (symbol (:name bh))
          behavior (:behavior bh)
          multi-cnd (:multi-cnd bh)
          end-cnd (:end-cnd bh)
          next-bh (:next-bh bh)]
      `(defn ~name ~args-vec
         (~behavior)
         ~(if multi-cnd
            (make-cond state-var multi-cnd)
            (make-when state-var end-cnd next-bh))))))

(defmacro defaction [name args-vec & behaviors]
  `(do
     (declare  ~(symbol (str name "-state")))
     ~@(make-declare behaviors)
     ~@(make-behaviors args-vec (symbol (str name "-state")) behaviors)
     (def ~(vary-meta (symbol (str name "-state"))
                      assoc :first-behavior (symbol (:name (first behaviors))))
       (atom ~(symbol (:name (first behaviors)))))
     (defn ~(symbol name) ~args-vec
       ((deref ~(symbol (str name "-state"))) ~@args-vec))))

(defmacro let-map
  "Equivalent of (let [a 5 b (+ a 5)] {:a a :b b})."
  [kvs]
  (let [keys (keys (apply hash-map kvs))
        keyword-symbols (mapcat #(vector (keyword (str %)) %) keys)]
    `(let [~@kvs]
       (hash-map ~@keyword-symbols))))

(defn action-code []
  '(fn [this]
     (let [bhv (:behavior @state)
           end-cnd (:end-cnd @state)
           next-state (if (:next-state @state) 
                        ((:next-state @state) this))
           multi-cnd (:multi-cnd @state)]
       (bhv)
       ;update state
       (if multi-cnd
         (loop [cnd-vec multi-cnd]
           (println cnd-vec)
           (if (not (first cnd-vec))
             nil
             (let [cnd (first cnd-vec)
                   end-cnd (:end-cnd cnd)
                   next-state ((:next-state cnd) this)]
               (if (end-cnd)
                 (reset! state next-state))
               (recur (rest cnd-vec)))))
         (if (end-cnd)
           (reset! state next-state))))))

(comment
  (defmacro defenemy [name slots]
  `(defn ~(symbol name) []
     (let-map [~@slots
               ~(symbol "state") (atom ~(symbol "up"))
               ~(symbol "action") ~(action-code)]))))

(defmacro defenemy [name args-vec slots]
  `(defn ~(symbol name) ~args-vec
     (let-map [~@slots
               ~(symbol "state") (atom ~(symbol "up"))
               ~(symbol "action") ~(action-code)])))

(defn action [obj]
  ((:action obj) obj))

(defenemy test-enemy [my-string]
  [print-my-str (fn [] (println my-string))
   count (atom 0)
   count-all (atom 0)
   count-up (fn [] 
              (reset! count (+ @count 1))
              (reset! count-all (+ @count-all 1)))
   count-down (fn [] 
                (reset! count (- @count 1))
                (reset! count-all (+ @count-all 1)))
   up   {:behavior (fn [] 
                     (count-up)
                     (println "up")
                     (print-my-str))
         :end-cnd #(>= @count 10) :next-state :down}
   down {:behavior (fn [] 
                     (count-down)
                     (println "down")
                     (print-my-str))
         :multi-cnd  [{:end-cnd #(< @count 0) :next-state :up}
                      {:end-cnd #(>= @count-all 30) :next-state :strange}]}
   strange {:behavior (fn [] (println "strange"))
            :end-cnd (fn [] nil)}])
