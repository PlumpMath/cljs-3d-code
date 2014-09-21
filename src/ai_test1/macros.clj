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

(comment 
  (my-cond [(= 1 3) 3]
           [(= 1 2) 2]
           [(= 1 1) 1]))

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
