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
    (let [next (symbol (:next bh))]
      `(declare ~next))))

(defn make-behaviors [args-vec state-var behaviors-map]
  (for [bh behaviors-map]
    (let [name (symbol (:name bh))
          behavior (:behavior bh)
          end-cond (:end-cond bh)
          next (symbol (:next bh))]
         `(defn ~name ~args-vec
            (~behavior)
            (when  (~end-cond)
              (reset! ~state-var ~next))))))

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
