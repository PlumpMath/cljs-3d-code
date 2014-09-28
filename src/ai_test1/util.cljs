(ns util.core)

(defn my-remove [arr item]
  (doseq [i (range (-> arr .-length))]
    (when (= item (aget arr i))
      (.splice arr i 1))))

(defn excist? [v]
  (not (empty? v)))

(defn acos [v]
  (.acos js/Math v))

(defn get-pos-x [obj]
  (-> obj .-position .-x))

(defn get-pos-z [obj]
  (-> obj .-position .-z))

(defn get-rot-y [obj]
  (-> obj .-rotation .-y))

(defn line-length [v1 v2]
  (if (> v1 v2)
    (- v1 v2)
    (- v2 v1)))

(defn sqrt [v]
  (.sqrt js/Math v))

(defn pow [v n]
  (.pow js/Math v n))

(defn pitago [v1 v2]
  (let [pw1 (pow v1 2)
        pw2 (pow v2 2)]
    (sqrt (+ pw1 pw2))))

(comment(defn baz [main-o target-o]
  "return rotate value for look at target"
  (let [main-x (get-pos-x main-o)
        main-z (get-pos-z main-o)
        target-x (get-pos-x target-o)
        target-z (get-pos-z target-o)
        other-x (if (> main-z target-z) target-x main-x)
        other-z (if (> main-z target-z) main-z target-z)
        line-a (if (> main-z target-z)
                 (line-length target-z other-z)
                 (line-length target-x other-x))
        line-b (if (> main-z target-z) 
                 (line-length main-x other-x)
                 (line-length main-z other-z))
        line-c (pitago line-a line-b)
        pow-a (pow line-a 2)
        pow-b (pow line-b 2)
        pow-c (pow line-c 2)]
    (acos (/ (- (+ pow-b pow-c) pow-a) (* 2 line-b line-c)))))
)

(defn sol-rot [main-o target-o]
  "return rotate value for look at target"
  (let [main-x (get-pos-x main-o)
        main-z (get-pos-z main-o)
        target-x (get-pos-x target-o)
        target-z (get-pos-z target-o)
        other-x (if (> main-z target-z) target-x main-x)
        other-z (if (> main-z target-z) main-z target-z)
        line-a (if (> main-z target-z)
                 (line-length target-z other-z)
                 (line-length target-x other-x))
        line-b (if (> main-z target-z) 
                 (line-length main-x other-x)
                 (line-length main-z other-z))
        line-c (pitago line-a line-b)
        pow-a (pow line-a 2)
        pow-b (pow line-b 2)
        pow-c (pow line-c 2)]
    (acos (/ (- (+ pow-b pow-c) pow-a) (* 2 line-b line-c)))))
