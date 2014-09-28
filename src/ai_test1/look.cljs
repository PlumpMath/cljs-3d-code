(ns look.core
  (:require [util.core :as util]))

(defn behind-side [v]
  (- 1.5 v))

(defn judge-side [v x1 x2]
  "judge left or right.
  left -> -v,right -> v"
  (if (< x1 x2)
    v
    (- v)))

(defn get-x-side [m-pos-x t-pos-x]
  "get target side from you see"
  (if (> t-pos-x m-pos-x )
    "right"
    "left"))

(defn get-z-side [m-pos-z t-pos-z]
  "get target side from you see"
  (cond
   (> t-pos-z m-pos-z) "front"
   (< t-pos-z m-pos-z) "behind"))

(defn purpose-rot [rot main-o target-o]
  (let [main-pos-x (util/get-pos-x main-o)
        main-pos-z (util/get-pos-z main-o)
        target-pos-x (util/get-pos-x target-o)
        target-pos-z (util/get-pos-z target-o)
        nw-rot (if (> target-pos-z main-pos-z)
                 rot
                 (behind-side rot))]
    (judge-side nw-rot main-pos-x target-pos-x)))

(defn judge-rot-angle [rot-angle main-o target-o]
  (let [main-pos-x (util/get-pos-x main-o)
        target-pos-x (util/get-pos-x target-o)]
    (judge-side rot-angle main-pos-x target-pos-x)))

(defn rot-axis [obj v side]
  (cond (= side "right")
        (.rotateOnAxis obj (THREE.Vector3. 0 1 0)  v)
        (= side "left")
        (.rotateOnAxis obj (THREE.Vector3. 0 1 0) (- v))))

(defn set-rot [obj v side]
  (cond (= side "right")
        (set! (-> obj .-position .-y) v)
        (= side "left")
        (set! (-> obj .-position .-y) (- v))))

(defn look-at-target [main-o target-o rot-angle]
  (let [main-rot-y (util/get-rot-y main-o)
        main-pos-x (util/get-pos-x main-o)
        main-pos-z (util/get-pos-z main-o)
        target-pos-x (util/get-pos-x target-o)
        target-pos-z (util/get-pos-z target-o)
        target-x-side (get-x-side main-pos-x target-pos-x)
        target-z-side (get-z-side main-pos-z target-pos-z)
        rot-value (util/sol-rot main-o target-o)
        goal-rot (purpose-rot rot-value main-o target-o)
        judged-angle (judge-rot-angle rot-angle main-o target-o)]
    (cond
     (< main-rot-y goal-rot) 
     (if (< main-pos-z target-pos-z)
       (rot-axis main-o judged-angle target-x-side) ;front go
       (rot-axis main-o (- judged-angle) target-x-side));bhind back

     (> main-rot-y goal-rot) 
     (cond
      (> target-pos-z main-pos-z) ;front back 
      (rot-axis main-o (- judged-angle) target-x-side)

      (< target-pos-z main-pos-z) ;behind go
      (rot-axis main-o judged-angle target-x-side)))))



