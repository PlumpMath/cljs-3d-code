(ns animation.core
  (:require-macros [macros.core :as mac]))

(defn make-load-fn [scene mesh-var animation-info first-anim camera]
  (fn [geometry materials]
    (doseq [i  (range  (-> materials .-length))]
      (set! (-> (aget materials i) .-morphTargets) true))
    (let [material (THREE.MeshFaceMaterial. materials)]
      (reset! mesh-var (THREE.MorphAnimMesh. geometry material))
      (set! (-> mesh-var .-duration) 5000) ;in milliseconds
      (set! (-> mesh-var .-scale) (THREE.Vector3. 10 10 10))
      (.log js/console (-> mesh-var .-scale))
      ;animation
      (doseq [ani-info animation-info]
        (let [label (:label ani-info)
              start (:start ani-info)
              end (:end ani-info)]
        (.setAnimationLabel @mesh-var label start end)))
      (.playAnimation @mesh-var first-anim 6)
      (.add @mesh-var camera)
      (.log js/console @mesh-var)
      (.lookAt camera (-> @mesh-var .-position))
      (.add scene @mesh-var))))

(defn make-chara [& {:keys [scene character-path weapon-path animation-info first-anim camera]}]
  (let [loader (THREE.JSONLoader.)
        chara-mesh (atom nil)
        weapon-mesh (atom nil)
        load-chara (make-load-fn scene chara-mesh animation-info first-anim camera)
        load-weapon (make-load-fn scene weapon-mesh animation-info first-anim camera)]
    (.load loader character-path load-chara)
    (when weapon-path
      (.load loader weapon-path load-weapon))
    {:character-mesh chara-mesh :weapon-mesh weapon-mesh}))

(defn twice-doing [chara fnc]
  (let [chara-mesh @(:character-mesh chara)
        weapon-mesh @(:weapon-mesh chara)]
    (when chara-mesh
      (fnc chara-mesh))
    (when weapon-mesh
      (fnc weapon-mesh))))

(defn set-animation [chara label start end]
  (twice-doing chara #(.setAnimationLabel % label start end)))

(defn play-animation [chara label fps]
  (twice-doing chara #(.playAnimation % label fps)))

(defn update-animation [chara delta]
  (twice-doing chara #(.updateAnimation % delta)))



