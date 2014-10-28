(ns ai-test1.core
  (:require [util.core :as util]
            [look.core :as lk]
            [animation.core :as ani])
  (:require-macros [macros.core :as mac]))

(def clock (THREE.Clock.))
(def keyboard (THREEx.KeyboardState.))

(def scene (THREE.Scene.))
(def SCREEN_WIDTH (.-innerWidth js/window))
(def SCREEN_HEIGHT (.-innerHeight js/window))
(def VIEW_ANGLE 45)
(def ASPECT (/ SCREEN_WIDTH SCREEN_HEIGHT))
(def NEAR 0.1)
(def FAR 20000)

;RENDERER
(def renderer (THREE.WebGLRenderer. (js* "{antialias:true}")))
(.setSize renderer SCREEN_WIDTH SCREEN_HEIGHT)
(def container (.getElementById js/document "ThreeJS"))
(.appendChild container (.-domElement renderer))
;LIGHT
(def light (THREE.PointLight. (js* "0xffffff")))
(.position.set light 0 250 0)
(.add scene light)
;FLOOR dd
(def floorMaterial (THREE.MeshBasicMaterial. (js* "{color:0x444444, side:THREE.DoubleSide}")))
(def floorGeometry (THREE.PlaneGeometry. 1000 1000 10 10))
(def floor (THREE.Mesh. floorGeometry floorMaterial))
(-> floor .-position .-y (set! -0.5))
(-> floor .-rotation .-x (set! (/ Math.PI 2)))
(.add scene floor)
;SKYBOX/FOG
(def skyBoxGeometry (THREE.BoxGeometry. 10000 10000 10000))
(def skyBoxMaterial (THREE.MeshBasicMaterial. (js* "{color: 0x9999ff, side: THREE.BackSide}")))
(def skyBox (THREE.Mesh. skyBoxGeometry skyBoxMaterial))
(.add scene skyBox)
;camera
(def camera (THREE.PerspectiveCamera. VIEW_ANGLE ASPECT NEAR FAR))
(.position.set camera 50 25.1 -100)
(.add scene camera)

(def cubeGeometry (THREE.BoxGeometry. 50 50 50 1 1 1))
(def cube-material (THREE.MeshBasicMaterial. (js* "{color: 0xfeeeff }")))
(def EnemyCube1 (THREE.Mesh. cubeGeometry cube-material))
(.position.set EnemyCube1 -20 25.1 100)

(def moving (THREE.Mesh. cubeGeometry cube-material))

(.add scene EnemyCube1)
;(.add scene moving)

(defn log [o]
  (.log js/console o))

(def level-model)

(defn addLevelToScene [geometry materials]
  (let [material (THREE.MeshFaceMaterial. materials)]
    (set! level-model (THREE.Mesh. geometry material))
    ;for texture
    (set! (-> level-model .-material .-needsUpdate) true)
    (set! (-> level-model .-geometry .-buffersNeedUpdate) true)
    (set! (-> level-model .-geometry .-uvsNeedUpdate) true)

    (set! (-> level-model .-name) "level")

    (set! (-> level-model .-scale) (THREE.Vector3. 1 1 1))
    (.add scene level-model)))

(comment
  (defn addLevelToScene [geometry materials]
  (let [level (THREE.Mesh. geometry level-material)]
    (set! (-> level .-scale) (THREE.Vector3. 100 100 100))
    (.push enemy-lst level)
    (.add scene level)
    (set! level-model level)))
  )

(def jsonLoader (THREE.JSONLoader.))
;(.load jsonLoader  "./models/level5.js" addLevelToScene)

(def human (ani/make-chara :character-path "./models/man9.js"
                           :weapon-path "./models/sword2.js"
                           :animation-info [{:label "slash" :start 1 :end 250}
                                            {:label "walk" :start 251 :end 309}
                                            {:label "stand" :start 251 :end 252}]
                           :first-anim "stand"
                           :scene scene
                           :camera camera))

(def ambientLight (THREE.AmbientLight. (js* "0x111111")))
(.add scene ambientLight)

(defn key-pressed [key]
  (.pressed keyboard key))

(defn set-colli-obj [colli-objs colli-lst]
    (doseq [obj colli-objs]
      (let [mesh (-> obj .-object)]
        (if (not (some #(= % mesh) colli-lst))
          (.push colli-lst mesh))))
  colli-lst)

(defn collision [moving meshlist]
  (let [originPoint (-> moving .-position .clone)
        colli-lst (array)]
    (doseq [index (range (-> moving .-geometry .-vertices .-length))]
      (mac/vars (localVertex (.clone (aget (-> moving .-geometry .-vertices) index)))
                (globalVertex (.applyMatrix4 localVertex (-> moving .-matrix)))
                (directionVector (.sub globalVertex (-> moving .-position)))
                (ray (THREE.Raycaster. originPoint (-> directionVector .clone .normalize)))
                (colli (.intersectObjects ray meshlist)))
      (when (and (> (-> colli .-length) 0)
                 (< (-> (aget colli 0) .-distance) (.length directionVector)))
        (set-colli-obj colli colli-lst)))
    colli-lst))

(def enemy-lst (array))

(.push enemy-lst EnemyCube1)

(defn slash-colli [scene chara lst]
  ;when has been loaded weapon
  (when (and @(:character-mesh chara)
             @(:weapon-mesh chara))
    (let [mesh-lst (collision @(:weapon-mesh chara) lst)]
      (if mesh-lst
        (doseq [mesh mesh-lst]
          (.remove scene mesh)
          (util/my-remove enemy-lst mesh))))))

(comment
  (defn baz [o fnc lst]
    (let [clone-o (.clone o)]
      (collision (fnc clone-o) lst)))
  )

(def ani-keys ["Y" "O"])

(def ani-state (atom "stand"))

(def ani-fps 6)

(defn update []
  (let [delta (-> clock .getDelta)
        elap (-> clock .getElapsedTime)
        moveDistance (* 100  delta)
        rotateAngle  (* (/ Math.PI 2) delta)]
    (when human
      (if (some #(key-pressed %) ani-keys)
        (do
          (when (key-pressed "Y")
            (when (not (= @ani-state "slash"))
              (ani/play-animation human "slash" ani-fps)
              (reset! ani-state "slash"))
            (ani/update-animation human (* 10000 delta))
            (slash-colli scene human enemy-lst)
            )

          (when (key-pressed "O")
            (when (not (= @ani-state "walk"))
              (ani/play-animation human "walk" ani-fps)
              (reset! ani-state "walk"))
            (ani/update-animation human (* 10000 delta))))
        ;when don't pressed key -> stand
        (do
          (when (not (= @ani-state "stand"))
            (ani/play-animation human "stand" ani-fps)
            (reset! ani-state "stand"))
          (ani/update-animation human (* 10000 delta)))))

      (when (key-pressed "A")
        (ani/twice-doing human #(.rotateOnAxis % (THREE.Vector3. 0 1 0) rotateAngle)))
      (when (key-pressed "D")
        (ani/twice-doing human #(mac/-= (-> % .-rotation .-y) rotateAngle)))
      (when (key-pressed "left")
        (ani/twice-doing human #(.translateX % (* -1 moveDistance))))
      (when (key-pressed "right")
        (ani/twice-doing human #(.translateX % moveDistance)))
      (when (key-pressed "up")
        (ani/twice-doing human #(.translateZ % (* -1 moveDistance))))
      (when (key-pressed "down")
        (ani/twice-doing human #(.translateZ % moveDistance)))

    (comment
      (when (key-pressed "A")
        (.rotateOnAxis moving (THREE.Vector3. 0 1 0) rotateAngle))
      (when (key-pressed "D")
        (mac/-= (-> moving .-rotation .-y) rotateAngle))
      (when (key-pressed "left")
        (.translateX moving (* -1 moveDistance)))
      (when (key-pressed "right")
        (.translateX moving moveDistance))
      (when (key-pressed "up")
        (.translateZ moving (* -1 moveDistance)))
      (when (key-pressed "down")
        (.translateZ moving moveDistance)))
    ;(test-colli moving enemy-lst)
    ;(slash-colli scene human enemy-lst)
    ;(human-colli scene human enemy-lst)
    ))

(defn render []
  (.render renderer scene camera))

(defn animate []
  (js/requestAnimationFrame animate)
  (render)
  (update))

(animate)


