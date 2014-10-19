(ns ai-test1.core
  (:require [util.core :as util]
            [look.core :as lk]
            [animation.core :as ani])
  (:require-macros [macros.core :as mac]))

(mac/vars container
          scene
          camera
          renderer
          controls
          stats
          light
          (clock (THREE.Clock.))
          (keyboard (THREEx.KeyboardState.))
          MovingCube
          EnemyCube1
          EnemyCube2
          (arrowList (array))
          (directionList (array)))

(mac/vars (scene (THREE.Scene.))
          ;CAMERA
          (SCREEN_WIDTH (.-innerWidth js/window))
          (SCREEN_HEIGHT (.-innerHeight js/window))
          (VIEW_ANGLE 45)
          (ASPECT (/ SCREEN_WIDTH SCREEN_HEIGHT))
          (NEAR 0.1)
          (FAR 20000))

(def collision-mesh-lst (array))



;RENDERER
(set! renderer (THREE.WebGLRenderer. (js* "{antialias:true}")))
(.setSize renderer SCREEN_WIDTH SCREEN_HEIGHT)
(set! container (.getElementById js/document "ThreeJS"))
(.appendChild container (.-domElement renderer))
;LIGHT
(set! light (THREE.PointLight. (js* "0xffffff")))
(.position.set light 0 250 0)
(.add scene light)
;FLOOR dd
(mac/vars (floorMaterial (THREE.MeshBasicMaterial. (js* "{color:0x444444, side:THREE.DoubleSide}")))
          (floorGeometry (THREE.PlaneGeometry. 1000 1000 10 10))
          (floor (THREE.Mesh. floorGeometry floorMaterial)))
(-> floor .-position .-y (set! -0.5))
(-> floor .-rotation .-x (set! (/ Math.PI 2)))
;(.add scene floor)
;SKYBOX/FOG
(mac/vars (skyBoxGeometry (THREE.CubeGeometry. 10000 10000 10000))
          (skyBoxMaterial (THREE.MeshBasicMaterial. (js* "{color: 0x9999ff, side: THREE.BackSide}")))
          (skyBox (THREE.Mesh. skyBoxGeometry skyBoxMaterial)))
(.add scene skyBox)
;CUSTO
(mac/vars (cubeGeometry (THREE.CubeGeometry. 50 50 50 1 1 1))
          (wireMaterial (THREE.MeshBasicMaterial. (js* "{ color: 0xff0000, wireframe:true }"))))
(set! MovingCube (THREE.Mesh. cubeGeometry wireMaterial))
(-> MovingCube .-position (set! (THREE.Vector3. 50 35.1 100)))
(-> MovingCube .-scale (set! (THREE.Vector3. 0.1 0.1 0.1)))
(.add scene MovingCube)
;camera
(set! camera
      (doto (THREE.PerspectiveCamera. VIEW_ANGLE ASPECT NEAR FAR) ;create camera
        (.position.set 50 25.1 120)
        (.lookAt (.-position scene))))
(.add scene camera)
;enemy cube
(set! EnemyCube1 (THREE.Mesh. cubeGeometry wireMaterial))
(-> EnemyCube1 .-position (set! (THREE.Vector3. 50 25.1 0)))
;(.add scene EnemyCube1)

(def canon-geometry (THREE.SphereGeometry. 10 10 10))
(def canon-material (THREE.MeshNormalMaterial.))
(def canon1 (THREE.Mesh. canon-geometry canon-material))
(set! (-> canon1 .-position .-z) (+ (-> EnemyCube1 .-position .-z) 25))
;(.add scene canon1)
;(.add EnemyCube1 canon1)

(set! EnemyCube2 (THREE.Mesh. cubeGeometry wireMaterial))
(-> EnemyCube2 .-position (set! (THREE.Vector3. 300 25.1 30)))
;(.add scene EnemyCube2)

(def canon2 (THREE.Mesh. canon-geometry canon-material))
(set! (-> canon2 .-position .-z) (+ (-> EnemyCube2 .-position .-z) 25))
;(.add scene canon2)
;(.add EnemyCube2 canon2)

(mac/vars (wallGeometry (THREE.CubeGeometry. 100 100 20 1 1 1))
          (wallMaterial (THREE.MeshLambertMaterial. (js* "{ color: 0xffffff}")))
          (wireMaterial (THREE.MeshLambertMaterial. (js* "{ color: 0xffffff}")))
          (wall (THREE.Mesh. wallGeometry wallMaterial)))
(-> wall .-position (set! (THREE.Vector3. 100 50 -100)))
;(.add scene wall)
;(.push collision-mesh-lst wall)

(def wall2 (THREE.Mesh. wallGeometry wallMaterial))
(-> wall2 .-position (set! (THREE.Vector3. -150 50 0)))
(set! (-> wall2 .-rotation .-y)  (/ 3.14159 2))

;(.push collision-mesh-lst MovingCube)
(.push collision-mesh-lst EnemyCube1)
(.push collision-mesh-lst EnemyCube2)

;(.add scene wall2)
;(.push collision-mesh-lst wall2)

;(def city (THREEx.ProceduralCity.))
;(.add scene city)

;animation vars
  (mac/vars (animOffset 1)  ;starting frame of animation
            (duration 5000) ;milliseconds to complete animation
            (keyframes 250) ;total number of animation frames
            (interpolation (/ duration keyframes)) ;milliseconds per frame
            (lastKeyframe 0) ;previous keyframe
            (currentKeyframe 0))

  (defn ani [model]
    (let [time (mod (.getTime (js/Date.)) duration)
          fl-inter (.floor js/Math (/ time  interpolation ))
          keyframe   (+ fl-inter animOffset)
          model-influ (-> model .-morphTargetInfluences)]
      (when (not (= keyframe currentKeyframe))
        (aset model-influ lastKeyframe 0)
        (aset model-influ currentKeyframe 1)
        (aset model-influ keyframe 0)
        (set! lastKeyframe currentKeyframe)
        (set! currentKeyframe keyframe))
      (aset model-influ keyframe (/ (mod time interpolation ) interpolation))
      (aset model-influ lastKeyframe (- 1 (aget model-influ keyframe)))))
  ;(when (= currentKeyframe 20)
  ;  (set! shot nil)
  ;  (create-ray))))

(defn log [o]
  (.log js/console o))

;level texture
(comment
  (def level-texture (THREE.ImageUtils.loadTexture. "models/Brick_Textured.JPG" ))
  (set! (-> level-texture .-wrapS) THREE.RepeatWrapping)
  (set! (-> level-texture .-wrapT) THREE.RepeatWrapping)
  (.repeat.set	level 10 10)
  (def level-material
    (THREE.MeshBasicMaterial. {:map level-texture
                               :side THREE.DoubleSide }))
  )

  ;load model
(def human)

(defn addModelToScene [geometry materials]
	(let [material (THREE.MeshFaceMaterial. materials)]
    (set! human (THREE.Mesh. geometry material))
    (set! (-> human .-scale) (THREE.Vector3. 1 1 1))
    (.add scene human)))

;load level model
(def level-model)

(defn addLevelToScene [geometry materials]
  (let [material (THREE.MeshFaceMaterial. materials)]
    (set! level-model (THREE.Mesh. geometry material))
    ;for texture
    (set! (-> level-model .-material .-needsUpdate) true)
    (set! (-> level-model .-geometry .-buffersNeedUpdate) true)
    (set! (-> level-model .-geometry .-uvsNeedUpdate) true)

    (set! (-> level-model .-name) "level")

    (set! (-> level-model .-scale) (THREE.Vector3. 100 100 100))
    (.push collision-mesh-lst level-model)
    (.add scene level-model)))

(comment
  (defn addLevelToScene [geometry materials]
	(let [level (THREE.Mesh. geometry level-material)]
    (set! (-> level .-scale) (THREE.Vector3. 100 100 100))
    (.push collision-mesh-lst level)
    (.add scene level)
    (set! level-model level)))
  )


(defn addModelToScene2 [geometry materials]
  ;(.parseAnimations mesh)
  ;(log (-> geometry .-firstAnimation))
  (doseq [i  (range  (-> materials .-length))]
    (set! (-> (aget materials i) .-morphTargets) true))
  (let [material (THREE.MeshFaceMaterial. materials)]
    ;(set! human (THREE.Mesh. geometry material))
    (set! human (THREE.Mesh. geometry material))
    (set! (-> human .-scale) (THREE.Vector3. 1 1 1))
    (.add scene human)))

(def sword)
(defn add-sword [geometry materials]
  (doseq [i  (range  (-> materials .-length))]
    (set! (-> (aget materials i) .-morphTargets) true))
  (let [material (THREE.MeshFaceMaterial. materials)]
    ;(set! human (THREE.Mesh. geometry material))
    (set! sword (THREE.Mesh. geometry material))
    (set! (-> sword .-scale) (THREE.Vector3. 1 1 1))
    (.add scene sword)))

(def ani-fps 6)

(defn add-sword2 [geometry materials]
  (doseq [i  (range  (-> materials .-length))]
    (set! (-> (aget materials i) .-morphTargets) true))
  (let [material (THREE.MeshFaceMaterial. materials)]
    ;(set! human (THREE.Mesh. geometry material))
    (.computeMorphNormals geometry)
    (set! sword (THREE.MorphAnimMesh. geometry material))

    (.parseAnimations sword)
    (.setAnimationLabel sword "slash" 1 250)
    (.setAnimationLabel sword "walk" 251 309)
    ;(.playAnimation sword (-> geometry .-firstAnimation) 6)
    ;(.playAnimation sword "slash" 6)
    (.playAnimation sword "slash" ani-fps)
    (set! (-> sword .-duration) 5000) ;in milliseconds
    (set! (-> sword .-scale) (THREE.Vector3. 10 10 10))

    ;(set! (-> sword .-matrixAutoUpdate) nil)
    ;(.updateMatrix sword)

    (.add scene sword)
    (log sword)
    ))

(def jsonLoader (THREE.JSONLoader.))
;(.load jsonLoader  "./models/sword1.js" addModelToScene2)
;(.load jsonLoader  "./models/man9.js" addModelToScene2)
;(.load jsonLoader  "./models/sword1.js" add-sword2)
(.load jsonLoader  "./models/man9.js" add-sword2)

;(.load jsonLoader  "./models/level5.js" addLevelToScene)

(def ambientLight (THREE.AmbientLight. (js* "0x111111")))
(.add scene ambientLight)

;camera chase MovingCube
(.add MovingCube camera)

(defn key-pressed [key]
  (.pressed keyboard key))

(def bullet-lst (array))

(defn create-bullet [obj]
  (let [geometry (THREE.SphereGeometry. 10 10 10)
        material (THREE.MeshNormalMaterial.)
        bullet (THREE.Mesh. geometry material)
        pos-vec (THREE.Vector3. (-> obj .-position .-x)
                                (-> obj .-position .-y)
                                (-> obj .-position .-z))
        rot-vec (THREE.Vector3. (-> obj .-rotation .-x)
                                (-> obj .-rotation .-y)
                                (-> obj .-rotation .-z))]
    (set! (-> bullet .-position) pos-vec)
    (set! (-> bullet .-rotation .-y) (-> obj .-rotation .-y))
    (.add scene bullet)
    (.push bullet-lst {:three-obj bullet :mv-dist (atom 0) :damage 5})))

;bullet remove from lst and scene
(defn bullet-remove [lst bullet]
  (.remove scene (:three-obj bullet))
  (util/my-remove lst bullet))

(defn update-bullet [bullet-lst]
  (doseq [bullet bullet-lst]
    (let [three-obj (:three-obj bullet)
          mv-dist (:mv-dist bullet)]
      (if (>= @mv-dist 350)
        (bullet-remove bullet-lst bullet)
        (do
          (.translateZ three-obj -5)
          (reset! mv-dist (+ @mv-dist 5)))))))

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

(defn collision-for-level [moving meshlist]
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

(defn delete-shooting [bullet-lst collision-mesh-lst]
  (doseq [bullet bullet-lst]
    (let [bullet-obj (:three-obj bullet)
          colli-meshes (collision bullet-obj collision-mesh-lst)]
      (when (util/excist? colli-meshes)
        (doseq [mesh colli-meshes]
          (.remove scene mesh)
          (util/my-remove collision-mesh-lst mesh))
        (bullet-remove bullet-lst bullet)))))

(def shoted-time (atom 0))

(defn collision-wall? [colli-lst]
  (some #(= % level-model) colli-lst))

(def human-ani-vars (ani/ani-vars :anim-offset 1 :duration 5000 :keyframes 250))
(def sword-ani-vars (ani/ani-vars :anim-offset 1 :duration 5000 :keyframes 250))
;(def walk-ani-vars (ani/ani-vars :anim-offset 251 :duration 10000 :keyframes 310))

(def walking nil)
(def walkingKeys  ["up" "down" "left" "right"])
(def attack nil)
(def ani-state (atom "slash"))
(defn update []
  (let [colli-lst (collision MovingCube collision-mesh-lst)
        delta (-> clock .getDelta)
        elap (-> clock .getElapsedTime)
        moveDistance (* 100  delta)
        rotateAngle  (* (/ Math.PI 2) delta)
        shot-time-lag 0.5]
    (set! walking nil)
    (set! attack nil)

    (when sword
      (when (key-pressed "Y")
        (when (not (= @ani-state "slash"))
          (.playAnimation sword "slash" ani-fps)
          (reset! ani-state "slash"))
        (.updateAnimation sword (* 10000 delta)))

      (when (key-pressed "O")
        (when (not (= @ani-state "walk"))
          (.playAnimation sword "walk" ani-fps)
          (reset! ani-state "walk"))
        (.updateAnimation sword (* 10000 delta))))

    (comment
      (doseq [key walkingKeys]
      (when (key-pressed key)
        (set! walking true))))

    (when (key-pressed "I")
      (set! walking true))

    (when (key-pressed "S")
      (set! attack true)
      ;(set! (-> fakeSword .-position)
       ;     (-> (aget (-> human .-geometry .-bones) 61) .-pos)))
    )
    ;reset model
    (when (key-pressed "R")
      (ani/reset-model human human-ani-vars))

    (when (key-pressed "C")
      (when human (log human)))

    ;(when (not (collision-wall? colli-lst))
      (when (key-pressed "A")
        (.rotateOnAxis MovingCube (THREE.Vector3. 0 1 0) rotateAngle))
      (when (key-pressed "D")
        (mac/-= (-> MovingCube .-rotation .-y) rotateAngle))
      (when (key-pressed "left")
        (.translateX MovingCube (* -1 moveDistance)))
      (when (key-pressed "right")
        (.translateX MovingCube moveDistance))
      (when (key-pressed "up")
        (.translateZ MovingCube (* -1 moveDistance)))
      (when (key-pressed "down")
        (.translateZ MovingCube moveDistance)))
   ;)
    (comment
      (when (key-pressed "S")
        (when (>= elap (+ @shoted-time shot-time-lag))
          (create-bullet MovingCube)
          (reset! shoted-time elap)))
      (when (util/excist? bullet-lst)
        (delete-shooting bullet-lst collision-mesh-lst)
        (update-bullet bullet-lst))
      (lk/look-at-target EnemyCube1 MovingCube rotateAngle)
      (lk/look-at-target EnemyCube2 MovingCube rotateAngle)
      )
    )

(defn render []
  (when (and human attack)
    (ani/anim human human-ani-vars)
    (ani/anim sword sword-ani-vars))
  ;(when (and human walking)
  ;  (ani/anim human walk-ani-vars))
  (.render renderer scene camera))

(defn animate []
  (js/requestAnimationFrame animate)
  (render)
  (update))

(animate)


