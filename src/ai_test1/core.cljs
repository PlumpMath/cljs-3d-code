(ns ai-test1.core)

(ns three-cljs.core
  (:require-macros [macros.core :as mac])
  (:require [ai.core :as ai]))

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
         EnemyCube
         (collidableMeshList (array))
         (arrowList (array))
         (directionList (array)))

(defn init []
  (mac/vars (scene (THREE.Scene.))
	;CAMERA
	         (SCREEN_WIDTH (.-innerWidth js/window))
           (SCREEN_HEIGHT (.-innerHeight js/window))
	         (VIEW_ANGLE 45)
           (ASPECT (/ SCREEN_WIDTH SCREEN_HEIGHT))
           (NEAR 0.1)
           (FAR 20000))
  (set! camera
        (doto (THREE.PerspectiveCamera. VIEW_ANGLE ASPECT NEAR FAR) ;create camera
	        (.position.set 0 150 400)
	        (.lookAt (.-position scene))))
  (.add scene camera)
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
	(.add scene floor)
	;SKYBOX/FOG
	(mac/vars (skyBoxGeometry (THREE.CubeGeometry. 10000 10000 10000))
	         (skyBoxMaterial (THREE.MeshBasicMaterial. (js* "{color: 0x9999ff, side: THREE.BackSide}")))
	         (skyBox (THREE.Mesh. skyBoxGeometry skyBoxMaterial)))
	(.add scene skyBox)
  ;CUSTO
  (mac/vars (cubeGeometry (THREE.CubeGeometry. 50 50 50 1 1 1))
	         (wireMaterial (THREE.MeshBasicMaterial. (js* "{ color: 0xff0000, wireframe:true }"))))
	(set! MovingCube (THREE.Mesh. cubeGeometry wireMaterial))
	(-> MovingCube .-position (set! (THREE.Vector3. 0 25.1 0)))
	(.add scene MovingCube)
  ;enemy cube
  (set! EnemyCube (THREE.Mesh. cubeGeometry wireMaterial))
	(-> EnemyCube .-position (set! (THREE.Vector3. 0 55.1 0)))
	(.add scene EnemyCube)
	(mac/vars (wallGeometry (THREE.CubeGeometry. 100 100 20 1 1 1))
	         (wallMaterial (THREE.MeshLambertMaterial. (js* "{ color: 0xffffff}")))
	         (wireMaterial (THREE.MeshLambertMaterial. (js* "{ color: 0xffffff}")))
           (wall (THREE.Mesh. wallGeometry wallMaterial)))
  (-> wall .-position (set! (THREE.Vector3. 100 50 -100)))
	(.add scene wall)
	(.push collidableMeshList wall)

	(def wall2 (THREE.Mesh. wallGeometry wallMaterial))
  (-> wall2 .-position (set! (THREE.Vector3. -150 50 0)))
  (set! (-> wall2 .-rotation .-y)  (/ 3.14159 2))

	(.add scene wall2)
	(.push collidableMeshList wall2)

  (.add MovingCube camera))

(defn collision [moving meshlist]
  (let [originPoint (-> moving .-position .clone)
        colli-list (array)]
  	(doseq [index (range (-> moving .-geometry .-vertices .-length))]
      (mac/vars (localVertex (.clone (aget (-> moving .-geometry .-vertices) index)))
	  	         (globalVertex (.applyMatrix4 localVertex (-> moving .-matrix)))
	  	         (directionVector (.sub globalVertex (-> moving .-position)))
               (ray (THREE.Raycaster. originPoint (-> directionVector .clone .normalize)))
               (colli (.intersectObjects ray meshlist)))
      (.push colli-list colli))
    colli-list))

(defn collision? [result-lst]
  (some #(> (-> % .-length) 0) result-lst))

(defn key-pressed [key]
  (.pressed keyboard key))

(defn log [o]
  (.log js/console o))

(def test-count (atom 1))

(mac/defaction up-down-left [mv-distance]
  {:name "up" :behavior (fn []
                          (mac/-= (-> EnemyCube .-position .-z) mv-distance)
                          (reset! test-count (+ @test-count 1)))
   :end-cond #(>= @test-count 20)
   :next "down"}
  {:name "down" :behavior (fn []
                            (mac/+= (-> EnemyCube .-position .-z) mv-distance)
                            (reset! test-count (- @test-count 1)))
   :end-cond #(<= @test-count 0)
   :next "left"}
  {:name "left" :behavior (fn []
                            (mac/-= (-> EnemyCube .-position .-x) mv-distance)
                            (reset! test-count (- @test-count 1)))
   :end-cond #(<= @test-count -20)
   :next "up"})

(defn update []
  (let [colli-lst (collision MovingCube collidableMeshList)
        delta (-> clock .getDelta)
	      moveDistance (* 100  delta)
	      rotateAngle  (* (/ Math.PI 2) delta)]
    (when (key-pressed "A")
      (mac/+= (-> MovingCube .-rotation .-y) rotateAngle))
    (when (key-pressed "D")
      (mac/-= (-> MovingCube .-rotation .-y) rotateAngle))
    (when (key-pressed "left")
      (mac/-= (-> MovingCube .-position .-x) moveDistance))
    (when (key-pressed "right")
      (mac/+= (-> MovingCube .-position .-x) moveDistance))
    (when (key-pressed "up")
      (mac/-= (-> MovingCube .-position .-z) moveDistance))
    (when (key-pressed "down")
      (mac/+= (-> MovingCube .-position .-z) moveDistance))
    (when (collision? colli-lst)
      (log "collision"))
    (up-down-left moveDistance)))

(defn render []
  (.render renderer scene camera))

(defn animate []
  (js/requestAnimationFrame animate)
  (render)
  (update))

(init)
(animate)
