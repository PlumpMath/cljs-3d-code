(ns ai-test1.core
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
         (collidableMeshList (array))
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
(set! EnemyCube1 (THREE.Mesh. cubeGeometry wireMaterial))
(-> EnemyCube1 .-position (set! (THREE.Vector3. 50 25.1 0)))
(.add scene EnemyCube1)
(set! EnemyCube2 (THREE.Mesh. cubeGeometry wireMaterial))
(-> EnemyCube2 .-position (set! (THREE.Vector3. 300 25.1 30)))
(.add scene EnemyCube2)
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

(.add MovingCube camera)

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
    (.push bullet-lst {:three-obj bullet :mv-dist (atom 0)})))

(defn update-bullet [bullet-lst]
  (doseq [bullet bullet-lst]
    (let [three-obj (:three-obj bullet)
          mv-dist (:mv-dist bullet)]
      (if (>= @mv-dist 350)
        (do
          (.remove scene three-obj)
          (remove #(= bullet %) bullet-lst))
        (do
          (.translateZ three-obj -5)
          ;(mac/+= (-> three-obj .-position .-z) 5)
          (reset! mv-dist (+ @mv-dist 5)))))))
  
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

(comment (mac/defaction up-down-left [mv-distance]
  {:name "up" :behavior (fn []
                          (mac/-= (-> EnemyCube .-position .-z) mv-distance)
                          (reset! test-count (+ @test-count 1))
                          (reset! up-count (+ @up-count 1)))
   :end-cnd #(>= @test-count 20)
   :next-bh "down"}
  {:name "down" :behavior (fn []
                            (mac/+= (-> EnemyCube .-position .-z) mv-distance)
                            (reset! test-count (- @test-count 1)))
   :end-cnd #(<= @test-count 0) :next-bh "left"}
  {:name "left" :behavior (fn []
                            (mac/-= (-> EnemyCube .-position .-x) mv-distance)
                            (reset! test-count (- @test-count 1)))
   :multi-cnd [{:end-cnd #(>= @up-count 100) :next-bh "down"}
                {:end-cnd #(<= @test-count -20) :next-bh "up"}]}))

(mac/defenemy test-enemy [three-obj] "up"
  [test-count (atom 1)
   up-count (atom 1)
   count-up (fn [] (reset! test-count (+ @test-count 1)))
   count-down (fn [] (reset! test-count (- @test-count 1)))
   up-count-up (fn [] (reset! up-count (+ @up-count 1)))
   mv-distance (atom nil)
   set-mv-distance (fn [value] (reset! mv-distance value))
   up {:behavior (fn []
                   (mac/-= (-> three-obj .-position .-z) @mv-distance)
                   (count-up)
                   (up-count-up))
       :end-cnd (fn [] (key-pressed "Z")) :next-state :down}
   down {:behavior (fn []
                     (mac/+= (-> three-obj .-position .-z) @mv-distance)
                     (count-down))
         :end-cnd #(<= @test-count 0) :next-state :left}
   left {:behavior (fn []
                     (mac/-= (-> three-obj .-position .-x) @mv-distance)
                     (count-down))
         :multi-cnd [{:end-cnd #(>= @up-count 100) :next-state :down}
                     {:end-cnd #(<= @test-count -20) :next-state :up}]}])

(comment (>= @test-count 20))

(def test-obj1 (test-enemy EnemyCube1))
(def test-obj2 (test-enemy EnemyCube2))

(defn action [obj]
  ((:action obj) obj))

(defn update-test-enemy [obj mv-distance]
  ((:set-mv-distance obj) mv-distance)
  (action obj))

(def shoted-time (atom 0))

(defn update []
  (let [colli-lst (collision MovingCube collidableMeshList)
        delta (-> clock .getDelta)
        elap (-> clock .getElapsedTime)
	      moveDistance (* 100  delta)
	      rotateAngle  (* (/ Math.PI 2) delta)
        shot-time-lag 0.5]
    (when (key-pressed "A")
      (mac/+= (-> MovingCube .-rotation .-y) rotateAngle))
    (when (key-pressed "D")
      (mac/-= (-> MovingCube .-rotation .-y) rotateAngle))
    (when (key-pressed "left")
      (.translateX MovingCube (* -1 moveDistance)))
    (when (key-pressed "right")
      (.translateX MovingCube moveDistance))
    (when (key-pressed "up")
      (.translateZ MovingCube (* -1 moveDistance)))
    (when (key-pressed "down")
      (.translateZ MovingCube moveDistance))
    (when (key-pressed "S")
      (when (>= elap (+ @shoted-time shot-time-lag))
        (create-bullet MovingCube)
        (reset! shoted-time elap)))
    (when (collision? colli-lst)
      (log "collision"))
    (update-bullet bullet-lst)
    ;(update-test-enemy test-obj1 moveDistance)
    ;(update-test-enemy test-obj2 (+ moveDistance 50))
    ;(up-down-left moveDistance)
    ))

(defn render []
  (.render renderer scene camera))

(defn animate []
  (js/requestAnimationFrame animate)
  (render)
  (update))

(animate)
