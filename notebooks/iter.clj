(ns fractal-noise)

(defn mandelbrot
  [c max-iter]
  (loop [z 0 iter 0]
    (if (>= iter max-iter)
      iter
      (let [z2 (+ (* z z) c)]
        (if (> (Math/abs z2) 2)
          iter
          (recur z2 (inc iter)))))))

(ns perlin-noise)

(defn fade [t]
  (* t t t (- (* t (- (* t 6) 15)) 10)))

(defn gradient [hash x y]
  (+ (* x hash) (* y (- hash))))

(defn perlin-noise [x y]
  ; חישוב רעש פרלין לנקודה נתונה
)
(ns fluid-dynamics
  (:require [quil.core :as q]))

(def width 100)
(def height 100)
(def num-particles 500)
(def viscosity 0.1)

(defn initialize-particles []
  (repeatedly num-particles
              (fn [] {:pos [(rand width) (rand height)]
                      :vel [(rand -0.5 0.5) (rand -0.5 0.5)]})))

(def particles (atom (initialize-particles)))

(defn update-particles [particles]
  (map (fn [{:keys [pos vel]}]
         (let [[x y] pos
               [vx vy] vel
               new-vx (* vx (- 1 viscosity))
               new-vy (* vy (- 1 viscosity))
               new-x (+ x new-vx)
               new-y (+ y new-vy)
               clipped-x (if (or (< new-x 0) (> new-x width)) (- new-x) new-x)
               clipped-y (if (or (< new-y 0) (> new-y height)) (- new-y) new-y)]
           {:pos [clipped-x clipped-y]
            :vel [new-vx new-vy]}))
       particles))

(defn simulate []
  (swap! particles update-particles))

(q/defsketch fluid-dynamics-simulation
  :title "Fluid Dynamics Simulation"
  :size [width height]
  :setup (fn [] (q/frame-rate 30))
  :draw (fn []
          (q/background 255)
          (q/fill 0)
          (doseq [{:keys [pos]} @particles]
            (q/ellipse (:x pos) (:y pos) 2 2))
          (simulate)))



(ns raymarched-fractals)

(defn raymarch
  [ray origin direction max-steps]
  ; Implement raymarching logic
  )

(defn fractal-distance
  [point]
  ; Calculate distance to fractal boundary
  )

(defn visualize-raymarched-fractal
  [width height]
  (let [pixels (atom [])]
    (doseq [x (range width)
            y (range height)]
      (let [ray (vec3 x y 0)
            origin (vec3 0 0 0)
            direction (normalize ray)
            distance (raymarch ray origin direction 100)]
        (swap! pixels conj [x y distance])))
    @pixels))

(clerk/html [:div
             (for [[x y distance] (visualize-raymarched-fractal 800 600)]
               [:div {:style {:width "1px" :height "1px" :background-color (str "rgb(" distance ",0,0)") :position "absolute" :left (str x "px") :top (str y "px")}}])])





(ns shimmering-graph
  (:require [quil.core :as q]))

(def x-values (range 0 (* Math/PI 4) 0.1))
(def noise-amplitude 0.2)

(defn add-noise [frame]
  (map #(-> % Math/sin (+ (* noise-amplitude (Math/sin (+ % (/ frame 50)))))) x-values))

(q/defsketch shimmering-graph
  :title "Shimmering Graph Effect"
  :size [400 300]
  :setup (fn [] (q/frame-rate 30))
  :draw (fn []
          (q/background 255)
          (q/stroke-weight 2)
          (q/stroke 0)
          (let [y-values (add-noise (.frameCount q))]
            (q/begin-shape)
            (doseq [[x y] (map vector x-values y-values)]
              (q/vertex (* x (/ q/width Math/PI)) (+ (/ q/height 2) (* y (/ q/height Math/PI)))))
            (q/end-shape)))))

(defn visualize-fractal-noise
  [width height max-iter]
  (let [pixels (atom [])]
    (doseq [x (range width)
            y (range height)]
      (let [cx (/ (* (- x (/ width 2)) 3.5) width)
            cy (/ (* (- y (/ height 2)) 2.0) height)
            iter (mandelbrot (+ cx (* cy 1i)) max-iter)
            noise-value (perlin-noise x y 6)]
        (swap! pixels conj [x y iter noise-value])))
    @pixels))

(clerk/html [:div
             (for [[x y iter noise-value] (visualize-fractal-noise 800 600 256)]
               [:div {:style {:width "1px" :height "1px" :background-color (str "rgb(" iter ",0," noise-value ")") :position "absolute" :left (str x "px") :top (str y "px")}}])])


(ns efficient-recursion)

(defn efficient-recursive-iteration [current limit]
  (if (< current limit)
    (cons current
          (efficient-recursive-iteration (+ current 1) limit))
    []))

(efficient-recursive-iteration 0 10)


