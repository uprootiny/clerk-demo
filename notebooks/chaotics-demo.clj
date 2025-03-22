(ns chaotics-demo)

(defn lorenz-attractor
  [sigma rho beta dt x0 y0 z0]
  (let [dx (fn [x y z] (* sigma (- y x)))
        dy (fn [x y z] (* x (- rho z)) (- y))
        dz (fn [x y z] (* x y) (- (* beta z)))]
    (loop [x x0 y y0 z z0 points []]
      (if (< (count points) 10000)
        (recur (+ x (* dx x y z) dt)
               (+ y (* dy x y z) dt)
               (+ z (* dz x y z) dt)
               (conj points [x y]))
        points))))

(defn visualize-lorenz
  []
  (let [points (lorenz-attractor 10 28 8/3 0.01 0.1 0 0)]
    (clerk/table {:head ["X" "Y"]
                  :rows points})))

(visualize-lorenz)


(defn julia-set
  [c max-iter x-min x-max y-min y-max width height]
  (let [pixels (atom [])]
    (doseq [x (range width)
            y (range height)]
      (let [zx (/ (* (- x (/ width 2)) 3.5) width))
            zy (/ (* (- y (/ height 2)) 2.0) height))
            [zx zy] (loop [zx zx zy zy iter 0]
                      (if (>= iter max-iter)
                        [zx zy]
                        (let [zx2 (+ (* zx zx) (- (* zy zy)) (real c))
                              zy2 (* 2 zx zy) (imag c)]
                          (if (> (+ (* zx2 zx2) (* zy2 zy2)) 4)
                            [zx zy]
                            (recur zx2 zy2 (inc iter))))))]
        (swap! pixels conj [x y])))
    @pixels))

(defn visualize-julia
  []
  (let [pixels (julia-set -0.8+0.156i 256 -1.5 1.5 -1.5 1.5 800 800)]
    (clerk/html [:div
                 (for [[x y] pixels]
                   [:div {:style {:width "1px" :height "1px" :background-color "black" :position "absolute" :left (str x "px") :top (str y "px")}}])])))
