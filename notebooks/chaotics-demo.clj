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
