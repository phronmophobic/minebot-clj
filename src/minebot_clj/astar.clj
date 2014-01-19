(ns mine-bot-clj.astar
  (:require [clojure.data.priority-map :refer [priority-map]]))




(defn astar
  ([start dist successors]
     (astar (priority-map [start] (dist start))
            dist
            successors
            #{start}))
  ([queue distf successorsf visited]
     (when-let [path (-> queue peek first)]
       (let [node (last path)]
         (if (zero? (distf node))
           path
           (let [successors (successorsf node)
                 queue (-> queue
                           (into (for [successor successors
                                       :when (not (contains? visited successor))]
                                   [(conj path successor) (+ (count path)
                                                             (distf successor))]))
                           (dissoc path))
                 visited (into visited successors)]
             (recur queue
                    distf
                    successorsf
                    visited
                    )))
         ))))


(defn make-map [& args]
  (into {}
        (for [[j s] (map-indexed vector args)
              [i c] (map-indexed vector s)]
          [[i j] (case c
                     \S :start
                     \  nil
                     \X :wall
                     \G :goal)])))

(defmacro defmap [name & body]
  `(def ~name
     (make-map ~@body)))



(defmap test1
  "S  X  G "
  "   X    "
  "   X    "
  "        "
  "   X    ")

(defn map-width [m]
  (->> m
       keys
       (map first)
       (apply max)
       inc))

(defn map-height [m]
  (->> m
       keys
       (map second)
       (apply max)
       inc))

(defn print-map [m & [path]]
  (let [width (map-width m)
        height (map-height m)]
    (doseq [x (range (+ 2 width))]
      (print "-"))
    (println)
    (doseq [y (range height)]
      (print "|")
      (doseq [x (range width)]
        (print 
         (let [tile (get m [x y])]
           (cond 
            (= tile :start) "S"
            (= tile :wall) "X"
            (= tile :goal) "G"
            (some #(= [x y] %) path) "*"
            (nil? tile) " "))))
      (print "|")
      (println))
    (doseq [x (range (+ 2 width))]
      (print "-"))))


(defn map-succesors [m [x y]]
  (for [i (range (dec x) (+ x 2))
        j (range (dec y) (+ y 2))
        :when (and (>= i 0)
                   (>= j 0)
                   (< i (map-width m))
                   (< j (map-height m))
                   (not= :wall (get m [i j]))
                   (or (= x i)
                       (= y j))
                   (or (not= x i)
                       (not= y j)))]
    [i j]))

(defn abs [x]
  (if (neg? x)
    (- x)
    x))

(defn map-dist [[x y] [gx gy]]
  (+ (abs (- x gx))
     (abs (- y gy))))

(defn map-astar [m]
  (let [start (->> m
                   (filter #(= (second %) :start))
                   first
                   first)
        
        goal (->> m
                   (filter #(= (second %) :goal))
                   first
                   first)]
    (astar start #(map-dist % goal) #(map-succesors m %))))

(let [m (make-map
         "S     "
         "XXXX   "
         "G     "
         )
      path (map-astar m)]
  (print-map m path)
  )

