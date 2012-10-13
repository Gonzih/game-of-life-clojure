(ns game-of-life-clojure.core
  (:use [clojure.java.shell :only [sh]]
        [clojure.string :only [join]]))

(defn random-world [size-x size-y]
  (vec (take (* size-x size-y)
             (repeatedly #(rand-int 2)))))

(defn new-game [size-x size-y]
  {:world (random-world size-x size-y) :size-x size-x :size-y size-y})

(defn map-cell [cell]
  (if (= cell 1) "·"))

(defn print-world [{:keys [size-x world]}]
  (join "\n" (for [row (partition size-x world)]
               (join " " (map map-cell row)))))

(defn clear [])

(defn generate-neighbor-vecs []
  (let [v [-1 0 1]]
    (->> (for [x v y v] [x y])
         (remove #(every? zero? %)))))

(def neighbor-vecs (generate-neighbor-vecs))

(defn calc-coords-from-vec [x y [v_x v_y]]
  [(+ x v_x) (+ y v_y)])

(defn get-index [size-x [x y]]
  (letfn [(dec-if-gt-zero [n]
            (if (> 0 n)
              (dec n)
              n))]
    (-> y
        (dec-if-gt-zero)
        (* size-x)
        (+ (dec-if-gt-zero x)))))

(defn out-of-bound? [size-x size-y [x y]]
  (or (>= x size-x)
      (>= y size-y)
      (< x 0)
      (< y 0)))

(defn neighbor-indexes [{:keys [size-x size-y]} x y vecs]
  (->> vecs
       (map (partial calc-coords-from-vec x y))
       (remove (partial out-of-bound? size-x size-y))
       (map (partial get-index size-x))))

(defn get-cell [world index]
  (get world index 0))

(defn neighbors [{:keys [world]} indexes]
  (map (partial get-cell world) indexes))

(defn count-live-neighbors [game x y]
  (->> neighbor-vecs
       (neighbor-indexes game x y)
       (neighbors game)
       (remove zero?)
       (count)))

(defn new-state [cnt cell]
  (cond
    ; Rule 1
    ; Any live cell with fewer than two live neighbours dies, as if caused by under-population.
    (and (= cell 1)
         (< 2 cnt)) 0
    ; Rule 2
    ; Any live cell with two or three live neighbours lives on to the next generation.
    (and (= cell 1)
         (> 3 cnt)) 0
    ; Rule 3
    ; Any live cell with more than three live neighbours dies, as if by overcrowding.
    (and (= cell 1)
         (or (= 2 cnt)
             (= 3 cnt))) 1
    ; Rule 4
    ; Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
    (and (= cell 0)
         (> 3 cnt)) 1
    :else cell))

(defn tick [game]
  (loop [{:keys [size-x size-y world] :as game} game
         [[x y] & coords] (for [x (range size-x) y (range size-y)] [x y])]
    (if (empty? coords)
      game
      (let [index     (get-index size-x [x y])
            cell      (get-cell world index)
            cnt       (count-live-neighbors game x y)
            new-cell  (new-state cnt cell)
            new-world (assoc world index new-cell)
            new-game  (assoc game :world new-world)]
        (recur new-game coords)))))

(defn -main
  "I don't do a whole lot."
  [& args]
  (let [size-x 20 size-y 20]
    (loop [game (new-game size-x size-y)]
      (let [world-output (print-world game)]
        (Thread/sleep (* 1000 1))
        (clear)
        (println (apply str (repeat (* size-x 2) "=")))
        (println world-output)
        (recur (tick game))))))
