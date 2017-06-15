(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board [x y :as coord]]
  (get-in board coord))

(defn has-value? [board coord]
  (boolean (all-values (value-at board coord))))
;  (> (value-at board coord) 0))

(defn row-values [board [row _]]
  (set (get board row)))
;  (reduce conj #{} (get board row)))

(defn col-values [board [_ col]]
  (set (map #(get % col) board)))
;  (loop [acc 0
;         col-vals #{}]
;    (if (= acc 9)
;      col-vals
;      (recur (inc acc)
;             (conj col-vals (get-in board [acc col]))))))

(defn coord-pairs [coords]
  (apply vector (for [row coords
                      col coords]
                  (vector row col))))

(defn block-coords [[row col]]
  (let [r0 (- row (rem row 3)) ; tai (quot row 3)
        c0 (- col (rem col 3))]
    (map #(vector (+ r0 (first %)) (+ c0 (second %))) (coord-pairs [0 1 2])))) ; (map #(map + [r0 c0] %))
(block-coords [0 2])

(defn block-values [board coord]
  (loop [coords (block-coords coord)
         block-vals #{}]
    (if (empty? coords)
      block-vals
      (recur (rest coords)
             (conj block-vals (get-in board (first coords)))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [row-vals (row-values board coord)
          col-vals (col-values board coord)
          block-vals (block-values board coord)]
      (set/difference all-values (set/union row-vals col-vals block-vals)))))

(defn row-filled? [row]
  (every? all-values row))
; (defn filled? [board]
;   (every? row-filled? board))

(defn filled? [board]
  (let [all-coord-pairs (coord-pairs (apply vector (range 0 9)))]
    (reduce #(and %1 (has-value? board %2)) true all-coord-pairs)))

(defn rows [board]
  (let [row-nums (range 0 9)]
    (reduce #(conj %1 (row-values board [%2 0])) [] row-nums)))
; (map set board))

; validates single sequence
(defn valid-seq? [a-seq]
  (and (= (count a-seq) 9)
       (= (apply min a-seq) 1)
       (= (apply max a-seq) 9)))
; validates list of sequencies
(defn valid-seqs? [seqs]
  (reduce #(and %1 (valid-seq? %2)) true seqs))

(defn valid-rows? [board]
  (valid-seqs? (rows board)))
; (every? #(= all-values %) (rows board))

(defn cols [board]
  (let [col-nums (range 0 9)]
    (reduce #(conj %1 (col-values board [0 %2])) [] col-nums)))

(defn valid-cols? [board]
  (valid-seqs? (cols board)))

(defn blocks [board]
  (let [all-blocks (coord-pairs [0 3 6])]
    (reduce #(conj %1 (block-values board %2)) [] all-blocks)))

(defn valid-blocks? [board]
  (valid-seqs? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [my-coord-pairs (coord-pairs (apply vector (range 0 9)))]
    (cond (empty? my-coord-pairs) nil
      ((complement has-value?) board (first my-coord-pairs)) (first my-coord-pairs)
      :else (recur (rest my-coord-pairs)))))

;[[5 3 0 | 0 7 0 | 0 0 0]
; [6 0 0 | 1 9 5 | 0 0 0]
; [0 9 8 | 0 0 0 | 0 6 0]
; -------+-------+-------
; [8 0 0 | 0 6 0 | 0 0 3]
; [4 0 0 | 8 0 3 | 0 0 1]
; [7 0 0 | 0 2 0 | 0 0 6]
; -------+-------+-------
; [0 6 0 | 0 0 0 | 2 8 0]
; [0 0 0 | 4 1 9 | 0 0 5]
; [0 0 0 | 0 8 0 | 0 7 9]]

(defn solve-helper [board]
  (if (valid-solution? board)
    board
    (if-let [empty-point (find-empty-point board)]
      (for [valid-value (valid-values-for board empty-point)
            solution (solve-helper (set-value-at board empty-point valid-value))]
        solution))))

(defn solve [board]
  (solve-helper board))
