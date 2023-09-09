;; A permutation is an ordered arrangement of objects. For example, 3124 is
;; one possible permutation of the digits 1, 2, 3 and 4. If all of the
;; permutations are listed numerically or alphabetically, we call it lexicographic order.
;; The lexicographic permutations of 0, 1 and 2 are:
;;
;; 012   021   102   120   201   210
;;
;; What is the millionth lexicographic permutation of the digits
;; 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

(ns problem24)

(def number [0 1 2 3 4 5 6 7 8 9])

(defn fact [n sum]
  (if (<= n 1)
    sum
  (fact (- n 1) (*' sum n))))

(defn max-multiple [n max index number]
  (cond (= (inc index) (count number)) index
        (>= (* n (inc index)) max) index
        :else (max-multiple n max (inc index) number)))

(defn answer [n f number]
  (if (or (<= n 0) (< f 0))
    ()
    (let [prod (fact f 1)
          index (max-multiple prod n 0 number)
          value (nth number index)
          numb (remove #(= value %) number)]
      (cons value (answer (- n (* index prod)) (dec f) numb)))))

(answer 1000000 9 number)
