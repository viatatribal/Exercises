;; 2^15 = 32768 and the sum of its digits is
;; 3 + 2 + 7 + 6 + 8 = 26
;; What is the sum of the digits of the number
;; 2^1000?

(ns problem17)

(defn p2 [n]
  (if (= n 1)
    2
    (*' 2 (p2 (dec n)))))

(defn answer [n]
  (reduce
   +
   (map (fn [n]
          (- (int n) 48))
        (seq
         (str (p2 n))))))


(answer 1000)
