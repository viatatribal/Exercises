;; n! means n * (n-1) * (n-2) * ... * 2 * 1
;; For example, 10! = 10 * 9 * ... * 2 * 1 = 3628800,
;; and the sum of the digits in the number 10! is
;; 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27
;; Find the sum of the digits in the number 100!

(ns problem20)

(defn fact [n res]
  (if (<= n 1)
    res
    (fact (dec n) (*' n res))))

(defn answer [n]
  (reduce
   +
   (map (fn [n]
          (- (int n) 48))
        (seq
         (str (fact n 1))))))


(answer 100)
