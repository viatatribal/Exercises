;; If the numbers 1 to 5 are written out in words:
;; one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19
;; letters used in total.
;;
;; If all the numbers from 1 to 1000 (one thousand) inclusive
;; were written out in words, how many letters would be used?
;;
;; NOTE: Do not count spaces or hyphens. For example,
;; 342 (three hundred and forty-two) contains 23 letters
;; and 115 (one hundred and fifteen) contains 20 letters.
;; The use of "and" when writing out numbers is in compliance
;; with British usage.

(ns problem17)

(defn ones-count [n]
  (nth (list 0 3 3 5 4 4 3 5 5 4) n))

(defn teens-count [n]
  (nth (list 3 6 6 8 8 7 7 9 8 8) n))

(defn tenths-count [n]
  (nth (list 0 3 6 6 5 5 5 7 6 6) n))

(defn hundreths-count [n]
  (+ 7 (ones-count n)))

(defn numToLetterCount [n]
  (cond
    (= n 1000) 11
    (>= n 100) (+ (hundreths-count (quot n 100))
                  (numToLetterCount (rem n 100)))
    (>= n 20) (+ (tenths-count (quot n 10))
                 (numToLetterCount (rem n 10)))
    (>= n 10) (teens-count (rem n 10))
    :else (ones-count n))) ; n < 10

;; there are 99 'and' in 100, 99 'and' in 200, ...,
;; 99 'and' in 900. Thus, we have 9 * 99 'and'
(+ (* 9 99 3) ;; number of 'and'
     (reduce
      +
      (map
       #(numToLetterCount %)
       (drop 1 (range 1001)))))


