;; You are given the following information, but you may prefer to
;; do some research for yourself.
;;
;;    1 Jan 1900 was a Monday.
;;    Thirty days has September,
;;    April, June and November.
;;    All the rest have thirty-one,
;;    Saving February alone,
;;    Which has twenty-eight, rain or shine.
;;    And on leap years, twenty-nine.
;;    A leap year occurs on any year evenly divisible by 4, but not on
;;    a century unless it is divisible by 400.
;;
;; How many Sundays fell on the first of the month during the
;; twentieth century (1 Jan 1901 to 31 Dec 2000)?

(ns problem19)

;; first day of each month
(def first-of-month
  '(1 32 60 91 121 152 182 213 244 274 305 335))

;; first day of each month in a leap year
(def first-of-month-leap
  '(1 32 61 92 122 153 183 214 245 275 306 336))

;; get all sundays for normal year
(defn sundays [n]
  (if (> n 365)
    ()
    (cons n (sundays (+ n 7)))))

;; get all sundays for leap year
(defn sundays-leap [n]
  (if (> n 366)
    ()
    (cons n (sundays-leap (+ n 7)))))

;; get the number of sundays that start
;; at the first day of the month for a given year
(defn sum-of-sundays [days]
  (count
   (filter
   #(some (fn [n] (= n %)) first-of-month)
   days)))

(defn answer [n start-day sum]
  (cond (= n 2001) sum
        (zero? (/ n 4))
        (let [days (sundays-leap start-day)]
          (answer (inc n)
                  (- 7 (- 366 (last days)))
                          (+ sum (sum-of-sundays days))))
        :else (let [days (sundays start-day)]
                (answer (inc n)
                        (- 7 (- 365 (last days)))
                        (+ sum (sum-of-sundays days))))))

;; 6 jan 1901 -> Sunday
(answer 1901 6 0)
