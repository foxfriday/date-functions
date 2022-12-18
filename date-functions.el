;;; date-functions.el --- Some functions to work with dates         -*- lexical-binding: t; -*-

;; Copyright (C) 2022 M. Rincón

;; Author: M. Rincón
;; Keywords: calendar dates
;; Version: 0.0.1

;;; Commentary:
;; A collection of functions useful for working with dates.

;;; Code:
(require 'calendar)

(defvar df-monday 1 "Value for Monday.")
(defvar df-tuesday 2 "Value for Tuesday.")
(defvar df-wednesday 3 "Value for Wednesday.")
(defvar df-thursday 4 "Value for Thursday.")
(defvar df-friday 5 "Value for Friday.")
(defvar df-saturday 6 "Value for Saturday.")
(defvar df-sunday 0 "Value for Sunday.")

(defun df-good-friday (year)
  "Return Good Friday on YEAR."
  (let* ((Y year)
         (a (mod Y 19))
         (b (/ Y 100))
         (c (mod Y 100))
         (d (/ b 4))
         (e (mod b 4))
         (f (/ (+ b 8) 25))
         (g (/ (+ 1 (- b f)) 3))
         (h (mod (- (+ (* 19 a) b 15) d g) 30))
         (i (/ c 4))
         (k (mod c 4))
         (l (mod (- (+ 32 (* 2 e) (* 2 i)) h k) 7))
         (m (/ (+ a (* 11 h) (* 22 l)) 451))
         (month (/ (- (+ h l 114) (* 7 m)) 31))
         (day (+ (mod (- (+ h l 114) (* 7 m)) 31) 1)))
    (cond ((= day 2)
           (list year (- month 1) 31))
          ((= day 1)
           (list year (- month 1) 30))
          (t
           (list year month (- day 2))))))

(defun df-is-leap-year (year)
  "Return non-nil if YEAR is a leap year."
  (or (and (zerop (% year 4))
	   (not (zerop (% year 100))))
      (zerop (% year 400))))

(defun df-compare-dates (y1 m1 d1 y2 m2 d2)
  "Compares Y1 M1 D1 and Y2 M2 D2 and return their relative location."
  (cond ((< y1 y2) -1)
        ((and (= y1 y2) (< m1 m2)) -1)
        ((and (= y1 y2) (= m1 m2) (< d1 d2)) -1)
        ((and (= y1 y2) (= m1 m2) (= d1 d2)) 0)
        (t 1)))

(defun df-is-weekend (y m d wd)
  "Return non-nil if is named holiday on Y M D and weekday WD."
  (or (= wd df-saturday) (= wd df-sunday)))

(defun df-is-new-years (y m d wd)
  "Return non-nil if is named holiday on Y M D and weekday WD."
  (or (and (= m 1) (or (= d 1) (and (= d 2) (= wd df-monday)))) (and (= d 31) (= m 12) (= wd df-friday))))

(defun df-is-mlk (y m d wd)
  "Return non-nil if is named holiday on Y M D and weekday WD."
  (and (= m 1) (>= d 15) (<= d 21) (= wd df-monday) (>= y 1983)))

(defun df-is-presidents-day (y m d wd)
  "Return non-nil if is named holiday on Y M D and weekday WD."
  (and (= m 2) (>= d 15) (<= d 21) (= wd df-monday)))

(defun df-is-good-friday (y m d wd)
  "Return non-nil if is named holiday on Y M D and weekday WD."
  (cond ((not (= wd df-friday))
         nil)
        ((not (or (and (= m 3) (> d 19)) (and (= m 4) (< d 24))))
         nil)
        (t
         (let* ((gf (df-good-friday y))
                (month (nth 1 gf))
                (day (nth 2 gf)))
           (and (= month m) (= day d))))))

(defun df-is-memorial-day (y m d wd)
  "Return non-nil if is named holiday on Y M D and weekday WD."
  (and (>= d 25) (= wd df-monday) (= m 5)))

(defun df-is-juneteenth-day (y m d wd)
  "Return non-nil if is named holiday on Y M D and weekday WD."
  (and (= m 6) (or (= d 19) (and (= d 20) (= wd df-monday)) (and (= d 18) (= wd df-friday))) (> y 2021)))

(defun df-is-independence-day (y m d wd)
  "Return non-nil if is named holiday on Y M D and weekday WD."
  (and (= m 7) (or (= d 4) (and (= d 5) (= wd df-monday)) (and (= d 3) (= wd df-friday)))))

(defun df-is-labor-day (y m d wd)
  "Return non-nil if is named holiday on Y M D and weekday WD."
  (and (<= d 7) (= wd df-monday) (= m 9)))

(defun df-is-columbus-day (y m d wd)
  "Return non-nil if is named holiday on Y M D and weekday WD."
  (and (= m 10) (>= d 8) (<= d 14) (= wd df-monday)))

(defun df-is-veterans-day (y m d wd)
  "Return non-nil if is named holiday on Y M D and weekday WD."
  (and (= m 11) (or (= d 11) (and (= d 12) (= wd df-monday)) (and (= d 10) (= wd df-friday)))))

(defun df-is-thanksgiving-day (y m d wd)
  "Return non-nil if is named holiday on Y M D and weekday WD."
  (and (= m 11) (>= d 22) (<= d 28) (= wd df-thursday)))

(defun df-is-thanksgiving-friday (y m d wd)
  "Return non-nil if is named holiday on Y M D and weekday WD."
  (and (= m 11) (>= d 23) (<= d 29) (= wd df-friday)))

(defun df-is-christmas-day (y m d wd)
  "Return non-nil if is named holiday on Y M D and weekday WD."
  (and (= m 12) (or (= d 25) (and (= d 26) (= wd df-monday)) (and (= d 24) (= wd df-friday)))))

(defvar df-us-federal-holidays (list 'df-is-weekend
                                     'df-is-mlk
                                     'df-is-presidents-day
                                     'df-is-memorial-day
                                     'df-is-juneteenth-day
                                     'df-is-independence-day
                                     'df-is-labor-day
                                     'df-is-columbus-day
                                     'df-is-veterans-day
                                     'df-is-thanksgiving-day
                                     'df-is-christmas-day
                                     'df-is-new-years)
  "List of observed federal holidays in the U.S.")

(defvar df-globex-holidays (list 'df-is-weekend
                                 'df-is-mlk
                                 'df-is-presidents-day
                                 'df-is-good-friday
                                 'df-is-memorial-day
                                 'df-is-juneteenth-day
                                 'df-is-independence-day
                                 'df-is-labor-day
                                 'df-is-thanksgiving-day
                                 'df-is-christmas-day
                                 'df-is-new-years)
  "List of observed Globex holidays.")

(defvar df-ice-otc-holidays (list 'df-is-weekend
                                  'df-is-mlk
                                  'df-is-presidents-day
                                  'df-is-good-friday
                                  'df-is-memorial-day
                                  'df-is-juneteenth-day
                                  'df-is-independence-day
                                  'df-is-labor-day
                                  'df-is-veterans-day
                                  'df-is-thanksgiving-day
                                  'df-is-thanksgiving-friday
                                  'df-is-christmas-day
                                  'df-is-new-years)
  "List of observed ICE OTC holidays.")

(defvar df-default-holidays df-us-federal-holidays
  "List of holidays to use as default.")

(defun df-day-of-week (y m d)
  "Return the day name on Y M D."
  (let* ((wd (calendar-day-of-week (list m d y))))
    (cond ((= wd 0) df-sunday)
          ((= wd 1) df-monday)
          ((= wd 2) df-tuesday)
          ((= wd 3) df-wednesday)
          ((= wd 4) df-thursday)
          ((= wd 5) df-friday)
          ((= wd 6) df-saturday))))

(defun df-month-last-day (y m)
  "Return the last day in M of Y."
     (cond ((= m 2)
            (if (df-is-leap-year y) 29 28))
           ((or (= m 4) (= m 6) (= m 9) (= 11))
            30)
           (t
            31)))

(defun df-is-holiday (y m d &optional holidays weekday)
  "Return non-nil if Y M D is a holiday according optional HOLIDAYS.
WEEKDAY is optional and expedites the calculation."
  (let* ((hdl (if holidays holidays df-default-holidays))
         (wd (if weekday weekday (df-day-of-week y m d))))
    (catch 'is-hd
      (dolist (hd hdl)
        (if (funcall hd y m d wd)
            (throw 'is-hd 1)))
      nil)))

(defun df-prior-weekday (n)
  "Return the prior day of the week from N or nil."
  (when n
    (if (= n 0)
        6
      (- n 1))))

(defun df-next-weekday (n)
  "Return the next day of the week from N or nil."
  (when n
    (if (= n 6)
        0
      (+ n 1))))

(defun df-prior-day (y m d &optional weekday)
  "Return the date before Y M D. If provided, move the WEEKDAY too."
  (let ((wd (df-prior-weekday weekday)))
    (if (= d 1)
        (cond ((= m 1)
               (list (- y 1) 12 31 wd))
              ((= m 3)
               (if (df-is-leap-year y)
                   (list y 2 29 wd)
                 (list y 2 28 wd)))
              ((or (= m 2) (= m 4) (= m 6) (= m 8) (= m 9) (= m 11))
               (list y (- m 1) 31 wd))
              (t
               (list y (- m 1) 30 wd)))
      (list y m (- d 1) wd))))

(defun df-next-day (y m d &optional weekday)
  "Return the date after Y M D. If provided, move the WEEKDAY too."
  (let ((wd (df-next-weekday weekday)))
    (cond ((= d 31)
           (if (= m 12)
               (list (+ y 1) 1 1 wd)
             (list y (+ m 1) 1 wd)))
          ((and (= d 30) (or (= m 4) (= m 6) (= m 9) (= m 10)))
           (list y (+ m 1) 1 wd))
          ((and (= m 2) (or (= d 29) (and (= d 28) (not (df-is-leap-year y)))))
           (list y 3 1 wd))
        (t
         (list y m (+ d 1) wd)))))

(defun df-move-work-day (y m d n &optional holidays wdstart)
  "Return date N work days from Y M D based on a list of HOLIDAYS.
If WDSTART is true, start the count from nearest business day. By
default, use U.S. federal holidays."
  (let* ((mover (if (> n 0) #'df-next-day #'df-prior-day))
         (i 0)
         (wd (df-day-of-week y m d))
         (dt (list y m d wd))
         (N (if (and wdstart (df-is-holiday y m d holidays wd)) (+ (abs n) 1) (abs n))))
    (while (< i N)
      (setq dt (funcall mover (nth 0 dt) (nth 1 dt) (nth 2 dt) (nth 3 dt)))
      (unless (df-is-holiday (nth 0 dt) (nth 1 dt) (nth 2 dt) holidays (nth 3 dt))
        (setq i (+ i 1))))
    dt))

;; Calendar-mode passes around these dynamically bound variables, "which
;; unfortunately have rather common names. They are meant to be available for
;; external functions, so the names can't be changed." The result is that the
;; compiler will complain about this. I don't know how to fix this.
(defvar date)
(defvar entry)

;;;###autoload
(defun df-diary-expirations (anchor n &optional holidays wdstart mark)
  "Add date to diary N business days from the ANCHOR calendar day.
Use a optional HOLIDAYS calendar or the default federal calendar
from the United States. If WDSTART is true, move to a work day
before doing the offset. The MARK sets the face for the
calendar."
  (let* ((y1 (calendar-extract-year date))
         (m1 (calendar-extract-month date))
         (d1 (calendar-extract-day date)))
    (let ((dt (df-move-work-day y1 m1 anchor n holidays wdstart)))
      (if (= d1 (nth 2 dt))
          (cons mark entry)))))

;;;###autoload
(defun df-diary-work-week-start (&optional holidays mark)
  "Add start of work week to the calendar.
Use the HOLIDAYS calendar or the default federal calendar. Use
the MARK face for the calendar."
  (let* ((y1 (calendar-extract-year date))
         (m1 (calendar-extract-month date))
         (d1 (calendar-extract-day date))
         (wd (df-day-of-week y1 m1 d1)))
    (cond ((df-is-holiday y1 m1 d1 holidays wd)
           nil)
          ((= wd df-monday)
           (cons mark entry))
          (t
           (let* ((dt (df-move-work-day y1 m1 d1 -1 holidays))
                  (pr (df-day-of-week (nth 0 dt) (nth 1 dt) (nth 2 dt))))
             (if (<= wd pr)
                 (cons mark entry)))))))

(provide 'date-functions)
;;; date-functions.el ends here
