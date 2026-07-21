;;; date-functions.el --- Some functions to work with dates         -*- lexical-binding: t; -*-

;; Copyright (C) 2026 M. Rincón

;; Author: M. Rincón
;; Keywords: calendar dates
;; URL: https://github.com/foxfriday/date-functions
;; Package-Requires: ((emacs "24.3"))
;; Version: 0.0.4

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; A collection of functions useful for working with dates.

;;; Code:
(require 'calendar)

(defgroup date-functions nil
  "Functions to work with dates and holiday calendars."
  :group 'calendar)

(defconst df-monday 1 "Value for Monday.")
(defconst df-tuesday 2 "Value for Tuesday.")
(defconst df-wednesday 3 "Value for Wednesday.")
(defconst df-thursday 4 "Value for Thursday.")
(defconst df-friday 5 "Value for Friday.")
(defconst df-saturday 6 "Value for Saturday.")
(defconst df-sunday 0 "Value for Sunday.")

(defun df-good-friday (year)
  "Return Good Friday on YEAR."
  (let* ((a (mod year 19))
         (b (/ year 100))
         (c (mod year 100))
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

(defun df-is-weekend (_y _m _d wd)
  "Return non-nil if weekday WD falls on a weekend."
  (or (= wd df-saturday) (= wd df-sunday)))

(defun df-is-new-years (_y m d wd)
  "Return non-nil if M D with weekday WD is the observed New Year's Day."
  (or (and (= m 1) (or (= d 1) (and (= d 2) (= wd df-monday)))) (and (= d 31) (= m 12) (= wd df-friday))))

(defun df-is-mlk (y m d wd)
  "Return non-nil if Y M D with weekday WD is Martin Luther King Jr. Day."
  (and (= m 1) (>= d 15) (<= d 21) (= wd df-monday) (>= y 1986)))

(defun df-is-presidents-day (_y m d wd)
  "Return non-nil if M D with weekday WD is Presidents' Day."
  (and (= m 2) (>= d 15) (<= d 21) (= wd df-monday)))

(defun df-is-good-friday (y m d wd)
  "Return non-nil if Y M D with weekday WD is Good Friday."
  (cond ((not (= wd df-friday))
         nil)
        ((not (or (and (= m 3) (> d 19)) (and (= m 4) (< d 24))))
         nil)
        (t
         (let* ((gf (df-good-friday y))
                (month (nth 1 gf))
                (day (nth 2 gf)))
           (and (= month m) (= day d))))))

(defun df-is-memorial-day (_y m d wd)
  "Return non-nil if M D with weekday WD is Memorial Day."
  (and (>= d 25) (= wd df-monday) (= m 5)))

(defun df-is-juneteenth-day (y m d wd)
  "Return non-nil if Y M D with weekday WD is the observed Juneteenth."
  (and (= m 6) (or (= d 19) (and (= d 20) (= wd df-monday)) (and (= d 18) (= wd df-friday))) (>= y 2021)))

(defun df-is-independence-day (_y m d wd)
  "Return non-nil if M D with weekday WD is the observed Independence Day."
  (and (= m 7) (or (= d 4) (and (= d 5) (= wd df-monday)) (and (= d 3) (= wd df-friday)))))

(defun df-is-labor-day (_y m d wd)
  "Return non-nil if M D with weekday WD is Labor Day."
  (and (<= d 7) (= wd df-monday) (= m 9)))

(defun df-is-columbus-day (_y m d wd)
  "Return non-nil if M D with weekday WD is Columbus Day."
  (and (= m 10) (>= d 8) (<= d 14) (= wd df-monday)))

(defun df-is-veterans-day (_y m d wd)
  "Return non-nil if M D with weekday WD is the observed Veterans Day."
  (and (= m 11) (or (= d 11) (and (= d 12) (= wd df-monday)) (and (= d 10) (= wd df-friday)))))

(defun df-is-thanksgiving-day (_y m d wd)
  "Return non-nil if M D with weekday WD is Thanksgiving Day."
  (and (= m 11) (>= d 22) (<= d 28) (= wd df-thursday)))

(defun df-is-thanksgiving-friday (_y m d wd)
  "Return non-nil if M D with weekday WD is the day after Thanksgiving."
  (and (= m 11) (>= d 23) (<= d 29) (= wd df-friday)))

(defun df-is-christmas-day (_y m d wd)
  "Return non-nil if M D with weekday WD is the observed Christmas Day."
  (and (= m 12) (or (= d 25) (and (= d 26) (= wd df-monday)) (and (= d 24) (= wd df-friday)))))

(defcustom df-us-federal-holidays (list #'df-is-weekend
                                        #'df-is-mlk
                                        #'df-is-presidents-day
                                        #'df-is-memorial-day
                                        #'df-is-juneteenth-day
                                        #'df-is-independence-day
                                        #'df-is-labor-day
                                        #'df-is-columbus-day
                                        #'df-is-veterans-day
                                        #'df-is-thanksgiving-day
                                        #'df-is-christmas-day
                                        #'df-is-new-years)
  "List of observed federal holidays in the U.S."
  :type '(repeat function))

(defcustom df-globex-holidays (list #'df-is-weekend
                                    #'df-is-mlk
                                    #'df-is-presidents-day
                                    #'df-is-good-friday
                                    #'df-is-memorial-day
                                    #'df-is-juneteenth-day
                                    #'df-is-independence-day
                                    #'df-is-labor-day
                                    #'df-is-thanksgiving-day
                                    #'df-is-christmas-day
                                    #'df-is-new-years)
  "List of observed Globex holidays."
  :type '(repeat function))

(defcustom df-ice-otc-holidays (list #'df-is-weekend
                                     #'df-is-mlk
                                     #'df-is-presidents-day
                                     #'df-is-good-friday
                                     #'df-is-memorial-day
                                     #'df-is-juneteenth-day
                                     #'df-is-independence-day
                                     #'df-is-labor-day
                                     #'df-is-veterans-day
                                     #'df-is-thanksgiving-day
                                     #'df-is-thanksgiving-friday
                                     #'df-is-christmas-day
                                     #'df-is-new-years)
  "List of observed ICE OTC holidays."
  :type '(repeat function))

(defcustom df-default-holidays df-us-federal-holidays
  "List of holidays to use as default."
  :type '(repeat function))

(defun df-day-of-week (y m d)
  "Return the day of the week on Y M D as a number, with Sunday as 0."
  (calendar-day-of-week (list m d y)))

(defun df-month-last-day (y m)
  "Return the last day in M of Y."
  (cond ((= m 2)
         (if (df-is-leap-year y) 29 28))
        ((or (= m 4) (= m 6) (= m 9) (= m 11))
         30)
        (t
         31)))

(defun df-is-holiday (y m d &optional holidays weekday)
  "Return non-nil if Y M D is a holiday according to optional HOLIDAYS.
The value returned is the matching predicate. WEEKDAY is optional
and expedites the calculation."
  (let* ((hdl (or holidays df-default-holidays))
         (wd (or weekday (df-day-of-week y m d))))
    (catch 'is-hd
      (dolist (hd hdl)
        (if (funcall hd y m d wd)
            (throw 'is-hd hd)))
      nil)))

(defun df-prior-weekday (n)
  "Return the prior day of the week from N or nil."
  (when n
    (mod (- n 1) 7)))

(defun df-next-weekday (n)
  "Return the next day of the week from N or nil."
  (when n
    (mod (+ n 1) 7)))

(defun df-prior-day (y m d &optional weekday)
  "Return the date before Y M D. If provided, move the WEEKDAY too."
  (let ((wd (df-prior-weekday weekday)))
    (cond ((> d 1)
           (list y m (- d 1) wd))
          ((= m 1)
           (list (- y 1) 12 31 wd))
          (t
           (list y (- m 1) (df-month-last-day y (- m 1)) wd)))))

(defun df-next-day (y m d &optional weekday)
  "Return the date after Y M D. If provided, move the WEEKDAY too."
  (let ((wd (df-next-weekday weekday)))
    (cond ((< d (df-month-last-day y m))
           (list y m (+ d 1) wd))
          ((= m 12)
           (list (+ y 1) 1 1 wd))
          (t
           (list y (+ m 1) 1 wd)))))

(defun df-move-work-day (y m d n &optional holidays wdstart)
  "Return date N work days from Y M D based on a list of HOLIDAYS.
If WDSTART is true, start the count from nearest business day. By
default, use U.S. federal holidays."
  (let* ((mover (if (< n 0) #'df-prior-day #'df-next-day))
         (i 0)
         (wd (df-day-of-week y m d))
         (dt (list y m d wd))
         (total (if (and wdstart (df-is-holiday y m d holidays wd)) (+ (abs n) 1) (abs n))))
    (while (< i total)
      (setq dt (funcall mover (nth 0 dt) (nth 1 dt) (nth 2 dt) (nth 3 dt)))
      (unless (df-is-holiday (nth 0 dt) (nth 1 dt) (nth 2 dt) holidays (nth 3 dt))
        (setq i (+ i 1))))
    dt))

;; Calendar-mode passes around these dynamically bound variables, "which
;; unfortunately have rather common names. They are meant to be available for
;; external functions, so the names can't be changed."
(defvar date)
(defvar entry)

;;;###autoload
(defun df-diary-expirations (anchor n &optional holidays wdstart mark)
  "Add date to diary N business days from the ANCHOR calendar day.
If ANCHOR is zero or negative, the N offset is done from the first
day of the following month. Use a optional HOLIDAYS calendar or the
default federal calendar from the United States. If WDSTART is true,
move to the next work day before doing the offset. The MARK sets the
face for the calendar."
  (let* ((y1 (calendar-extract-year date))
         (m1 (calendar-extract-month date))
         (d1 (calendar-extract-day date))
         (dd (if (<= anchor 0) 1 anchor))
         (mm (if (<= anchor 0) (if (= m1 12) 1 (+ m1 1)) m1))
         (yy (if (and (<= anchor 0) (= m1 12)) (+ y1 1) y1)))
    (let ((dt (df-move-work-day yy mm dd n holidays wdstart)))
      (if (and (= d1 (nth 2 dt)) (= m1 (nth 1 dt)) (= y1 (nth 0 dt)))
          (cons mark entry)))))

;;;###autoload
(defun df-diary-from-month (month n &optional holidays wdstart mark)
  "Add date to diary N business days from the start of the MONTH.
Use a optional HOLIDAYS calendar or the default federal calendar from
the United States. If WDSTART is true, move to the next work day before
doing the offset. The MARK sets the face for the calendar."
  (let* ((y1 (calendar-extract-year date))
         (m1 (calendar-extract-month date))
         (d1 (calendar-extract-day date))
         (yy (cond ((and (< n 0) (> m1 month)) (+ y1 1))
                   ((and (> n 0) (< m1 month)) (- y1 1))
                   (t y1))))
    (let ((dt (df-move-work-day yy month 1 n holidays wdstart)))
      (if (and (= d1 (nth 2 dt)) (= m1 (nth 1 dt)) (= y1 (nth 0 dt)))
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
