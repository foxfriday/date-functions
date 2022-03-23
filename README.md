# Date Functions #

This is a small library with some date utilities, mostly to determine if a given
date is a holiday and things like that. The date functions are for the United
States, but other holidays could be easily added. I use them primarily for
putting things on my calendar. I wouldn't really trust them for serious things.

## Examples ##

If you have a report that needs to be sent on the first work day of the week,
create a new function:

```lisp
(defun extra-diary-week-start (y m d)
  (let* ((y1 (calendar-extract-year date))
         (m1 (calendar-extract-month date))
         (d1 (calendar-extract-day date)))
    (if (and (> 0 (df-compare-dates y m d y1 m1 d1))
             (df-is-start-work-week y1 m1 d1))
        entry)))
```

Then you can add this line to your diary file -- assuming that the first
report needs to be sent on February 2, 2022:

```
%%(extra-diary-week-start 2022 2 1) My Meeting
```

Now suppose you want to be reminded of an event that occurs on the 5th business
day prior to the last day of the month, and you want to use a calendar other
than the U.S. Federal calendar. First, create a new function:

```lisp
(defun extra-diary-expiration (y m d)
    (let* ((y1 (calendar-extract-year date))
           (m1 (calendar-extract-month date))
           (d1 (calendar-extract-day date)))
      (if (and (> 0 (df-compare-dates y m d y1 m1 d1))
               (df-is-last-work-day y1 m1 d1 5 df-ice-otc-holidays))
          entry)))
```

Then, as before, add this line to your diary file:

 ```
%%(extra-diary-bidweek 2022 2 1) Bid Week Start
```
