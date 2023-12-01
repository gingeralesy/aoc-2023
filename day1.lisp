(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/1

(declaim (inline d1-char->number))
(defun d1-char->number (ch)
  (declare (optimize (speed 3)))
  (ecase ch
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)))

(declaim (inline d1-string->number))
(defun d1-string->number (str)
  (declare (type (simple-array character (*)) str))
  (declare (optimize (speed 3)))
  (alexandria:eswitch (str :test #'string=)
    ("1" 1)
    ("2" 2)
    ("3" 3)
    ("4" 4)
    ("5" 5)
    ("6" 6)
    ("7" 7)
    ("8" 8)
    ("9" 9)
    ("one" 1)
    ("two" 2)
    ("three" 3)
    ("four" 4)
    ("five" 5)
    ("six" 6)
    ("seven" 7)
    ("eight" 8)
    ("nine" 9)))

(defun d1p1 ()
  (declare (optimize (speed 3)))
  (let ((sum 0))
    (declare (type (unsigned-byte 32) sum))
    (do-file (line "day1.txt" sum)
      (let ((length (length line))
            first-digit last-digit)
        (dotimes (i (length line))
          (let ((ch-a (char line i))
                (ch-b (char line (- length i 1))))
            (when (and (null first-digit) (digit-char-p ch-a))
              (setf first-digit (d1-char->number ch-a)))
            (when (and (null last-digit) (digit-char-p ch-b))
              (setf last-digit (d1-char->number ch-b)))
            (when (and first-digit last-digit) (return))))
        (setf sum (+ sum (* 10 first-digit) last-digit))))))

;; Answer: 54597

(defun d1p2 ()
  (declare (optimize (speed 3)))
  (let ((sum 0)
        (start-scanner
          (cl-ppcre:create-scanner "(\\d|one|two|three|four|five|six|seven|eight|nine)"))
        (end-scanner
          (cl-ppcre:create-scanner "(enin|thgie|neves|xis|evif|ruof|eerht|owt|eno|\\d)")))
    (declare (type (unsigned-byte 32) sum))
    (do-file (line "day1.txt" sum)
      (setf sum (+ sum
                   (* 10 (d1-string->number (cl-ppcre:scan-to-strings start-scanner line)))
                   (d1-string->number
                    (reverse (cl-ppcre:scan-to-strings end-scanner (reverse line)))))))))

;; Answer: 54504
