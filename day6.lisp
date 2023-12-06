(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/6

(defun d6-data (&optional ignore-ws-p)
  (with-local-file (stream "day6.txt")
    (let ((races (cons NIL NIL))
          (num-re (cl-ppcre:create-scanner "\\b\\d+\\b"))
          (line (read-clean-line stream)))
      (flet ((parse-line (line)
               (let ((nums (cl-ppcre:all-matches-as-strings num-re line)))
                 (if ignore-ws-p
                     (parse-integer (format NIL "~{~d~}" nums))
                     (mapcar #'parse-integer nums)))))
        (unless (and (stringp line) (string= "Time:" (subseq line 0 5)))
          (error "Invalid times line: ~a" line))
        (setf (car races) (parse-line line))
        (setf line (read-clean-line stream))
        (unless (and (stringp line) (string= "Distance:" (subseq line 0 9)))
          (error "Invalid distance line: ~a" line))
        (setf (cdr races) (parse-line line))
        (setf line (read-clean-line stream))
        (unless (eql line :eof) (error "Unexpected content: ~a" line))
        races))))

(declaim (inline d6-distance))
(defun d6-distance (hold limit)
  (* hold (max 0 (- limit hold))))

(defun d6p1 ()
  (loop with (times . distances) = (d6-data)
        with retval = 1
        for time in times
        for distance in distances
        for min-hold = time
        for max-hold = 0
        for found-p = nil
        do (dotimes (i time)
             (let ((dist (d6-distance i time)))
               (cond
                 ((and found-p (< dist distance))
                  (return))
                 ((< distance dist)
                  (when (< i min-hold) (setf min-hold i))
                  (setf max-hold i)
                  (setf found-p t)))))
        do (setf retval (* retval (1+ (- max-hold min-hold))))
        finally (return retval)))

;; Answer: 840336

(defun d6-bsearch (high-p base top target limit)
  (loop with left = base
        with right = top
        for hold = (floor (+ left right) 2)
        for distance = (d6-distance hold limit)
        do (cond
             ((< distance target) (setf left (1+ hold)))
             ((< target distance) (setf right (1- hold))))
        until (or (= distance target) (< right left))
        finally (return
                  (if (< target distance)
                      (loop for value = hold then (if high-p (1+ value) (1- value))
                            while (< target (d6-distance value limit))
                            finally (return (if high-p (1- value) (1+ value))))
                      (loop for value = hold then (if high-p (1- value) (1+ value))
                            until (< target (d6-distance value limit))
                            finally (return value))))))

(defun d6p2 ()
  (let* ((time-distance (d6-data T))
         (time (car time-distance))
         (target (cdr time-distance))
         (half (floor time 2)))
    ;; Note: +1 because the upper edge is inclusive.
    (- (1+ (d6-bsearch T (1+ half) time target time))
       (d6-bsearch NIL 0 half target time))))

;; Answer: 41382569
