(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/8

(defun d9-data (reverse-p)
  (let ((num-re (cl-ppcre:create-scanner "-?\\b\\d+\\b"))
        (oasis (make-queue)))
    (do-file (line "day9.txt" (queue-as-list oasis))
      (loop with reverse-nums = ()
            for value in (cl-ppcre:all-matches-as-strings num-re line)
            for num = (parse-integer value)
            do (push num reverse-nums) ;; Reversed order.
            collect num into nums
            finally (queue-push (if reverse-p reverse-nums nums) oasis)))))

(defun d9-create-report (readings reversed-p)
  (declare (optimize (speed 3)))
  (loop with all-zero-p = T
        for next of-type (signed-byte 32) in (if reversed-p readings (rest readings))
        for prev of-type (signed-byte 32) in (if reversed-p (rest readings) readings)
        for diff = (- next prev)
        collect diff into diffs
        unless (zerop diff) do (setf all-zero-p NIL)
        finally (return ;; Reverse order so that zeroes are top on report.
                  (nconc (if all-zero-p (list diffs) (d9-create-report diffs reversed-p))
                         (list readings)))))

(defun d9-extrapolate (report subtract-p &optional (count 1))
  (declare (type (unsigned-byte 8) count))
  (declare (optimize (speed 3)))
  (let ((last-val 0))
    (declare (type (signed-byte 32) last-val))
    (dotimes (i count (values report last-val))
      (push 0 (first report))
      (loop for prev in report
            for prev-val of-type (signed-byte 32) = (first prev)
            for next on (rest report)
            for next-val of-type (signed-byte 32) = (first (car next))
            for val = (if subtract-p (- next-val prev-val) (+ prev-val next-val))
            do (push val (car next))
            finally (setf last-val val)))))

(defun d9p1 ()
  (loop for readings in (d9-data T)
        for report = (d9-create-report readings T)
        sum (nth-value 1 (d9-extrapolate report NIL))))

;; Answer: 1992273652

(defun d9p2 ()
  (loop for readings in (d9-data NIL)
        for report = (d9-create-report readings NIL)
        for value = (nth-value 1 (d9-extrapolate report T))
        sum value))
