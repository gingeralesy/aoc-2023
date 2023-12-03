(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/3

(defun d3-data ()
  (let ((num-re (cl-ppcre:create-scanner "\\b\\d+\\b"))
        (sym-re (cl-ppcre:create-scanner "[^\\d\\.]"))
        (schematic ())
        (tail ()))
    (do-file (line "day3.txt" schematic)
      (let ((row (list line)))
        (loop for prev = 0 then end
              for (start end) = (multiple-value-list (cl-ppcre:scan num-re line :start prev))
              while (and start end)
              collect (cons start end) into nums
              finally (push nums row))
        (loop for prev = 0 then end
              for (start end) = (multiple-value-list (cl-ppcre:scan sym-re line :start prev))
              while (and start end)
              collect start into syms
              finally (push syms row))
        (cond
          (schematic
           (setf (cdr tail) (list row))
           (setf tail (cdr tail)))
          (T
           (setf schematic (list row))
           (setf tail schematic)))))))

(defmacro do-d3-schematic (((prev-syms-var prev-nums-var prev-line-var)
                            (curr-syms-var curr-nums-var curr-line-var)
                            (next-syms-var next-nums-var next-line-var)
                            schematic &optional return-value)
                           &body body)
  (let ((prev-var (gensym "PREV"))
        (curr-var (gensym "CURR")))
    `(loop for ,prev-var = NIL then ,curr-var
           for ,curr-var on ,schematic
           for (,prev-syms-var ,prev-nums-var ,prev-line-var) = (car ,prev-var)
           for (,curr-syms-var ,curr-nums-var ,curr-line-var) = (car ,curr-var)
           for (,next-syms-var ,next-nums-var ,next-line-var) = (cadr ,curr-var)
           do (progn ,@body)
           finally (return ,return-value))))

(defun d3p1 ()
  (let ((schematic (d3-data))
        (sum 0))
    (flet ((has-sym-p (start end syms)
             (dolist (sym syms)
               (when (and (<= start sym) (< sym end))
                 (return T)))))
      (do-d3-schematic ((prev-syms prev-nums prev-line)
                        (curr-syms curr-nums curr-line)
                        (next-syms next-nums next-line)
                        schematic sum)
        (loop for (start-num . end-num) in curr-nums
              when (or (has-sym-p (1- start-num) (1+ end-num) prev-syms)
                       (has-sym-p (1- start-num) (1+ end-num) curr-syms)
                       (has-sym-p (1- start-num) (1+ end-num) next-syms))
              do (incf sum (parse-integer (subseq curr-line start-num end-num))))))))

;; Answer: 517021

(defun d3p2 ()
  (let ((schematic (d3-data))
        (sum 0))
    (flet ((nums-for (x nums line)
             (loop for (start . end) in nums
                   when (and (<= start (1+ x)) (<= x end))
                   collect (parse-integer (subseq line start end)))))
      (do-d3-schematic ((prev-syms prev-nums prev-line)
                        (curr-syms curr-nums curr-line)
                        (next-syms next-nums next-line)
                        schematic sum)
        (loop for sym in curr-syms
              when (char= #\* (char curr-line sym))
              do (let ((nums (nconc (nums-for sym prev-nums prev-line)
                                    (nums-for sym curr-nums curr-line)
                                    (nums-for sym next-nums next-line))))
                   (when (< 1 (length nums))
                     (incf sum (apply #'* nums)))))))))

;; Answer: 81296995
