(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/5

(defparameter *d5-keys* '(:seed :soil :fertilizer :water :light :temperature :humidity :location))

(defun d5-data ()
  (with-local-file (stream "day5.txt")
    (let ((num-re (cl-ppcre:create-scanner "\\b\\d+\\b"))
          (map-name-re (cl-ppcre:create-scanner "^(\\w+)-to-(\\w+) map:$"))
          (almanac (make-hash-table :test 'equal))
          (seeds-line (read-clean-line stream)))
      (unless (cl-ppcre:scan-to-strings "^seeds:( \\d+)+$" seeds-line)
        (error "Invalid seeds line: ~a" seeds-line))
      (flet ((parse-nums (line)
               (or (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings num-re line))
                   (error "Invalid number line: ~a" line)))
             (parse-map-name (line)
               (multiple-value-bind (match groups)
                   (cl-ppcre:scan-to-strings map-name-re line)
                 (unless (and match groups) (error "Invalid map name line: ~a" line))
                 (cons (intern (format NIL "~:@(~a~)" (svref groups 0)) :keyword)
                       (intern (format NIL "~:@(~a~)" (svref groups 1)) :keyword)))))
        ;; The seeds line is uniquely on the same row as the hash-table slot name.
        (setf (gethash :seeds almanac) (parse-nums seeds-line))
        (loop with cur-map = NIL
              with cur-map-nums = ()
              with tail = ()
              for line = (read-clean-line stream)
              until (eql line :eof)
              do (cond ;; Clear the current map at an empty line.
                   ((= 0 (length line)) (setf cur-map NIL))
                   (cur-map ;; Fill the current map  with the numbers.
                    (cond
                      (cur-map-nums ;; Push to end.
                       (setf (cdr tail) (list (parse-nums line)))
                       (setf tail (cdr tail)))
                      (T ;; New numbers.
                       (setf cur-map-nums (list (parse-nums line)))
                       (setf tail cur-map-nums)))
                    (setf (gethash cur-map almanac) cur-map-nums))
                   (T ;; Expect a new map named after an empty line.
                    (setf cur-map (parse-map-name line))
                    (setf cur-map-nums ())
                    (setf tail ())))
              finally (return almanac))))))

(defun d5-convert (value from to almanac)
  (loop for keys on (loop for key-start on *d5-keys*
                          until (eql from (car key-start))
                          finally (return key-start))
        for cur = (car keys)
        for next = (cadr keys)
        for prev-value = value then cur-value
        for cur-value = (loop with key = `(,cur . ,next)
                              for (to-value from-start from-count) in (gethash key almanac)
                              for match-p = (and (<= from-start prev-value)
                                                 (< prev-value (+ from-start from-count)))
                              until match-p
                              finally (return (if match-p
                                                  (+ to-value (- prev-value from-start))
                                                  prev-value)))
        until (eql to next)
        finally (return cur-value)))

(defun d5p1 ()
  (loop with almanac = (d5-data)
        for seed in (gethash :seeds almanac)
        minimizing (d5-convert seed :seed :location almanac)))

;; Answer: 31599214

(defun d5p2 ()
  ;; FIXME: This is terrible and takes forever.
  (loop with almanac = (d5-data)
        with seeds = (gethash :seeds almanac)
        for (seed-start seed-count) on seeds by #'cddr
        minimizing (loop for i from 0 below seed-count
                         for seed = (+ i seed-start)
                         minimizing (d5-convert seed :seed :location almanac))))

;; Answer: 20358599
