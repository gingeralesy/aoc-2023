(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/5

(defparameter *d5-keys* #(:seed :soil :fertilizer :water :light :temperature :humidity :location))

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
                       (setf tail cur-map-nums)
                       (setf (gethash cur-map almanac) cur-map-nums))))
                   (T ;; Expect a new map named after an empty line.
                    (setf cur-map (parse-map-name line))
                    (setf cur-map-nums ())
                    (setf tail ())))
              finally (return almanac))))))

(defun d5-convert (value from to almanac)
  (loop with keys of-type (simple-vector 8) = *d5-keys*
        with from-index = (or (position from keys) (error "Invalid key: ~a" from))
        with to-index = (or (position to keys) (error "Invalid key: ~a" to))
        with reverse-p = (< to-index from-index)
        with delta = (if reverse-p -1 +1)
        for index = (+ from-index delta) then (+ index delta)
        until (if reverse-p (< index to-index) (< to-index index))
        for prev = (svref keys from-index) then next
        for next = (svref keys index)
        for key = (if reverse-p `(,next . ,prev) `(,prev . ,next))
        for prev-value = value then next-value
        for next-value = (loop for (value-a value-b count) in (gethash key almanac)
                               for to-value = (if reverse-p value-b value-a)
                               for from-start = (if reverse-p value-a value-b)
                               for match-p = (and (<= from-start prev-value)
                                                  (< prev-value (+ from-start count)))
                               until match-p
                               finally (return (if match-p
                                                   (+ to-value (- prev-value from-start))
                                                   prev-value)))
        until (eql to next)
        finally (return next-value)))

;; (defun d5-flatten (almanac)
;;   (flet ((merge (start end)
;;            (let (())))))
;;   (loop with keys of-type (simple-vector 8) = *d5-keys*
;;         with start = (svref keys 0)
;;         for index from 1 below 7
;;         for mid = (svref keys index)
;;         for next = (svref keys (1+ index))
;;         do (merge (gethash `(,start . ,mid) almanac) (gethash `(,mid . ,next) almanac))))

(defun d5p1 ()
  (loop with almanac = (d5-data)
        for seed in (gethash :seeds almanac)
        minimizing (d5-convert seed :seed :location almanac)))

;; Answer: 31599214

(defun d5p2 ()
  ;; FIXME: This is still brute-force.
  (loop with almanac = (d5-data)
        with seeds = (gethash :seeds almanac)
        for location from 0
        for seed = (d5-convert location :location :seed almanac)
        until (loop for (seed-start seed-count) on seeds by #'cddr
                    for found-p = (and (<= seed-start seed) (< seed (+ seed-start seed-count)))
                    until found-p
                    finally (return found-p))
        finally (return location)))

;; Answer: 20358599
