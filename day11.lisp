(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/11

(defun d11-data (&optional (padding-size 2))
  (let ((width 0)
        (height 0)
        (count 0)
        (rows (make-hash-table))
        (cols (make-hash-table))
        (galaxies (make-queue)))
    (do-file (line "day11.txt" galaxies)
      (unless (< 0 width) (setf width (length line)))
      (loop for i from 0 below width
            for char = (char line i)
            do (ecase char
                 (#\#
                  (let ((galaxy (upoint i height)))
                    (incf count)
                    (queue-push galaxy galaxies)
                    (push galaxy (gethash i cols))
                    (push galaxy (gethash height rows))))
                 (#\.))
            finally (incf height)))
    (loop with padding = 0
          for i from 0 below (+ width padding)
          for galaxies = (gethash i cols)
          do (if galaxies
                 (loop for galaxy in galaxies do (incf (upoint-x galaxy) padding))
                 (incf padding (1- padding-size)))
          finally (incf width padding))
    (loop with padding = 0
          for i from 0 below (+ height padding)
          for galaxies = (gethash i rows)
          do (if galaxies
                 (loop for galaxy in galaxies do (incf (upoint-y galaxy) padding))
                 (incf padding (1- padding-size)))
          finally (incf height padding))
    (values (make-array count :element-type 'upoint :initial-contents (queue-as-list galaxies))
            count width height)))

(defun d11p1 ()
  (multiple-value-bind (galaxies count) (d11-data)
    (loop for i from 0 below count
          for galaxy-a = (aref galaxies i)
          sum (loop for j from (1+ i) below count
                    for galaxy-b = (aref galaxies j)
                    sum (+ (abs (- (upoint-x galaxy-a) (upoint-x galaxy-b)))
                           (abs (- (upoint-y galaxy-a) (upoint-y galaxy-b))))))))

;; Answer: 10033566

(defun d11p2 ()
  (multiple-value-bind (galaxies count) (d11-data 1000000)
    (loop for i from 0 below count
          for galaxy-a = (aref galaxies i)
          sum (loop for j from (1+ i) below count
                    for galaxy-b = (aref galaxies j)
                    sum (+ (abs (- (upoint-x galaxy-a) (upoint-x galaxy-b)))
                           (abs (- (upoint-y galaxy-a) (upoint-y galaxy-b))))))))

;; Answer: 560822911938
