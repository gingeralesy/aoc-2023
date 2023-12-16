(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/16

(defparameter *d16-energy-cache* (make-hash-table :test 'equal))

(defun d16-data ()
  (let ((map (make-queue))
        (width 0)
        (height 0))
    (do-file (line "day16.txt" (values (make-array `(,height ,width)
                                                   :initial-contents (queue-as-list map)
                                                   :element-type '(integer 0 4))
                                       width height))
      (when (zerop width) (setf width (length line)))
      (incf height)
      (loop for char across line
            collect (ecase char (#\. 0) (#\\ 1) (#\/ 2) (#\- 3) (#\| 4)) into row
            finally (queue-push row map)))))

(declaim (inline d16-dir-value))
(defun d16-dir-value (dir)
  (ecase dir (:north 1) (:south 2) (:west 4) (:east 8)))

(defun d16-move (map beam)
  (destructuring-bind (x y dir) beam
    (ecase dir
      (:north
       (ecase (aref map y x)
         ((0 4) ;; . |
          (setf (second beam) (1- y)))
         ((1 3) ;; \ -
          (setf (first beam) (1- x))
          (setf (third beam) :west))
         (2 ;; /
          (setf (first beam) (1+ x))
          (setf (third beam) :east))))
      (:south
       (ecase (aref map y x)
         ((0 4) ;; . |
          (setf (second beam) (1+ y)))
         ((1 3) ;; \ -
          (setf (first beam) (1+ x))
          (setf (third beam) :east))
         (2 ;; /
          (setf (first beam) (1- x))
          (setf (third beam) :west))))
      (:east
       (ecase (aref map y x)
         ((0 3) ;; . -
          (setf (first beam) (1+ x)))
         ((2 4) ;; / |
          (setf (second beam) (1- y))
          (setf (third beam) :north))
         (1 ;; \
          (setf (second beam) (1+ y))
          (setf (third beam) :south))))
      (:west
       (ecase (aref map y x)
         ((0 3) ;; . -
          (setf (first beam) (1- x)))
         ((2 4) ;; / |
          (setf (second beam) (1+ y))
          (setf (third beam) :south))
         (1 ;; \
          (setf (second beam) (1- y))
          (setf (third beam) :north)))))
    beam))

;; Below are optimisations that aren't necessary, as the runtime is below 0.1 seconds anyways.
;; TODO: The proper way to do part 2 is to cache every tile that gets energised when the beam hits
;;       an object in the map to a direction, cache that, and then use that to quickly get the final
;;       results on further iterations.
;; TODO: Recursively run this whenever something is collided with and add the returned energy map to
;;       the current energy map.

(defun d16-energise (direction start-x start-y map width height)
  ;; On map 0 is empty, 1 in \ mirror, 2 is / mirror, 3 is - splitter, 4 is | splitter.
  (let ((energised (make-array `(,height ,width) :element-type '(unsigned-byte 16)
                                                 :initial-element 0)))
    ;; On energy map 0 is empty, 1 is northway, 2 is southway, 4 is westway, 8 is eastway.
    (setf (aref energised start-y start-x) (d16-dir-value direction))
    (loop with beams = (list (list start-x start-y direction))
          while beams
          for beam = (d16-move map (pop beams))
          for (x y dir) = beam
          for dir-value = (d16-dir-value dir)
          do (when (and (<= 0 x) (<= 0 y) (< x width) (< y height)
                        (zerop (logand (aref energised y x) dir-value)))
               (setf (aref energised y x) (logior (aref energised y x) dir-value))
               (push beam beams)
               (ecase dir ;; Split the beam. They always turn left at splitter.
                 (:north (when (= (aref map y x) 3) (push (list x y :south) beams)))
                 (:south (when (= (aref map y x) 3) (push (list x y :north) beams)))
                 (:west (when (= (aref map y x) 4) (push (list x y :east) beams)))
                 (:east (when (= (aref map y x) 4) (push (list x y :west) beams))))))
    (loop for y from 0 below height
          sum (loop for x from 0 below width
                    unless (zerop (aref energised y x)) count x))))

(defun d16p1 ()
  (multiple-value-bind (map width height) (d16-data)
    (d16-energise :east 0 0 map width height)))

;; Answer: 6605

(defun d16p2 ()
  (multiple-value-bind (map width height) (d16-data)
    (loop for start from 0 below (max width height)
          maximize (max (d16-energise :east 0 start map width height)
                        (d16-energise :west (1- width) start map width height)
                        (d16-energise :south start 0 map width height)
                        (d16-energise :north start (1- height) map width height)))))

;; Answer: 6766
