(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/14

"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."

(declaim (inline d14-num->char d14-char->num))
(defun d14-num->char (num) (ecase num (0 #\.) (1 #\#) (2 #\O)))
(defun d14-char->num (char) (ecase char (#\. 0) (#\# 1)(#\O 2)))

(deftype d14-tile () '(integer 0 2))

(defstruct (d14-platform (:constructor %make-d14-platform (map width height)))
  (hash "" :type (simple-array character (*)))
  (map NIL :type (simple-array d14-tile (* *)))
  (width 0 :type (unsigned-byte 32))
  (height 0 :type (unsigned-byte 32))
  (load 0 :type (unsigned-byte 64)))

(defmacro with-d14-platform ((map width height &optional load hash) platform &body body)
  (let ((platform-var (gensym "PLATFORM"))
        (load-var (or load (gensym "LOAD")))
        (hash-var (or hash (gensym "HASH"))))
    `(let* ((,platform-var ,platform)
            (,map (d14-platform-map ,platform-var))
            (,width (d14-platform-width ,platform-var))
            (,height (d14-platform-height ,platform-var))
            (,load-var (d14-platform-load ,platform-var))
            (,hash-var (d14-platform-hash ,platform-var)))
       (declare (type (unsigned-byte 64) ,load-var))
       (declare (ignorable ,load-var ,hash-var))
       ,@body)))

(defun make-d14-platform (rocks width height)
  (loop with map = (make-array `(,height ,width) :element-type 'd14-tile :initial-element 0)
        with plat = (%make-d14-platform map width height)
        with load = 0
        with hash = (make-string (* width height))
        for row in rocks
        for y from 0
        do (loop for tile in row
                 for x from 0
                 do (setf (aref map y x) tile)
                 when (= tile 2)
                 do (incf (d14-platform-load plat) (- height y))
                 do (setf (char hash (+ x (* y width))) (d14-num->char tile)))
        finally (return (prog1 plat (setf (d14-platform-hash plat) hash)))))

(defmethod d14-platform-tilt ((plat d14-platform) direction)
  (declare (optimize (speed 3)))
  (with-d14-platform (map width height load hash) plat
    (labels ((update-hash (x y)
               (setf (char hash (+ x (* y width))) (d14-num->char (aref map y x))))
             (swap (from-x from-y to-x to-y)
               (let ((tmp (aref map from-y from-x)))
                 (setf (aref map from-y from-x) (aref map to-y to-x))
                 (setf (aref map to-y to-x) tmp)
                 (setf load (u64 (+ load (- from-y to-y))))
                 (update-hash from-x from-y)
                 (update-hash to-x to-y))))
      (ecase direction
        (:north
         (loop for from-y from 1 below height
               do (dotimes (x width)
                    (when (= (aref map from-y x) 2)
                      (loop with move-p = NIL
                            for y from (1- from-y) downto 0
                            while (= 0 (aref map y x))
                            minimize y into target
                            do (setf move-p T)
                            finally (when move-p (swap x from-y x target)))))))
        (:south
         (loop for from-y from (- height 2) downto 0
               do (dotimes (x width)
                    (when (= (aref map from-y x) 2)
                      (loop with move-p = NIL
                            for y from (1+ from-y) below height
                            while (= 0 (aref map y x))
                            maximize y into target
                            do (setf move-p T)
                            finally (when move-p (swap x from-y x target)))))))
        (:west
         (loop for from-x from 1 below width
               do (dotimes (y height)
                    (when (= (aref map y from-x) 2)
                      (loop with move-p = NIL
                            for x from (1- from-x) downto 0
                            while (= 0 (aref map y x))
                            minimize x into target
                            do (setf move-p T)
                            finally (when move-p (swap from-x y target y)))))))
        (:east
         (loop for from-x from (- width 2) downto 0
               do (dotimes (y height)
                    (when (= (aref map y from-x) 2)
                      (loop with move-p = NIL
                            for x from (1+ from-x) below width
                            while (= 0 (aref map y x))
                            maximize x into target
                            do (setf move-p T)
                            finally (when move-p (swap from-x y target y)))))))))
    (setf (d14-platform-load plat) load)
    plat))

(defmethod d14-platform-to-string ((plat d14-platform))
  (with-d14-platform (map width height) plat
    (let ((str (make-string (1- (* height (1+ width))))))
      (dotimes (y height str)
        (dotimes (x width)
          (setf (char str (+ x (* y (1+ width)))) (d14-num->char (aref map y x))))
        (when (< y (1- height))
          (setf (char str (+ width (* y (1+ width)))) #\Newline))))))

(defun d14-data ()
  (let ((rocks (make-queue))
        (width 0)
        (height 0))
    (do-file (line "day14.txt" (make-d14-platform (queue-as-list rocks) width height))
      (loop for char across line
            when (zerop width) count char into x
            collect (d14-char->num char) into row
            finally (progn
                      (incf height)
                      (queue-push row rocks)
                      (when (< width x) (setf width x)))))))

(defun d14p1 ()
  (d14-platform-load (d14-platform-tilt (d14-data) :north)))

;; Answer: 108759

(defun d14p2 (&optional (n 1000000000) (cycle '(:north :west :south :east)))
  (let* ((plat (d14-data))
         (index-cache (make-hash-table :test 'equal))
         (step-cache (make-hash-table :test 'equal))
         (load-cache (make-hash-table :test 'equal))
         (prev (copy-array (d14-platform-hash plat))))
    (setf (gethash prev index-cache) 0)
    (setf (gethash prev load-cache) (d14-platform-load plat))
    (dotimes (i n (error "Failed to find the loop."))
      (let ((current (gethash prev step-cache)))
        (when current
          ;; I feel like there's a math error here somewhere but it works. Oh well.
          (let* ((start (gethash current index-cache))
                 (end (1+ (gethash prev index-cache)))
                 ;; 1+ to start to not include it and 1+ to remainder to make get the final step.
                 (remainder (1+ (mod (- n (1+ start)) (- end start))))
                 (next current))
            ;; And then find the one that we'd end at.
            (dotimes (j remainder) (setf next (gethash next step-cache)))
            (return (gethash next load-cache))))
        (unless current
          (dolist (dir cycle) (d14-platform-tilt plat dir))
          (setf current (copy-array (d14-platform-hash plat)))
          (setf (gethash prev step-cache) current)
          (setf (gethash current load-cache) (d14-platform-load plat))
          (setf (gethash current index-cache) (1+ (gethash prev index-cache))))
        (setf prev current)))))

;; Answer: 89089
