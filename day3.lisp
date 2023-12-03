(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/3

(defun d3-data ()
  (declare (optimize (speed 3)))
  (let ((schematic ())
        (tail ())
        (width 0)
        (height 0))
    (declare (type (unsigned-byte 32) width height))
    (do-file (line "day3.txt")
      (unless (< 0 width)
        (setf width (length line)))
      (loop with y = height
            for x from 0 below width
            collect (char line x) into row
            finally (cond
                      (schematic
                       (setf (cdr tail) (list row))
                       (setf tail (cdr tail)))
                      (T
                       (setf schematic (list row))
                       (setf tail schematic))))
      (incf height))
    (values (make-array (list height width) :element-type 'character :initial-contents schematic)
            width height)))

(declaim (inline d3-node))
(defun d3-node (x y schematic width height)
  (when (and (<= 0 x) (< x width) (<= 0 y) (< y height))
    (aref schematic y x)))

(defun d3p1 ()
  (multiple-value-bind (schematic width height) (d3-data)
    (labels ((node (x y) (d3-node x y schematic width height))
             (symbol-node-p (x y)
               (let ((char (node x y)))
                 (and char (not (or (digit-char-p char) (char= char #\.)))))))
      (let ((nums ()))
        (dotimes (y height (apply #'+ nums))
          (loop for x = 0 then (+ x num-width)
                while (< x width)
                for char = (or (node x y) (error "Missing character: ~d,~d" x y))
                for num-width = (if (digit-char-p char)
                                    (loop for w from 1
                                          for next = (node (+ x w) y)
                                          while (and next (digit-char-p next))
                                          finally (return w))
                                    1)
                when (and (digit-char-p char)
                          (loop with has-symbol-p = NIL
                                until has-symbol-p
                                for dy from -1 upto 1
                                do (loop for dx from -1 upto num-width
                                         until has-symbol-p
                                         do (setf has-symbol-p (symbol-node-p (+ x dx) (+ y dy))))
                                finally (return has-symbol-p)))
                collect (loop for i from 0 below num-width
                              collecting (node (+ x i) y) into num
                              finally (return (parse-integer (format NIL "~{~c~}" num))))
                into row-nums
                finally (setf nums (nconc nums row-nums))))))))

;; Answer: 517021

(defun d3p2 ()
  (multiple-value-bind (schematic width height) (d3-data)
    (labels ((node (x y) (d3-node x y schematic width height))
             (num-at (x y)
               (when (digit-char-p (node x y))
                 (let ((num-start (loop for x0 = (1- x) then (1- x0)
                                        for prev = (node x0 y)
                                        while (and prev (digit-char-p prev))
                                        finally (return (1+ x0))))
                       (num-end (loop for x0 from (1+ x)
                                      for next = (node x0 y)
                                      while (and next (digit-char-p next))
                                      finally (return x0))))
                   (loop for x0 from num-start below num-end
                         collect (node x0 y) into num
                         finally (return (values (parse-integer (format NIL "~{~c~}" num))
                                                 num-end)))))))
      (let ((gears ()))
        (dotimes (y height (apply #'+ gears))
          (dotimes (x width)
            (when (char= #\* (or (node x y) (error "Missing character: ~d,~d" x y)))
              (loop with gear-nums = ()
                    for y0 from (1- y) upto (1+ y)
                    do (loop for x0 = (1- x) then (if num (1+ num-end) (1+ x0))
                             until (< (1+ x) x0)
                             for (num num-end) = (multiple-value-list (num-at x0 y0))
                             when num do (push num gear-nums))
                    finally (when (< 1 (length gear-nums))
                              (push (apply #'* gear-nums) gears))))))))))

;; Answer: 81296995
