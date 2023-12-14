(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/13

(defun d13-data ()
  (with-local-file (stream "day13.txt")
    (loop with maps = (make-queue)
          with map = (make-queue)
          with width = 0
          with height = 0
          with map-count = 0
          for line of-type input-line = (read-clean-line stream)
          unless line do (error "Unexpected error reading file: day13.txt")
          do (cond
               ((or (eql line :eof) (zerop (length line)))
                (when (< 0 (queue-length map))
                  (queue-push (make-array `(,height ,width) :element-type 'boolean
                                                            :initial-contents (queue-as-list map))
                              maps)
                  (setf map (make-queue))
                  (setf width 0)
                  (setf height 0)
                  (incf map-count)))
               (T
                (incf height)
                (setf width (length line))
                (loop for char across line
                      collect (ecase char (#\. NIL) (#\# T)) into row
                      finally (queue-push row map))))
          until (eql line :eof)
          finally (return (make-array map-count :initial-contents (queue-as-list maps))))))

(defun d13-row-mirrored-at-p (map at row)
  (loop for i from at downto 0
        for j from (1+ at) below (array-dimension map 1)
        unless (eql (aref map row i) (aref map row j)) do (return NIL)
        finally (return T)))

(defun d13-row-mirrors (map row &optional min max)
  (loop for i from (or min 0) below (if max (1+ max) (1- (array-dimension map 1)))
        when (d13-row-mirrored-at-p map i row) collect i))

(defun d13-col-mirrored-at-p (map at col)
  (loop for i from at downto 0
        for j from (1+ at) below (array-dimension map 0)
        unless (eql (aref map i col) (aref map j col)) do (return NIL)
        finally (return T)))

(defun d13-col-mirrors (map col &optional min max)
  (loop for i from (or min 0) below (if max (1+ max) (1- (array-dimension map 0)))
        when (d13-col-mirrored-at-p map i col) collect i))

(defun d13p1 ()
  (flet ((find-horizontal-mirroring (map)
           (loop for row from 0 below (array-dimension map 0)
                 for row-mirrors = (d13-row-mirrors map row)
                 then (loop with new = (d13-row-mirrors
                                        map row (first row-mirrors) (car (last row-mirrors)))
                            for value in new
                            when (find value row-mirrors :test #'=) collect value)
                 while row-mirrors
                 finally (return (when (= 1 (length row-mirrors)) (1+ (car row-mirrors))))))
         (find-vertical-mirroring (map)
           (loop for col from 0 below (array-dimension map 1)
                 for col-mirrors = (d13-col-mirrors map col)
                 then (loop with new = (d13-col-mirrors
                                        map col (first col-mirrors) (car (last col-mirrors)))
                            for value in new
                            when (find value col-mirrors :test #'=) collect value)
                 while col-mirrors
                 finally (return (when (= 1 (length col-mirrors)) (1+ (car col-mirrors)))))))
    (loop for map across (d13-data)
          sum (or (find-horizontal-mirroring map)
                  (* 100 (or (find-vertical-mirroring map)
                             (error "Bad map: ~a" map)))))))

;; Answer: 29213

;; TODO: Part 2. Done similar to part one, except when one row or column has only one fitting answer
;;       while others have two you pick the other one. If there are more than two, switch blocks on
;;       the off row until one of the other matches hit. (Don't forget that the original match might
;;       still be the correct one.)
