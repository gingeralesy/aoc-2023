(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/12

(deftype d12-spring () '(and keyword (or (eql :unknown) (eql :broken) (eql :fixed))))

(defstruct (d12-springs (:constructor make-d12-springs (set-count spring-count sets springs)))
  (set-count 0 :type (unsigned-byte 8))
  (spring-count 0 :type (unsigned-byte 8))
  (solution-count 0 :type (unsigned-byte 8))
  (sets NIL :type simple-vector) ;; Known consecutive sets of broken springs.
  (springs NIL :type simple-vector) ;; Known range of fixed and broken springs.
  (solutions ;; Possible locations of fixed and broken springs.
   (make-array 16 :element-type 'simple-vector :initial-element #() :adjustable T :fill-pointer 0)
   :type (vector T)))

(defun d12-data ()
  (declare (optimize (speed 3)))
  (let ((spring-sets (make-queue))
        (num-re (cl-ppcre:create-scanner "\\b\\d+\\b")))
    (do-file (line "day12.txt" (let ((count (queue-length spring-sets)))
                                 (values
                                  (make-array count :element-type 'd12-springs
                                                    :initial-contents (queue-as-list spring-sets))
                                  count)))
      (loop with (springs-line nums-line) = (or (cl-ppcre:split " " line)
                                                (error "Invalid line: ~a" line))
            with nums = (mapcar #'parse-integer
                                (or (cl-ppcre:all-matches-as-strings num-re nums-line)
                                    (error "Invalid line: ~a" line)))
            with set-count = (length nums)
            for char across (or (the (or null (simple-array character (*))) springs-line)
                                (error "Invalid line: ~a" line))
            until (eql char #\Space)
            collect (ecase char (#\? :unknown) (#\# :broken) (#\. :fixed)) into springs
            count char into spring-count
            finally (queue-push
                     (make-d12-springs
                      set-count spring-count
                      (make-array set-count :initial-contents nums)
                      (make-array spring-count :initial-contents springs))
                     spring-sets)))))

(defun d12-valid-count (start set-ix sets set-count springs spring-count &optional (folds 1))
 (labels ((valid-at-p (set at springs spring-count)
            (and (<= (+ at set) spring-count)
                 (or (zerop at) (not (eql (svref springs (mod (1- at) (length springs))) :broken)))
                 (or (= (+ at set) spring-count)
                     (not (eql (svref springs (mod (+ at set) (length springs))) :broken)))
                 (dotimes (i set T)
                   (when (eql (svref springs (mod (+ i at) (length springs))) :fixed)
                     (return NIL)))))
          (valid-for (set from springs spring-count)
            (loop for i from from below spring-count
                  for valid-p = (valid-at-p set i springs spring-count)
                  when (eql (svref springs (mod i (length springs))) :broken)
                  do (return (if valid-p (nconc valid (list i)) valid))
                  when (and (eql (svref springs (mod i (length springs))) :unknown) valid-p)
                  collect i into valid
                  finally (return valid))))
   (let* ((spring-count (* spring-count folds))
          (set-count (* set-count folds))
          (set (svref sets (mod set-ix (length sets))))
          (valids (valid-for set start springs spring-count))
          (next (1+ set-ix)))
     (if (< next set-count)
         (loop for valid in valids ;; Recurse.
               for from = (+ valid set 1)
               sum (d12-valid-count from next sets set-count springs spring-count))
         (loop for valid in valids
               for from = (+ valid set 1)
               unless (and (< from spring-count)
                           (or (< from (* (1- folds) (length springs)))
                               (find :broken springs
                                     :start (mod from (length springs)))))
               count valid)))))

(defun d12p1 ()
  (let ((spring-sets (d12-data)))
    (loop for spring-set across spring-sets
          for set-count = (d12-springs-set-count spring-set)
          for spring-count = (d12-springs-spring-count spring-set)
          for sets = (d12-springs-sets spring-set)
          for springs = (d12-springs-springs spring-set)
          sum (d12-valid-count 0 0 sets set-count springs spring-count))))

;; Answer: 7460

(defun d12p2 ()
  (let ((spring-sets (d12-data)))
    (format T "~&")
    (unwind-protect
         (loop for spring-set across spring-sets
               for set-count = (d12-springs-set-count spring-set)
               for spring-count = (d12-springs-spring-count spring-set)
               for sets = (d12-springs-sets spring-set)
               for springs = (d12-springs-springs spring-set)
               do (format T ".")
               sum (d12-valid-count 0 0 sets set-count springs spring-count 5))
      (format T "~%"))))
