(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/15

(defun d15-hash (str)
  (declare (type (simple-array character (*)) str))
  (declare (optimize (speed 3)))
  (loop with hash of-type (unsigned-byte 8) = 0
        for char across str
        for code = (char-code char)
        do (setf hash (mod (* (+ hash code) 17) #x100))
        finally (return hash)))

(defun d15p1 ()
  (let ((delim (cl-ppcre:create-scanner ",")))
    (with-local-file (stream "day15.txt")
      (let ((line (read-clean-line stream)))
        (unless (eql :eof (read-clean-line stream))
          (error "Multiple lines detected."))
        (loop for str in (cl-ppcre:split delim line)
              sum (d15-hash str))))))

;; Answer: 507769

(defun d15p2 ()
  (loop with op-re = (cl-ppcre:create-scanner "^(\\w+)(-|=(\\d+))$")
        with boxes = (make-array #x100 :initial-contents (loop repeat #x100 collect (make-queue)))
        with line = (with-local-file (stream "day15.txt")
                      (prog1 (read-clean-line stream)
                        (unless (eql :eof (read-clean-line stream))
                          (error "Multiple lines detected."))))
        for str in (cl-ppcre:split "," line)
        for groups = (or (nth-value 1 (cl-ppcre:scan-to-strings op-re str))
                         (error "Invalid operation: ~a" str))
        for key = (svref groups 0)
        for box = (svref boxes (d15-hash key))
        do (ecase (char (svref groups 1) 0)
             (#\=
              (let ((slot (queue-find key box :key #'car :test #'string=))
                    (value (parse-integer (svref groups 2))))
                (if slot
                    (setf (cdr slot) value)
                    (queue-push (cons key value) box))))
             (#\- (queue-remove key box :key #'car :test #'string=)))
        finally (return (loop for box across boxes
                              counting box into i
                              sum (loop for lens in (queue-as-list box)
                                        counting lens into j
                                        sum (* i j (cdr lens)))))))

;; Answer: 269747
