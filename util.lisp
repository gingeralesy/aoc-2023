(in-package #:aoc-2023)

(deftype input-line () '(or (and keyword (eql :eof)) (simple-array character (*))))

(defparameter *clean-re* (cl-ppcre:create-scanner "^(.*\\S?)\\s*$"))

(defun local-file (filename &key error)
  (declare (optimize (speed 3)))
  (let ((file (asdf:system-relative-pathname :aoc-2023 filename)))
    (when (and error (not (probe-file file)))
      (error "Missing file: ~a" filename))
    file))

(defmacro with-local-file ((stream-var filename) &body body)
  (let ((filename-var (gensym "FILENAME"))
        (file-var (gensym "FILE")))
    `(let* ((,filename-var ,filename)
            (,file-var (local-file ,filename-var :error T)))
       (with-open-file (,stream-var ,file-var :direction :input
                                              :element-type 'character
                                              :if-does-not-exist :error
                                              :external-format :utf-8)
         ,@body))))

(defun clean-line (line)
  (declare (optimize (speed 3)))
  (if (stringp line)
      (multiple-value-bind (match groups)
          (cl-ppcre:scan-to-strings *clean-re* line)
        (declare (type (or null (simple-array T (*))) groups))
        (or (and match groups (aref groups 0)) ""))
      line))

(declaim (inline read-clean-line))
(defun read-clean-line (stream)
  (declare (optimize (speed 3)))
  (let ((line (read-line stream NIL :eof)))
    (if (eql line :eof) line (clean-line line))))

(defmacro do-file ((line-var filename &optional return-value) &body body)
  (let ((stream-var (gensym "STREAM")))
    `(with-local-file (,stream-var ,filename)
       (loop for ,line-var of-type input-line = (read-clean-line ,stream-var)
             unless ,line-var do (error "Unexpected error reading file: ~a" ,filename)
             until (eql ,line-var :eof)
             do (progn ,@body)
             finally (return ,return-value)))))

(declaim (inline u32 u64))
(defun u32 (value) (logand #xffffffff value))
(defun u64 (value) (logand #xffffffffffffffff value))

(defmacro profile-run (&body body)
  (let ((start-var (gensym "START")))
    `(let ((,start-var (local-time:now)))
       (prog1 (progn ,@body)
         (format T "~&Run took ~3$ sec.~%"
                 (local-time:timestamp-difference (local-time:now) ,start-var))))))

(declaim (inline quadratic))
(defun quadratic (a b c)
  (declare (type (signed-byte 64) a b c))
  (let* ((a (coerce a 'double-float))
         (b (coerce b 'double-float))
         (c (coerce c 'double-float))
         (sqrt (sqrt (- (* b b) (* 4.0d0 a c)))))
    (values (/ (- b sqrt) (* 2.0d0 a)) (/ (+ b sqrt) (* 2.0d0 a)))))
