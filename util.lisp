(in-package #:aoc-2023)

(deftype input-line () '(or (and keyword (eql :eof)) (simple-array character (*))))

(defparameter *clean-re* (cl-ppcre:create-scanner "^(.*\\S)?\\s*$"))

(defun local-file (filename &key error)
  (declare (optimize (speed 3)))
  (let ((file (asdf:system-relative-pathname :aoc-2023 filename)))
    (when (and error (not (probe-file file)))
      (error "Missing file: ~a" filename))
    file))

(defun clean-line (line)
  (declare (optimize (speed 3)))
  (if (stringp line)
      (multiple-value-bind (match groups)
          (cl-ppcre:scan-to-strings *clean-re* line)
        (declare (type (or null (simple-array T (*))) groups))
        (or (and match groups (aref groups 0)) ""))
      line))

(declaim (inline u32 u64))
(defun u32 (value) (logand #xffffffff value))
(defun u64 (value) (logand #xffffffffffffffff value))

(defmacro profile-run (&body body)
  (let ((start-var (gensym "START")))
    `(let ((,start-var (local-time:now)))
       (prog1 (progn ,@body)
         (format T "~&Run took ~3$ sec.~%"
                 (local-time:timestamp-difference (local-time:now) ,start-var))))))
