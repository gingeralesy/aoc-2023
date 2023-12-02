(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/2

(defparameter *d2-parser* (cl-ppcre:create-scanner "^Game (\\d+):(.+)$"))
(defparameter *d2-delim* (cl-ppcre:create-scanner ";"))
(defparameter *d2-result-re* (cl-ppcre:create-scanner "(\\d+) (red|green|blue)"))

(defun d2-data ()
  (declare (optimize (speed 3)))
  (let ((games ()))
    (do-file (line "day2.txt" (nreverse games))
      (multiple-value-bind (match groups)
          (cl-ppcre:scan-to-strings *d2-parser* line)
        (unless (and match groups) (error "Invalid line: ~a" line))
        (loop with game-id = (parse-integer (svref groups 0))
              for result in (cl-ppcre:split *d2-delim* (svref groups 1))
              collect (loop for cubes in (cl-ppcre:all-matches-as-strings *d2-result-re* result)
                            for cube-groups =
                            (or (nth-value 1 (cl-ppcre:scan-to-strings *d2-result-re* cubes))
                                (error "Invalid result: ~a" cubes))
                            for color = (intern (format NIL "~:@(~a~)" (svref cube-groups 1))
                                                :keyword)
                            collect (cons color (parse-integer (svref cube-groups 0))))
              into game
              finally (push (nconc (list game-id) game) games))))))

(defun d2p1 ()
  (loop for (game-id . rounds) in (d2-data)
        for valid-p = T
        do (dolist (round rounds)
             (loop while valid-p
                   for (color . count) in round
                   do (ecase color
                        (:red (when (< 12 count) (setf valid-p NIL)))
                        (:green (when (< 13 count) (setf valid-p NIL)))
                        (:blue (when (< 14 count) (setf valid-p NIL)))))
             (unless valid-p (return)))
        when valid-p sum game-id))

;; Answer: 2169

(defun d2p2 ()
  (loop for (game-id . rounds) in (d2-data)
        for max-red = 0
        for max-green = 0
        for max-blue = 0
        do (dolist (round rounds)
             (loop for (color . count) in round
                   do (ecase color
                        (:red (when (< max-red count) (setf max-red count)))
                        (:green (when (< max-green count) (setf max-green count)))
                        (:blue (when (< max-blue count) (setf max-blue count))))))
        sum (* max-red max-green max-blue)))

;; Answer: 60948
