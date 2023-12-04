(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/4

(defun d4-data (count)
  (flet ((make-lookup-array (numbers)
           (let ((array (make-array 100 :element-type 'bit :initial-element 0)))
             (dolist (num numbers array)
               (setf (bit array num) 1)))))
    (let ((cards (make-array (1+ count) :element-type 'cons :initial-element (cons NIL NIL)))
          (num-re (cl-ppcre:create-scanner "\\b\\d+\\b")))
      (do-file (line "day4.txt" cards)
        ;; Example line: "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
        (destructuring-bind (name numbers) (cl-ppcre:split ":" line)
          (destructuring-bind (win-nums own-nums) (cl-ppcre:split "\\|" numbers)
            (let ((id (parse-integer (cl-ppcre:scan-to-strings num-re name)))
                  (card
                    (cons
                     (make-lookup-array
                      (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings num-re win-nums)))
                     (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings num-re own-nums)))))
              (setf (aref cards id) card))))))))

()

(defun d4p1 (&optional (count 213))
  (loop with cards = (d4-data count)
        for card-id from 1 upto count
        for (lookup . numbers) = (aref cards card-id)
        for value = 0
        do (dolist (num numbers)
             (unless (zerop (bit lookup num))
               (setf value (if (< 0 value) (* 2 value) 1))))
        sum value))

;; Answer: 26914

(defun d4p2 (&optional (count 213))
  (loop with total = count
        with cards = (d4-data count)
        with hand = (loop for i from 1 upto count collecting i)
        while hand
        for card-id = (pop hand)
        for (lookup . numbers) = (aref cards card-id)
        for win-count = 0
        do (dolist (num numbers)
             (unless (zerop (bit lookup num))
               (incf win-count)
               (let ((new (+ card-id win-count)))
                 (unless (< count new)
                   (incf total)
                   (push new hand)))))
        finally (return total)))
