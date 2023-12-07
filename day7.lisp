(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/7

(defun d7-hand-type (hand)
  (let ((checked ()))
    (flet ((card-count (card)
             (loop for i from 0 below 5
                   when (or (= card (aref hand i)) (= 1 (aref hand i)))
                   count i))
           (find-card (card)
             (find card checked :key #'car :test #'=))
           (find-count (count)
             (find count checked :key #'cdr :test #'=)))
      (dotimes (i 5)
        (unless (find-card (aref hand i))
          (let ((count (card-count (aref hand i))))
            (push `(,(aref hand i) . ,count) checked))))
      (when (find-count 5) (return-from d7-hand-type 7)) ;; Five of a kind.
      (when (find-count 4) (return-from d7-hand-type 6)) ;; Four of a kind.
      (when (find-count 3)
        ;; If there are two jokers it cannot be a full because the rest of the cards are different.
        ;; If there is one joker we need a two cards with a count of three.
        (let ((joker-count (or (cdr (find-card 1)) 0))
              (two-cards (find-count 2)))
          (when (and (< joker-count 2)
                     (or (and (< joker-count 1) two-cards)
                         (loop for check in checked
                               when (= 3 (cdr check)) count check into threes
                               finally (return (= 2 threes)))))
            (return-from d7-hand-type 5))) ;; Full house.
        (return-from d7-hand-type 4)) ;; Three of a kind.
      (loop for check in checked
            when (= 2 (cdr check)) count check into pair-count
            finally (return (cond
                              ((= 2 pair-count) 3)
                              ((or (= 1 pair-count)
                                   (= 4 pair-count)) ;; One joker, no other pairs.
                               2)
                              ((= 0 pair-count) 1)
                              (T (error "No type found: ~a" hand))))))))

(defun d7-data (&optional jokers-p)
  (flet ((parse-hand (line)
           (loop with hand = (make-array 5 :element-type '(integer 0 15) :initial-element 0)
                 for i from 0 below 5
                 for char = (char line i)
                 do (setf (aref hand i)
                          (ecase (char-upcase char)
                            (#\2 2)
                            (#\3 3)
                            (#\4 4)
                            (#\5 5)
                            (#\6 6)
                            (#\7 7)
                            (#\8 8)
                            (#\9 9)
                            (#\T 10)
                            (#\J (if jokers-p 1 11))
                            (#\Q 12)
                            (#\K 13)
                            (#\A 14)))
                 finally (return hand))))
    (let ((hands ())
          (tail ())
          (count 0))
      (do-file (line "day7.txt" (values hands count))
        (let* ((cards (parse-hand line))
               (bid (parse-integer (subseq line 6)))
               (hand `(,(d7-hand-type cards) ,cards ,bid)))
          (cond
            (hands
             (setf (cdr tail) (list hand))
             (setf tail (cdr tail)))
            (T
             (setf hands (list hand))
             (setf tail hands)))
          (incf count))))))

(defun d7< (hand-a hand-b)
  (cond
    ((< (first hand-a) (first hand-b)) T)
    ((< (first hand-b) (first hand-a)) NIL)
    (T ;; Equal type.
     (loop with cards-a = (second hand-a)
           with cards-b = (second hand-b)
           for i from 0 below 5
           do (cond
                ((< (aref cards-a i) (aref cards-b i))
                 (return-from d7< T))
                ((< (aref cards-b i) (aref cards-a i))
                 (return-from d7< NIL)))))))

(defun d7p1 ()
  (loop for hand in (sort (d7-data) #'d7<)
        counting hand into i
        sum (* i (third hand))))

;; Answer: 250370104

(defun d7p2 ()
  (loop for hand in (sort (d7-data T) #'d7<)
        counting hand into i
        sum (* i (third hand))))
