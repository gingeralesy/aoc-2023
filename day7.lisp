(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/7

(defun d7-hand-type (hand)
  "Analyses the hand for its type. The returned type is a number between 1 and 7.
7 - Five of a kind.
6 - Four of a kind.
5 - Full house.
4 - Three of a kind.
3 - Two pairs.
2 - Two of a kind.
1 - High card."
  (declare (type (simple-array (integer 0 15) (5)) hand))
  ;; (declare (optimize (speed 3)))
  (let ((checked (make-array 15 :element-type '(integer 0 5) :initial-element 0)))
    (flet ((find-count (count)
             "Returns the first card with the wanted count. Does not include the joker."
             (find count checked :start 2 :test #'=)))
      (let ((highest-count 0)
            (highest-card 2)) ;; Default to 2.
        (dotimes (i 5)
          (let* ((card (aref hand i))
                 (count (1+ (aref checked card))))
            (setf (aref checked card) count)
            (when (and (< 1 card) (< highest-count count)) ;; Do not include jokers.
              (setf highest-card card)
              (setf highest-count count))))
        ;; In JJJJJ hand the highest-card value is still the default, which gets the 5.
        (incf (aref checked highest-card) (aref checked 1))
        (setf highest-count (aref checked highest-card)) ;; Get the highest count + jokers.
        (when (= highest-count 5) (return-from d7-hand-type 7)) ;; Five of a kind.
        (when (= highest-count 4) (return-from d7-hand-type 6)) ;; Four of a kind.
        (when (= highest-count 3)
          (when (find-count 2) (return-from d7-hand-type 5)) ;; Full house.
          (return-from d7-hand-type 4)) ;; Three of a kind.
        (when (= highest-count 1) (return-from d7-hand-type 1)))) ;; High card.
    ;; So it's either one or two pairs.
    (loop for card from 2 below 15 ;; Skip the joker again.
          for count = (aref checked card)
          when (= 2 count) count card into pair-count
          finally (return (1+ pair-count)))))

(defun d7-data (&optional jokers-p)
  ;; (declare (optimize (speed 3)))
  (flet ((parse-hand (line)
           (loop with hand = (make-array 5 :element-type '(integer 0 15) :initial-element 0)
                 for i from 0 below 5
                 for char of-type base-char = (char line i)
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
    (let (hands tail)
      (do-file (line "day7.txt" hands)
        (let* ((cards (parse-hand line))
               (bid (parse-integer (subseq line 6)))
               (hand `(,(d7-hand-type cards) ,cards ,bid)))
          (cond
            (hands
             (setf (cdr tail) (list hand))
             (setf tail (cdr tail)))
            (T
             (setf hands (list hand))
             (setf tail hands))))))))

(defun d7< (hand-a hand-b)
  (declare (type list hand-a hand-b))
  ;; (declare (optimize (speed 3)))
  (let ((type-a (first hand-a))
        (type-b (first hand-b)))
    (declare (type (integer 1 7) type-a type-b))
    (cond
      ((< type-a type-b) T)
      ((< type-b type-a) NIL)
      (T ;; Equal type.
       (loop with cards-a of-type (simple-array (integer 0 15) (5)) = (second hand-a)
             with cards-b of-type (simple-array (integer 0 15) (5)) = (second hand-b)
             for i from 0 below 5
             do (cond
                  ((< (aref cards-a i) (aref cards-b i))
                   (return-from d7< T))
                  ((< (aref cards-b i) (aref cards-a i))
                   (return-from d7< NIL)))
             finally (error "Identical hands: ~a = ~a" hand-a hand-b))))))

(defun d7p1 ()
  (loop for hand in (sort (d7-data) #'d7<)
        counting hand into i
        sum (* i (third hand))))

;; Answer: 250370104

(defun d7p2 ()
  (loop for hand in (sort (d7-data T) #'d7<)
        counting hand into i
        sum (* i (third hand))))

;; Answer: 251735672
