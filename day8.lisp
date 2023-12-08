(in-package #:aoc-2023)

;; https://adventofcode.com/2023/day/8

(defun d8-data ()
  (declare (optimize (speed 3)))
  (with-local-file (stream "day8.txt")
    (let (starts start-tail ends end-tail)
      (values
       (loop with line of-type input-line = (read-clean-line stream)
             for i from 0 below (length line)
             collecting (ecase (char line i) (#\L :left) (#\R :right)))
       (loop with node-re = (cl-ppcre:create-scanner
                             "^(\\w\\w\\w) = \\((\\w\\w\\w), (\\w\\w\\w)\\)$")
             with node-map = (make-hash-table)
             for line of-type input-line = (read-clean-line stream)
             until (eql line :eof)
             when (and line (< 0 (length line)))
             do (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings node-re line)
                  (unless (and match groups) (error "Invalid line: ~a" line))
                  (let* ((node-name (the (simple-array character (3)) (svref groups 0)))
                         (node (intern node-name :keyword))
                         (left (intern (svref groups 1) :keyword))
                         (right (intern (svref groups 2) :keyword)))
                    (case (char node-name 2)
                      (#\A
                       (cond
                         (starts
                          (setf (cdr start-tail) (list node))
                          (setf start-tail (cdr start-tail)))
                         (T
                          (setf starts (list node))
                          (setf start-tail starts))))
                      (#\Z
                       (cond
                         (ends
                          (setf (cdr end-tail) (list node))
                          (setf end-tail (cdr end-tail)))
                         (T
                          (setf ends (list node))
                          (setf end-tail ends)))))
                    (setf (gethash node node-map) `(,left . ,right))))
             finally (return node-map))
       starts (loop with end-table = (make-hash-table)
                    for end in ends
                    do (setf (gethash end end-table) end)
                    finally (return end-table))))))

(declaim (inline d8-move))
(defun d8-move (from direction node-map)
  (let ((choices (gethash from node-map)))
    (ecase direction
      (:left (car choices))
      (:right (cdr choices)))))

(defun d8-route (directions node-map start ends)
  (declare (type hash-table ends))
  (declare (optimize (speed 3)))
  (loop for current = directions then (or (rest current) directions)
        for node of-type keyword = (d8-move start (car current) node-map)
        then (d8-move node (car current) node-map)
        collect node into path
        count node into steps
        until (gethash node ends)
        finally (return (values steps path))))

(defun d8p1 ()
  (multiple-value-bind (directions node-map) (d8-data)
    (nth-value 0 (d8-route directions node-map :aaa (alexandria:plist-hash-table '(:zzz :zzz))))))

;; Answer: 20659

(defun d8-find-ends (directions node-map start ends)
  (declare (optimize (speed 3)))
  (loop for direction in directions
        for curr = start then next
        for next = (d8-move curr direction node-map)
        counting next into steps
        when (gethash next ends) collect steps into end-distances
        finally (return (list (nconc (when (gethash start ends) (list 0)) end-distances) next 1))))

(defun d8-find-all-ends (directions node-map ends)
  (declare (optimize (speed 3)))
  (labels ((update-route (from routes &optional path)
             (declare (type keyword from))
             (declare (type list path))
             (let ((route (gethash from routes)))
               (when (and route (first route)) ;; Already valid.
                 (return-from update-route route))
               (when (or (null route) (find from path) ;; A loop without an end.
                         (and (null (first route)) (eql from (second route))))
                 (format T "~&~a loops.~%" from) ;; Debugging.
                 (return-from update-route))
               (when (first (gethash (second route) routes)) ;; Already valid.
                 (return-from update-route route))
               (let ((new-next (update-route (second route) routes (nconc path (list from)))))
                 (when (and (null (first route)) (null new-next)) ;; An already removed dead end.
                   (format T "~&~a leads to a loop.~%" from)
                   (return-from update-route))
                 (when (or (null new-next) (first new-next)) ;; Shouldn't be possible.
                   (unless new-next (format T "~&WARN! ~a leads to an error!" from))
                   (return-from update-route route))
                 (let ((new-route (list (first route) (second new-next)
                                        (1+ (the (unsigned-byte 16) (third new-next))))))
                   (setf (gethash from routes) new-route)
                   new-route)))))
    (let ((nodes (alexandria:hash-table-keys node-map))
          (routes (make-hash-table)))
      (loop for node in nodes ;; Find the routes to endings available from every node.
            for route = (d8-find-ends directions node-map node ends)
            do (setf (gethash node routes) route))
      (loop for node in nodes ;; Update the routing to skip empty nodes.
            ;; do (update-route node routes)
            for route = (update-route node routes)
            unless route do (format T "~&Loop removed for ~a~%" node) ;; Debugging.
            unless route do (remhash node routes)
            finally (return routes)))))

(defun d8p2 ()
  (declare (optimize (speed 3)))
  (multiple-value-bind (directions node-map starts ends) (d8-data)
    (declare (type list directions starts))
    (loop with count = (length starts)
          with round-distance of-type (unsigned-byte 16) = (length directions)
          with routes = (d8-find-all-ends directions node-map ends)
          with nodes = (make-array count :element-type 'keyword :initial-contents starts)
          with rounds = (make-array count :element-type '(unsigned-byte 64) :initial-element 0)
          ;; This finds the nodes that are behind to update.
          for max-round of-type (unsigned-byte 64) = (loop for i from 0 below count
                                                           maximizing (aref rounds i))
          for nodes-to-update = (loop for i from 0 below count
                                      collect i into all
                                      when (< (aref rounds i) max-round)
                                      collect i into behind
                                      finally (return (or behind all)))
          ;; Update the nodes that are behind until they catch up.
          ;; If all the nodes are equal position only one step is moved.
          ;; Note: If the first iteration of the input were to find a match this would fail. To fix
          ;;       that this node updating should be moved to the end of the loop. But that looked
          ;;       ugly so I kept it this way.
          do (loop for i in nodes-to-update
                   do (loop for node = (aref nodes i)
                            for (_ target distance) = (gethash node routes)
                            unless (gethash target routes)
                            do (error "Node ~a's target ~a does not exist." node target)
                            do (setf (aref rounds i) (+ (aref rounds i)
                                                        (the (unsigned-byte 16) distance)))
                            do (setf (aref nodes i) target)
                            while (< (aref rounds i) max-round)))
             ;; do (format t "~&~a~%~a~%------~%" nodes rounds) ;; Debugging.
             ;; While it's possible that a round's full distance movement would match, the node
             ;; that is behind will be moved to where the ending would be anyways so they can both
             ;; be checked for the zero index on the same round.
          when (loop for i from 1 below count ;; All on the same round.
                     unless (= (aref rounds 0) (aref rounds i)) do (return NIL)
                     finally (return T))
          do (loop for distance of-type (unsigned-byte 16) in
                   (loop with ends-a = (the list (first (gethash (aref nodes 0) routes)))
                         with ends-b = (the list (first (gethash (aref nodes 1) routes)))
                         for end of-type (unsigned-byte 16) in ends-a
                         when (find end ends-b) collect end)
                   when (loop for i from 2 below count
                              for route = (gethash (aref nodes i) routes)
                              unless (find distance (the list (first route))) do (return NIL)
                              finally (return T))
                   collect distance into matches
                   finally (when matches
                             (return-from d8p2
                               (+ (loop for match of-type (unsigned-byte 16) in matches
                                        minimizing match)
                                  (u64 (* (aref rounds 0) round-distance)))))))))

;; Answer: 15690466351717
