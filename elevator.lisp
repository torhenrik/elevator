(defclass elevator ()
  ((top-floor :accessor top-floor :initarg :top-floor)
   (requests :accessor requests :initarg :requests)
   (current-floor :accessor current-floor :initarg :current-floor)
   (direction :accessor direction :initarg :direction)
   (emergency-p :accessor emergency-p :initarg :emergency-p)
   (travel-time :accessor travel-time :initarg :travel-time)
   (stop-time :accessor stop-time :initarg :stop-time)))

(defun make-elevator (&key
                        (top-floor 5)
                        (requests (make-array top-floor :initial-element nil))
                        (current-floor 0)
                        (direction :down)
                        (emergency-p nil)
                        (travel-time 10)
                        (stop-time 5))
  (make-instance 'elevator
                 :top-floor top-floor
                 :requests requests
                 :current-floor current-floor
                 :direction direction
                 :emergency-p emergency-p
                 :travel-time travel-time
                 :stop-time stop-time))

;; Helpers

(defmethod number-of-requests-between ((elevator elevator) start stop)
  (length (remove-if #'null (subseq (requests elevator)
                                    (min start stop)
                                    (max start stop)))))

(defmethod top-request ((elevator elevator))
  (position t (requests elevator) :from-end t))

(defmethod requests-above ((elevator elevator))
  (not (zerop (number-of-requests-between elevator (current-floor elevator) (top-floor elevator)))))

(defmethod requested-floors ((elevator elevator))
  (let ((floors nil))
    (dotimes (i (length (requests elevator)))
      (when (elt (requests elevator) i)
        (push i floors)))
    floors))


(defmethod number-of-floors-between (start stop)
  (- (max start stop) (min start stop)))

(defmethod next-direction ((elevator elevator))
  (cond ((emergency-p elevator)
         :stop)
        ((eq (current-floor elevator) (top-floor elevator))
         :down)
        ((and (eq (direction elevator) :up)
              (not (requests-above elevator)))
         :down)
        ((and (eq (current-floor elevator) 0)
              (requests-above elevator))
         :up)
        ((zerop (current-floor elevator))
         :down)
        (t (direction elevator))))


(defmethod next-floor ((elevator elevator))
  (setf (direction elevator) (next-direction elevator))
  (case (direction elevator)
    (:up
     (1+ (current-floor elevator)))
    (:down
     (max 0 (1- (current-floor elevator))))
    (:stop
     (current-floor elevator))))

(defmethod path ((elevator elevator) floor)
  (if (eq (current-floor elevator) floor)
      nil
      (let ((path nil)
            (elevator-copy
              (make-elevator :top-floor (top-floor elevator)
                             :requests (requests elevator)
                             :current-floor (current-floor elevator)
                             :direction (direction elevator)
                             :emergency-p (emergency-p elevator)
                             :stop-time (stop-time elevator)
                             :travel-time (travel-time elevator))))
        (request-floor elevator-copy floor)
        (loop
          do (progn (goto-next-floor elevator-copy)
                    (push (current-floor elevator-copy) path))
          while (not (eq (current-floor elevator-copy) floor)))
        (reverse path))))
      
        
        
;; Solutions to the assignment

(defmethod request-floor ((elevator elevator) floor)
  (setf (elt (requests elevator) floor) t))    

(defmethod remove-all-requests ((elevator elevator))
  (setf (requests elevator) (make-array (top-floor elevator) :initial-element nil)))

(defmethod goto-next-floor ((elevator elevator))
  (setf (direction elevator) (next-direction elevator))
  (setf (elt (requests elevator) (current-floor elevator)) nil)
  (setf (current-floor elevator) (next-floor elevator)))


(defmethod get-direction ((elevator elevator))
  (direction elevator))

(defmethod estimated-arrival-time ((elevator elevator) floor)
  (let ((travel-time (travel-time elevator))
        (stop-time (stop-time elevator)))
  (cond ((and (< floor (current-floor elevator))
              (eq (next-direction elevator) :down))
         ;; We go down to the target floor.
         (+ (* (number-of-floors-between (current-floor elevator) floor) travel-time)
            (* (number-of-requests-between elevator (current-floor elevator) floor) stop-time)))

        ((and (> floor (current-floor elevator))
              (eq (next-direction elevator) :down))
         ;; We go down to base level and then up again to the target floor.
         (+ (+ (* (current-floor elevator) travel-time)
               (* floor travel-time))
            (* (number-of-requests-between elevator 0 floor) stop-time)))

        ((and (< floor (current-floor elevator))
              (eq (next-direction elevator) :up))
         ;; We go up to highest requested floor, and then down to the target floor.
         (+ (* (number-of-floors-between (current-floor elevator) (top-request elevator))
                                         travel-time)
            (* (number-of-floors-between (top-request elevator) floor) travel-time)
            (* (number-of-requests-between elevator (top-request elevator) floor) stop-time)))
        
        ((and (> floor (current-floor elevator))
              (eq (next-direction elevator) :up))
         ;; We go up to the target floor.
         (+ (* (number-of-floors-between (current-floor elevator) floor) travel-time)
            (* (number-of-requests-between elevator (current-floor elevator) floor) stop-time)))

        ((eq floor (current-floor elevator))
         ;; We're already at the target floor.
         0)
        (t (error "This should not happen. At ~A going to ~A in direction ~A"
                  (current-floor elevator) floor (direction elevator))))))

(defmethod start-emergency ((elevator elevator))
  (remove-all-requests elevator)
  (setf (emergency-p elevator) t))

(defmethod stop-emergency ((elevator elevator))
  (setf (emergency-p elevator) t))



;; Helpers for running and testing the program

(defmethod estimated-arrival-times ((elevator elevator))
  (let ((estimates))
    (dotimes (i (top-floor elevator))
      (push (cons i (estimated-arrival-time elevator i)) estimates))
    estimates))
                 
(defmethod statusreport ((elevator elevator))
  (format t "Elevator with ~A floors, traveltime is ~A and stoptime is ~A.~%" (top-floor elevator) (travel-time elevator) (stop-time elevator))
  (if (emergency-p elevator)
      (format t "Emergency has been pressed~%")
      (format t "No emergency.~%"))
  (format t "Current floor is ~A.~%" (current-floor elevator))
  (format t "Direction is ~A, will go ~A next.~%" (direction elevator) (next-direction elevator))
  (format t "Stops requested ")
  (if (requested-floors elevator)
      (format t "at: ~A.~%" (requested-floors elevator))
      (format t ": None.~%"))
  (format t "Estimated arrival times:~%")
  (dolist (estimate (estimated-arrival-times elevator))
    (let ((floor (car estimate))
          (est-time (cdr estimate)))
      (format t "~A~A Floor ~A: ~A seconds.  " 
              (if (eq (current-floor elevator) floor)
                  ">"
                  " ")
              (if (elt (requests elevator) floor)
                  "*"
                  " ")            
              floor      
              est-time)
      (when (not (eq (current-floor elevator) floor))
        (format t "Path: ~A" (path elevator floor)))
      (format t "~%")))
  (format t "~%"))


(defmethod estimated-arrival-time ((elevator elevator) floor)
  (let ((travel-time (travel-time elevator))
        (stop-time (stop-time elevator)))
  (cond ((and (< floor (current-floor elevator))
              (eq (next-direction elevator) :down))
         ;; We go down to the target floor.
         (+ (* (number-of-floors-between (current-floor elevator) floor) travel-time)
            (* (number-of-requests-between elevator (current-floor elevator) floor) stop-time)))

        ((and (> floor (current-floor elevator))
              (eq (next-direction elevator) :down))
         ;; We go down to base level and then up again to the target floor.
         (+ (+ (* (current-floor elevator) travel-time)
               (* floor travel-time))
            (* (number-of-requests-between elevator 0 floor) stop-time)))

        ((and (< floor (current-floor elevator))
              (eq (next-direction elevator) :up))
         ;; We go up to highest requested floor, and then down to the target floor.
         (+ (* (number-of-floors-between (current-floor elevator) (top-request elevator))
                                         travel-time)
            (* (number-of-floors-between (top-request elevator) floor) travel-time)
            (* (number-of-requests-between elevator (top-request elevator) floor) stop-time)))
        
        ((and (> floor (current-floor elevator))
              (eq (next-direction elevator) :up))
         ;; We go up to the target floor.
         (+ (* (number-of-floors-between (current-floor elevator) floor) travel-time)
            (* (number-of-requests-between elevator (current-floor elevator) floor) stop-time)))

        ((eq floor (current-floor elevator))
         ;; We're already at the target floor.
         0)
        (t (error "This should not happen. At ~A going to ~A in direction ~A"
                  (current-floor elevator) floor (direction elevator))))))


;;; Testing the elevator algoritm

(defun elevator-test ()
  (let ((elevator (make-elevator)))
    
    (format t "TEST 1: We're going down, and a request is coming below.  We continue down.~%")
    (setf elevator (make-elevator :direction :down :current-floor 3))
    (request-floor elevator 1)
    (statusreport elevator)
    (assert (equal (path elevator 1) (list 2 1)))
    (statusreport elevator)
    (format t "OK.~%")
    
    (format t "TEST 2: We're going down, and a request is coming above.  We continue down before we go up again.~%")
    (setf elevator (make-elevator :direction :down :current-floor 3))
    (request-floor elevator 4)
    (assert (eq (next-direction elevator) :down))
    (statusreport elevator)
    (format t "OK.~%")
    
    (format t "TEST 3: We're going up, and a request is coming above.  We continue up.~%")
    (setf elevator (make-elevator :direction :up :current-floor 2))
    (request-floor elevator 3)
    (statusreport elevator)
    (assert (eq (next-direction elevator) :up))
    (format t "OK.~%")
    
    (format t "TEST 4: We're going up with a request above. A request is coming below.  We continue up.~%")
    (setf elevator (make-elevator :direction :up :current-floor 3))
    (request-floor elevator 4)
    (request-floor elevator 2)
    (statusreport elevator)
    (assert (eq (next-direction elevator) :up))
    (format t "OK.~%")
    
    (format t "TEST 5: We're going up with no requests above.  We go down.")
    (setf elevator (make-elevator :direction :up :current-floor 2))
    (statusreport elevator)
    (assert (eq (next-direction elevator) :down))
    (format t "OK.~%")

    (format t "TEST 6: We're at ground level with no requests.  We don't move.~%")
    (setf elevator (make-elevator :direction :down :current-floor 0))
    (statusreport elevator)
    (assert (eq (next-direction elevator) :down))
    (next-floor elevator)
    (assert (zerop (current-floor elevator)))
    (assert (every #'null (requests elevator)))
    (format t "OK.~%")

    (format t "TEST 7: Emergency has been pressed.  The requests are cleared and we don't move.~%")
    (setf elevator (make-elevator :direction :up :current-floor 2))
    (request-floor elevator 3)
    (request-floor elevator 1)
    (statusreport elevator)
    (start-emergency elevator)
    (assert (every #'null (requests elevator)))
    (next-floor elevator)
    (assert (eq (current-floor elevator) 2))))


   

