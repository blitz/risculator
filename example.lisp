;;; -*- Mode: Lisp -*-

(in-package :blitz.risculator)

(defunit (step-count
          :state ((count 32 :initial 0))
          :output ((res 8)
                   (res-big 32)))
  (setf res (logand count #xFF)
        res-big count
        count (logand #xFFFFFFFF (1+ count))))

(defunit (adder8
          :input ((val1 8)
                  (val2 8))
          :output ((carry 1)
                   (res 8)))
  (let ((real-result (+ val1 val2)))
    (setf res (logand real-result #xFF)
          carry (logand 1 (ash real-result 8)))))

(defunit (printer
          :input ((count 32)
                  (val 8)))
  (when (= count 100000000)
    (return val)))

(defpipe (random-add-print
          :units ((st step-count)
                  (add adder8)
                  (pr printer)))
  ;; Always INPUT <- OUTPUT
  (<- (add val1) (st res)
      (add val2) (add res)
      (pr count) (st res-big)
      (pr val) (add res)))

(defun reference (&optional (count 0) (val 0))
  (declare (type (unsigned-byte 32) count)
           (type (unsigned-byte 8) val))
  (declare (optimize (speed 3)
                     (safety 0)))
  (if (= count 100000000)
      val
      (reference (1+ count) (logand #xFF (+ (logand #xFF count) val)))))

;;; EOF
