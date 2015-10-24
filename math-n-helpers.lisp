(in-package #:fft-player)

(defun calc-fft-sample-stride (audio-sample-rate fft-sample-rate sample-buffer-size)
  (floor (/ audio-sample-rate (* fft-sample-rate sample-buffer-size))))

(defun default-hanning-function (sample-index sample-count)
  (* 0.5 (- 1 (cos (* 2 pi (/ sample-index (1- sample-count)))))))

(defun complex-magnitude (c)
  ;; given a complex number return the magnitude
  (sqrt (+ (expt (realpart c) 2) (expt (imagpart c) 2))))


(let ((a (make-array 2 :element-type 'double-float :initial-element 0d0))
      (b (make-array 3 :element-type 'double-float :initial-element 0d0)))
  (defun compute-second-order-low-pass-params
      (&key (sample-rate 44100) (high-end-freq 400))
    (let* ((w0 (* 2d0 pi (/ high-end-freq sample-rate)))
           (cosw0 (cos w0))
           (sinw0 (sin w0))
           (alpha (* (/ sinw0 2d0) (/ cosw0 2d0)))
           (a0 (1+ alpha))
           (b0 (/ (/ (- 1d0 cosw0) 2d0) a0)))
      (print alpha)
      (print a0)
      (print cosw0)
      (setf (aref a 0) (/ (* -2d0 cosw0) a0)
            (aref a 1) (/ (- 1d0 alpha) a0)
            (aref b 0) b0
            (aref b 1) (/ (- 1d0 cosw0) a0)
            (aref b 2) b0)))

  (defun process-second-order-filter (x mem)
    (let ((ret (- (+ (* (aref b 0) x)
                     (* (aref b 1) (aref mem 0))
                     (* (aref b 2) (aref mem 1)))
                  (* (aref a 0) (aref mem 2))
                  (* (aref a 1) (aref mem 3)))))
      (setf (aref mem 1) (aref mem 0)
            (aref mem 0) x
            (aref mem 3) (aref mem 2)
            (aref mem 2) ret)
      ret)))

(let ((mem0 (make-array 4 :element-type 'double-float :initial-element 0d0))
      (mem1 (make-array 4 :element-type 'double-float :initial-element 0d0)))
  (defun double-low-pass (val)
    (process-second-order-filter (process-second-order-filter val mem0)
                                 mem1)))
