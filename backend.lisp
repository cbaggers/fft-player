(in-package #:fft-player)

(defclass fft-player ()
  ((mixer
    :initform (create-mixer))
   (streamer
    :initarg :streamer)
   (idol
    :initform (indy:idolize -1))
   (fft-sample-count
    :initform 128)
   (playback-sample-rate
    :initform 44100)
   (fft-sample-rate
    :initform 60)
   (fft-sample-stride
    :initform (calc-fft-sample-stride 44100 60 128))
   (hanning-func
    :initform #'default-hanning-function)
   (fft-smoothing-factor
    :initform 0.95)
   (sample-index
    :initform 0)
   (complex-sample-array
    :initform (make-array 128 :element-type '(complex double-float)))
   (complex-fft-array
    :initform (make-array 128 :element-type '(complex double-float)))
   (smoothing-buffer
    :initform (make-array 128 :element-type 'double-float))))

(defclass fft-vorbis-streamer (vorbis-streamer)
  ((player :initarg :player :reader fft-player)))

(let ((initialized nil))
  (defun init-fft-player-backend ()
    (unless initialized
      (setf initialized t)
      (main-thread-init)
      (compute-second-order-low-pass-params))))

(defun make-fft-player (file-path)
  (let ((player (make-instance 'fft-player))
        (streamer (make-vorbis-streamer file-path :class 'fft-vorbis-streamer)))
    (setf (slot-value streamer 'player) player)
    (setf (slot-value player 'streamer) streamer)
    player))

(defmethod streamer-mix-into :after ((streamer fft-vorbis-streamer) mixer mix-buffer
                                     offset length time)
  ;; For the sake of this example we will only be taking 128 samples at a time.
  ;; each sample is stereo. 2 16bit samples are combined into 1 32bit number
  (let ((player (fft-player streamer)))
    (with-slots (sample-index complex-sample-array fft-sample-stride
                              fft-sample-count hanning-func)
        player
      (when (> length fft-sample-count)
        (loop :for si :from offset :to (+ offset length) :by fft-sample-stride :do
           (setf (aref complex-sample-array sample-index)
                 ;; extract one of the channels, and transform into the -1.0 to 1.0 range
                 (let* ((s (aref mix-buffer si))
                        (one-channel (- (/ (logand s #xFFFF) #x7FFF) 1.0)))
                   ;; make a complex sample as this is what bordeaux-fft uses
                   (complex
                    (* (double-low-pass (coerce one-channel 'double-float))
                       ;; multiply by the hanning function, this is
                       ;; to help stop lobes
                       (funcall hanning-func sample-index fft-sample-count))))
                 sample-index (mod (1+ sample-index) fft-sample-count))
           (when (= sample-index 0)
             (fft-and-process player)))))))

(defun fft-and-process (player)
  (with-slots (complex-fft-array complex-sample-array smoothing-buffer
                                 fft-smoothing-factor idol)
      player
    (fft! complex-sample-array complex-fft-array)
    (loop :for s :across complex-fft-array :for i :from 0 :do
       (setf (aref smoothing-buffer i)
             (+ (* (aref smoothing-buffer i) fft-smoothing-factor)
                (* (complex-magnitude (aref complex-fft-array i))
                   (- 1.0 fft-smoothing-factor)))))
    (let ((next-arr (make-array 64 :element-type 'double-float)))
      (progn
        (loop :for i :from 1 :to 64 :do
           (setf (aref next-arr (1- i)) (aref smoothing-buffer i)))
        (indy:swap-val idol next-arr)))))
