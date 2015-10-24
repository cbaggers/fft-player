(in-package #:fft-player)

;;"Hydrate-Kenny_Beltrey.ogg"

(defun make-player (ogg-file-path)
  (init-fft-player-backend)
  (make-fft-player ogg-file-path))

(defmethod play (player)
  (with-slots (mixer streamer) player
    (if (member streamer (mixalot:mixer-stream-list mixer) :test #'eq)
        (streamer-unpause streamer mixer)
        (mixer-add-streamer mixer streamer))))

(defmethod pause (player)
  (with-slots (mixer streamer) player
    (streamer-pause streamer mixer)))

(defmethod dispose (player)
  (with-slots (mixer) player
    (mixer-remove-all-streamers mixer)))

(defun last-fft (player)
  (with-slots (idol) player
    (indy:behold idol)))
