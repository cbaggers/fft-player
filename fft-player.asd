;;;; fft-player.asd

(asdf:defsystem #:fft-player
  :description "Describe fft-player here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:mixalot #:mixalot-vorbis #:bordeaux-fft #:indy)
  :components ((:file "package")
               (:file "math-n-helpers")
               (:file "backend")
               (:file "frontend")))
