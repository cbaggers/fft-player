;;;; package.lisp

(defpackage #:fft-player
  (:use #:cl #:mixalot #:mixalot-vorbis #:bordeaux-fft)
  (:export :make-player :play :pause :dispose :last-fft))
