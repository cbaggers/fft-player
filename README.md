# fft-player

This is a stupidly simple ogg player that allows threadsafe access to the fft data of the audio being played.

I need this for some experiments but doubt this is will be useful to anyone.

    FFT-PLAYER> (defvar p (make-player "/Users/Baggers/Hydrate-Kenny_Beltrey.ogg"))

    FFT-PLAYER> (play p)
    T

    FFT-PLAYER> (pause p)
    :PAUSED

    FFT-PLAYER> (play p)
    T

    FFT-PLAYER> (last-fft n) ;; returns (simple-array double-float (64))
    #(13.115547048964576d0 4.987238110025276d0 0.5628531463760165d0
      0.11269125312599336d0 0.040338343893339804d0 0.019561061924362612d0
      0.01621200034417116d0 0.015725844804904465d0 0.006728962346668822d0
      0.003896444405510334d0 .... 0.002981695003461692d0)

    FFT-PLAYER> (dispose p)
