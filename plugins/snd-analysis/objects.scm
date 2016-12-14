(define-module (hans plugin snd-analysis objects)
  :use-module (hans objects))

(define library "libhans.snd.analysis")

(define-public (snd-fft settings args)
  (audio-object "snd-fft"
    library
    "Performs a Fast Fourier transform on any incoming signal and outputs the
real and imaginary parts of the transform."
    '()
    (list (audio-buffer "snd/fft/real" 1 (assq-ref settings 'blocksize))
          (audio-buffer "snd/fft/imag" 1 (assq-ref settings 'blocksize)))))

(define-public (snd-ifft settings args)
  (audio-object "snd-ifft"
    library
    "Performs an inverse fast Fourier transform on fft data"
    '()
    (list (audio-buffer "snd/ifft/signal" 1 (assq-ref settings 'blocksize)))))

(define-public (snd-feature settings args)
  (audio-object "snd-feature"
    library
    "Extract an audio feature from a signal"
    `(;; Onset detection
      ,(parameter 'energy "Calculates the local energy of the spectral frame")
      ,(parameter 'hfc "Computes High Frequency Content of the spectral frame")
      ,(parameter 'complex "Complex Domain Method onset detection function")
      ,(parameter 'phase "Phase Based Method onset detection function")
      ,(parameter 'specdiff "Spectral difference method onset detection")
      ,(parameter 'kl "Kullback-Liebler onset detection")
      ,(parameter 'mkl "Modified Kullback-Liebler onset detection")
      ;; Signal
      ,(parameter 'rms "Loudness estimate in (dB)")
      ;; Spectral
      ,(parameter 'specflux "Measure of how quickly power spectrum has changed")
      ,(parameter 'centroid "Represents the barycenter of the spectrum (hZ)")
      ,(parameter 'spread "Spectral distribution variance around the centroid")
      ,(parameter 'skewness "Computed from the third order moment of spectrum")
      ,(parameter 'kurtosis "Measure of flatness of the spectrum")
      ,(parameter 'slope "Decreasing rate of the spectral amplitude")
      ,(parameter 'decrease "Decreasing rate, based on perceptual criteria")
      ,(parameter 'rolloff "Bin below which 95% of spectrum energy is found"))
    '()))
