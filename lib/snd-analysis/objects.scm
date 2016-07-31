(define-module (hans lib snd-analysis objects))
(use-modules (hans objects))

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
    (list
      ;; Onset detection
      (parameter 'energy
        "Calculates the local energy of the spectral frame" 1 '(0))
      (parameter 'hfc
        "Computes the High Frequency Content of the spectral frame" 1 '(0))
      (parameter 'complex
        "Complex Domain Method onset detection function" 1 '(0))
      (parameter 'phase
        "Phase Based Method onset detection function" 1 '(0))
      (parameter 'specdiff
        "Spectral difference method onset detection function" 1 '(0))
      (parameter 'kl
        "Kullback-Liebler onset detection function" 1 '(0))
      (parameter 'mkl
        "Modified Kullback-Liebler onset detection function" 1 '(0))
      ;; Signal
      (parameter 'rms
        "Loudness estimate in decibels" 1 '(0))
      ;; Spectral
      (parameter 'specflux
        "Measure of how quickly power spectrum has changed" 1 '(0))
      (parameter 'centroid
        "Represents the barycenter of the spectrum (hZ)" 1 '(0))
      (parameter 'spread
        "Variance of the spectral distribution around its centroid" 1 '(0))
      (parameter 'skewness
        "Computed from the third order moment of the spectrum" 1 '(0))
      (parameter 'kurtosis
        "Measure of the flatness of the spectrum" 1 '(0))
      (parameter 'slope
        "Decreasing rate of the spectral amplitude" 1 '(0))
      (parameter 'decrease
        "Decreasing rate, based on perceptual criteria" 1 '(0))
      (parameter 'rolloff
        "Bin number below which 95% of the spectrum energy is found" 1 '(0)))
    '()))
