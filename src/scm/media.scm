(define-module (hans media)
  :use-module (ice-9 popen)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (hans sys)
  :use-module (hans extension)
  :export (image-encode
           make-video-encoder
           video-encoder-encode
           video-encoder-close))

(hans-load-extension "libhansmedia" "scm_init_hans_media_module")

(define (lossless-cmd in out)
  (string-append
    "ffmpeg -loglevel panic -y -hide_banner -i "
    in
    " -c:v libx264 -crf 0 -preset veryslow "
    out))

(define (lossy-cmd in out)
  (string-append
    "ffmpeg -loglevel panic -y -hide_banner -i "
    in
    " -c:v libx264 -profile:v high444 -preset veryslow "
    out))

(define (filename-extension filename)
  (let ((components (remove (lambda (s)
                              (string=? s ""))
                            (string-split filename #\.))))
    (if (eq? (length components) 1)
      (cons filename "")
      (cons (string-join (reverse (cdr (reverse components))) ".")
            (string-append "." (last components))))))

(define (unique-filename filename)
  ;; Returns a unique filename
  (let ((components (filename-extension filename)))
    (define (make-unique the-filename i)
      (if (not (file-exists? the-filename))
        the-filename
        (make-unique (apply string-append `(,(car components)
                                            "-"
                                            ,(number->string i)
                                            ,(cdr components)))
                     (+ 1 i))))
    (make-unique filename 0)))

(define-record-type <video-out>
  (make-video-out encoder ffmpeg path)
  video-out?
  (encoder video-out-encoder)
  (ffmpeg  video-out-ffmpeg)
  (path    video-out-path))

(define (make-video-encoder filepath width height)
  (let* ((path (tmpnam))
         (_ (mknod path 'fifo #o660 0))
         (out (unique-filename filepath))
         (cmd (lossy-cmd path out))
         (ffmpeg (open-output-pipe cmd))
         (encoder (make-hans-primitive 'video-encoder `(,out ,width ,height))))
    (make-video-out encoder ffmpeg path)))

(define (video-encoder-encode video-out frame)
  (%video-encoder-encode (video-out-encoder video-out) frame))

(define (video-encoder-close video-out)
  (%video-encoder-close (video-out-encoder video-out))
  ;; XXX: ofstream.close doesnt result in the fifo being closed???
  (close-port (open-file (video-out-path video-out) "w"))
  (close-pipe (video-out-ffmpeg video-out))
  (delete-file (video-out-path video-out)))
