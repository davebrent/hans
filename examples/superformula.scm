(use-modules (hans compiler)
             (hans patcher)
             (hans modules gfx-superformula objects)
             (hans modules gfx-quad objects)
             (hans modules gfx-filter objects))

(define hans (make-environment `(
  (gfx-quad         . ,gfx-quad)
  (gfx-superformula . ,gfx-superformula)
  (gfx-filter       . ,gfx-filter))))

;; Share an object across programs
(define superformula (hans 'create 'gfx-superformula '() '(0 0)))

(define (make-program name shader)
  (let ((effect (hans 'create 'gfx-filter `((name . ,shader)) '(0 0)))
        (window (hans 'create 'gfx-quad '() '(0 0))))
    (hans-program name
      (make-audio-graph)
      (make-graphics-graph
        (hans 'connect superformula 0 effect 0)
        (hans 'connect effect 0 window 0)))))

(define options '(
  (output        . "superformula.hans")
  (library-paths . ("/Users/dave/Projects/hans/build/lib"
                    "/vagrant/build/lib"))))

(let ((programs (list (make-program "cgadisplay" "filter/shader/cgadisplay")
                      (make-program "dotscreen" "filter/shader/dotscreen")
                      (make-program "greyscale" "filter/shader/greyscale")
                      (make-program "halftone" "filter/shader/halftone")
                      (make-program "rgbsplit" "filter/shader/rgbsplit"))))
        
  (hans-compile (hans-file programs) options))
