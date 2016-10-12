(define-module (examples patterns)
  :use-module (hans sequencer)
  :export (patt0 
           patt1))

(define (patt0 bpm)
  `((,(bpm->ms bpm 4) ,(pattern 72 ~ ~ ~ 60 ~ ~ ~ 60 ~ ~ ~ 60 ~ ~ ~))))

(define (patt1 bpm)
  `((,(bpm->ms bpm 7)  ,(pattern 47 ~ ~ ~ ~ ~ ~ 47 ~ ~ ~ 47 ~ ~))
    (,(bpm->ms bpm 4)  ,(pattern 36 36 ~ ~ ~ ~ ~ 36 36 ~ ~ 36 ~ ~))
    (,(bpm->ms bpm 7)  ,(pattern ~ ~ ~ ~ ~ 38 ~ ~))
    (,(bpm->ms bpm 4)  ,(pattern 40 ~ ~ 40 ~ ~ ~ ~ ~ ~ ~ ~ 40 ~))
    (,(bpm->ms bpm 14) ,(pattern ~ ~ ~ ~ ~ ~ ~ 41))
    (,(bpm->ms bpm 21) ,(pattern 43 ~ 43 43 ~ ~ ~))
    (,(bpm->ms bpm 7)  ,(pattern ~ 45 ~ ~ ~ 45 45))))
