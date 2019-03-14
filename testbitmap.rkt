#lang racket
(require racket/gui/base)
(require racket/draw)
(require images/flomap)

(define imgname "")
; base image ref
(define baseimg (make-bitmap 900 600  ))
(define baseimgh 900)
(define baseimgw 600)
(define winh 800)
(define winw 1100)
(define h winh)
(define mbh 23)
(define ratio 0.5)
(define zoom  1)
(define α 0)
(define w winw)
(define img (make-bitmap winw winh ))
(define imgdc(new bitmap-dc% [bitmap img]))

(define (loadimg)
  (let [(name (get-file "Select image" #f  #f #f #f  null '(("JPEG" "*.jp*") ("PNG" "*.png"))))
        ]
    (when name (send baseimg  load-file name)
      (set! α 0)
      (set!  baseimgh (send baseimg get-height))
      (set!  baseimgw (send baseimg get-width))
      (set! ratio (/ baseimgw baseimgh))
      (rebuild)))
  )

(define (zscaled v r)(inexact->exact(round(* r zoom v))))

(define (rebuild )
  (let* ([hpan (+ mbh (send hpanel get-height))]
         ;[ch (send canvas get-height) ]
         [ch (if (< 1 ratio) (send canvas get-width) (send canvas get-height))]
         ;[cw(send canvas get-width) ]
         ;[cr (inexact->exact(round(/ ch cw)))]
         )
    (printf "enter rebuild w h  ~a ~a ~n" winw winh )
    (case α
      [(-90 90 260)
       (set! w (zscaled ch 1))
       (set! h (zscaled  ch ratio))
       ]
      [else
       (set! w (zscaled  ch ratio ))
       (set! h (zscaled ch 1))
       ]      
      )
    (printf "rebuild z w h ~a ~a ~a ~n" zoom w h)
    (set! img (make-bitmap w h))
    (send imgdc clear)


    (set! imgdc(new bitmap-dc% [bitmap img]))                  
    (case α
      [(0)  (send imgdc  set-origin 0 0)] 
      [(90) (send imgdc  set-origin 0  h )] 
      [(180)(send imgdc  set-origin w  h )])
    (send imgdc set-rotation (* α (/ pi 180 )))
    (send imgdc set-scale zoom zoom )
    (if (= α 90)
        (send imgdc  draw-bitmap-section-smooth  baseimg 0 0 h w  0 0  baseimgw baseimgh)     
        (send imgdc  draw-bitmap-section-smooth  baseimg 0 0 w h 0 0 baseimgw baseimgh)
        )
    (send mainframe set-status-text (format "baseSize ~a/~a  zoom:~a ratio:~a  w/h:~a/~a " baseimgh baseimgw zoom ratio w h  ))
    (send canvas init-auto-scrollbars w h 0.0 0.0)
    (send canvas refresh-now)
    ;(send canvas flush)
    ))
; event on paint
(define (do-paint canvas dc)
  (send dc draw-bitmap-section  img 0 0 0 0  w h)
;  (set! img(flomap->bitmap  
;       (flomap-transform  (bitmap->flomap img) (flomap-fisheye-transform  (* 2/3 pi)))))
  )

(define myframe% (class frame% 
                   ;(define/override (on-size nw nh )
                     ;(send this resize winh winw)
                    ; (printf "frame nw ~a nh ~a ~n" nw nh)
                     ;;; je ne sais pas pourquoi il me sort des 23 ??? hauteur menu-bar ???
                     ;(if  (> nh 50) (set! winh nh)(set! mbh nh))
                     ; (set! winw nw)
           
                    ; )
                   (super-new)) 
  )
;; main frame with canvas inside
(define mainframe (new myframe% [label "demo"][width winw][height winh]
                       [alignment (list 'center 'center)]))
(define menu-bar (new menu-bar%
                      (parent mainframe)))
(define menu-file (new menu%
                       (label "&File")
                       (parent menu-bar)))
(define menu-edit (new menu% 
                       (label "&Edit")
                       (parent menu-bar)))
(new menu%
     (label "&Help")
     (parent menu-bar))
(define (menudo  obj ev)
  (printf "ACTION: ~s  EVENT: ~a"  (send obj get-label) (send ev get-event-type) )

  )
(define (do-menu-rotate n)
  (set! α n)
  (rebuild)
  )
(new menu-item%	 [label "Open"]	[parent menu-file] [callback (λ (obj ev)(loadimg))])
(new menu-item%	 [label "Save"]	[parent menu-file] [callback (λ (obj ev) (send img save-file "out.png" 'png)) ])
(new separator-menu-item% [parent menu-file])
(new menu-item%	 [label "Quit"]	[parent menu-file] [callback (λ (obj ev) (exit))])
(new menu-item%	 [label "Rotate 0 "] [parent menu-edit] [callback (λ (obj ev) (do-menu-rotate 0)) ])
(new menu-item%	 [label "Rotate 90 "] [parent menu-edit] [callback (λ (obj ev) (do-menu-rotate 90)) ])
(new menu-item%	 [label "Rotate 180 "] [parent menu-edit] [callback (λ (obj ev) (do-menu-rotate 180)) ])
(define vpanel (new vertical-panel%   [parent mainframe] [style (list 'border)]
                    [alignment (list 'center 'center)] ))
(define hpanel (new horizontal-panel%   [parent vpanel][stretchable-height #f][stretchable-width #f]))
(define radio-rotation (new radio-box% [label "Rotation"][parent hpanel]
                            [style (list 'horizontal )]
                            [choices (list "0" "90" "180")]
                            [callback (λ (obj ev) (do-menu-rotate (string->number (send radio-rotation get-item-label (send radio-rotation get-selection))))) ]
                            ))
(define zoomscale (new slider% [label "zoom"][parent hpanel][min-value 50][max-value 150]
                       [init-value 100][style (list 'horizontal 'horizontal-label )]
                       [callback (λ (obj ev) (set! zoom (/ (send zoomscale get-value)100.0))(rebuild))]
                       ))
(define resetscale (new button% [label "reset scale"][parent hpanel]
                        [callback (λ (obj ev) (send zoomscale set-value 100) (set! zoom 1)(rebuild))]))
(define canvas (new canvas%	[parent vpanel]
                    [style (list 'vscroll 'hscroll )]
                    [paint-callback do-paint]))

(send mainframe create-status-line)
(send mainframe show #t)
