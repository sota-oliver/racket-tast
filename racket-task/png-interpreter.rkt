#lang racket

(require 2htdp/image
         simple-qr)

; state definition
(struct state (stack commands image))

; initial state
(define initial-state
  (state '() '() (empty-scene 500 500)))

; push
(define (push s x)
  (state (cons x (state-stack s)) (state-commands s) (state-image s)))

; pop
(define (pop s)
  (if (empty? (state-stack s))
      (error "Stack underflow")
      (state (cdr (state-stack s)) (state-commands s) (state-image s))))

; peek
(define (top s)
  (if (empty? (state-stack s))
      (error "Stack underflow")
      (car (state-stack s))))

; command definition
(define (interpret-command cmd s)
  (match cmd
    [(? string?)
     (push s cmd)]
    [(? number?)
     (push s cmd)]
    ['qr-code
     (let* ([data (top s)]
            [s1 (pop s)])
       (qr-write data "output-qr.png"
                 #:module_width 10
                 #:color '("black" . "white")
                 #:output_type 'png)
       s1)]))

; run commands
(define (run-commands commands)
  (define (loop cmds state)
    (if (null? cmds)
        state
        (loop (cdr cmds) (interpret-command (car cmds) state))))
  (loop commands initial-state))

; generate a fractal pattern (e.g. sierpinski carpet)
(define (pattern n)
  (cond
    [(zero? n) (square 1 "solid" "black")]
    [else
     (local [(define c (pattern (- n 1)))
             (define i (square (image-width c) "solid" "white"))]
       (above (beside c c c)
              (beside c i c)
              (beside c c c)))]))

; generate a QR code and save it to a file
(define (generate-qr-code)
  (define commands '( "https://docs.racket-lang.org/" qr-code))
  (run-commands commands))

; generate pattern and save it to a file
(define (generate-pattern n)
  (let ([carpet (pattern n)])
    (save-image carpet "output-pattern.png")))

; main function to generate all outputs
(define (main)
  (generate-qr-code)           ; generate QR code and save it as "output-qr.png"
  (generate-pattern 5))        ; generate pattern (Sierpinski Carpet level 5) and save it as "output-pattern.png"

; start main function
(main)
