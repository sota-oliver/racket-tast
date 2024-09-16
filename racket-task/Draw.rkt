#lang racket

(require 2htdp/image)

; Definicija stanja
(struct state (stack image))

; Inicijalno stanje
(define initial-state
  (state '() (empty-scene 500 500)))

; Dodavanje operacije na stack
(define (push s x)
  (state (cons x (state-stack s)) (state-image s)))

; Vraćanje vrha stacka
(define (top s)
  (car (state-stack s)))

; Funkcija za crtanje pravokutnika
(define (draw-rect s x y w h)
  (state (state-stack s)
         (place-image (rectangle w h 'solid 'black) x y (state-image s))))

; Funkcija za generiranje slike s uzorkom pravokutnika
(define (generate-pattern s)
  (for*/fold ([state s])
             ([i (in-range 50 500 50)]    ; X
              [j (in-range 50 500 50)])   ; Y
    (let* ([s1 (push state i)]
           [s2 (push s1 j)] 
           [s3 (push s2 40)]
           [s4 (push s3 40)])
      (draw-rect s4 (top s1) (top s2) (top s3) (top s4)))))


; Funkcija za generiranje slike i spremanje u datoteku
(define (generate-image filename)
  (define final-state (generate-pattern initial-state))
  (save-image (state-image final-state) filename))

; Generiranje slike i spremanje u datoteku
(generate-image "pattern.png")
