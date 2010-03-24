;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname the-great-escape) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)

; ------------------------------
;;;;;;;;;;;;;;;
;; CONSTANTS ;;
;;;;;;;;;;;;;;;
(define SCREEN-WIDTH 500)
(define SCREEN-HEIGHT 500)
(define DEFAULT-PLAYER-SPEED 5)

; ------------------------------
;;;;;;;;;;;;;;;;;;;;;
;; DATA STRUCTURES ;;
;;;;;;;;;;;;;;;;;;;;;
(define-struct vector2 (x y))
(define-struct particle (speed direction))
(define-struct sprite (x y particle image))
(define-struct world (player))

; ------------------------------
;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;
(define (magnitude-vector2 v)
  (sqrt (+ (sqr (vector2-x v))
           (sqr (vector2-y v)))))

(define (normalize-vector2 v)
  (let ((x (vector2-x v))
        (y  (vector2-y v))
        (mag (magnitude-vector2 v)))
    (make-vector2 (/ x mag) (/ y mag))))

(define (scale-vector2 n v)
  (make-vector2 (* n (vector2-x v))
                (* n (vector2-y v))))

(define (set-sprite-heading s x y)
  (make-sprite (sprite-x s)
               (sprite-y s)
               (set-particle-direction (sprite-particle s) (- x (sprite-x s)) (- y (sprite-y s)))
               (sprite-image s)))

(define (make-normalized-vector2 x y)
  (normalize-vector2 (make-vector2 x y)))

(define (set-particle-direction p x y)
  (make-particle (particle-speed p)
                 (make-normalized-vector2 x y)))


; ------------------------------
;;;;;;;;;;;;;;;;;;;;
;; DEFAULT VALUES ;;
;;;;;;;;;;;;;;;;;;;;
(define player-layer (circle 10 "solid" "yellow"))
(define (empty-scene w h) (rectangle w h "solid" "blue"))

(define initial-player
  (make-sprite
   (/ SCREEN-WIDTH 2)
   (- SCREEN-HEIGHT (image-height player-layer))
   (make-particle DEFAULT-PLAYER-SPEED (make-normalized-vector2 0 (- 1)))
   player-layer))

(define initial-world (make-world initial-player))

; ------------------------------
;;;;;;;;;;;;;;;;;;;;
;; UPDATE METHODS ;;
;;;;;;;;;;;;;;;;;;;;
;; For updating the
;; position of 
;; objects in the 
;; world etc..
;;;;;;;;;;;;;;;;;;;;
(define (update-world w)
  (make-world
   (update-sprite (world-player w))))

(define (update-sprite s)
  (let* ((particle (sprite-particle s))
         (dxy (scale-vector2 (particle-speed particle) (particle-direction particle)))
         (dx (vector2-x dxy))
         (dy (vector2-y dxy)))
  (make-sprite (+ dx (sprite-x s))
               (+ dy (sprite-y s))
               (sprite-particle s)
               (sprite-image s))))

; ------------------------------
;;;;;;;;;;;;;;;;;;;;
;; RENDER METHODS ;;
;;;;;;;;;;;;;;;;;;;;
(define (render-world w)
  (render-sprite (world-player w)
                 (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)))

(define (render-sprite s scene)
  (place-image (sprite-image s) (sprite-x s) (sprite-y s) scene))

; ------------------------------
;;;;;;;;;;;;;;;;;;;;
;; INPUT HANDLERS ;;
;;;;;;;;;;;;;;;;;;;;
(define (handle-mouse-input w x y mouse-event)
  (if (mouse=? "button-up" mouse-event)
      (make-world (set-sprite-heading (world-player w) x y))
      w))

; ------------------------------
;;;;;;;;;;;;;;;;;;;;
;; GAME EXECUTION ;;
;;;;;;;;;;;;;;;;;;;;
(big-bang 
 initial-world
 (on-tick update-world)
 (on-draw render-world)
 (on-mouse handle-mouse-input))