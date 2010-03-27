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
(define DEFAULT-OFFICER-SPEED 3)

; ------------------------------
;;;;;;;;;;;;;;;;;;;;;
;; DATA STRUCTURES ;;
;;;;;;;;;;;;;;;;;;;;;
(define-struct vector2 (x y))
(define-struct particle (speed direction))
(define-struct sprite (x y particle image))
(define-struct world (game player officers))
(define-struct game (status))
(define-struct bounding-circle (x y radius))

; ------------------------------
;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;
(define (magnitude-vector2 v)
  (sqrt (+ (sqr (vector2-x v))
           (sqr (vector2-y v)))))

(define (normalize-vector2 v)
  (scale-vector2 (/ 1 (magnitude-vector2 v)) v))

(define (scale-vector2 n v)
  (make-vector2 (* n (vector2-x v))
                (* n (vector2-y v))))

(define (subtract-vector2 v1 v2)
  (make-vector2 (- (vector2-x v1) (vector2-x v2))
                (- (vector2-y v1) (vector2-y v2))))

(define (distance-between-bounding-circles b1 b2)
  (distance-between-vector2 (make-vector2 (bounding-circle-x b1) (bounding-circle-y b1))
                            (make-vector2 (bounding-circle-x b2) (bounding-circle-y b2))))

(define (distance-between-vector2 v1 v2)
  (magnitude-vector2 (subtract-vector2 v1 v2)))

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

(define (set-particle-speed p speed)
  (make-particle speed (particle-direction p)))

(define (stop-sprite s)
  (make-sprite (sprite-x s)
               (sprite-y s)
               (set-particle-speed 0 (sprite-particle s))
               (sprite-image s)))

(define (game-status-text str)
  (overlay (text str 26 "olive") (rectangle 200 60 "solid" "black")))

; This HAS to be already defined as something, but I can't find it :-/
(define (any? pred l)
  (if (null? l)
      #f
      (or (pred (car l)) (any? pred (cdr l)))))

; ------------------------------
;;;;;;;;;;;;;;;;;;;;
;; DEFAULT VALUES ;;
;;;;;;;;;;;;;;;;;;;;
(define player-layer (circle 10 "solid" "yellow"))
(define officer-layer (circle 10 "solid" "blue"))
(define (empty-scene w h) (rectangle w h "solid" (make-color 100 100 100)))
(define level-completed-text (game-status-text "You escaped!"))
(define player-caught-text (game-status-text "You got caught!"))
 

(define initial-player
  (make-sprite
   (/ SCREEN-WIDTH 2)
   (- SCREEN-HEIGHT (image-height player-layer))
   (make-particle DEFAULT-PLAYER-SPEED (make-normalized-vector2 0 (- 1)))
   player-layer))

(define initial-game (make-game 'player-escaping))

(define generate-officer
  (make-sprite (/ SCREEN-WIDTH 2)
               (/ (image-height officer-layer) 2)
               (make-particle DEFAULT-OFFICER-SPEED (make-normalized-vector2 0 1))
               officer-layer))

(define initial-world (make-world initial-game initial-player (list generate-officer)))

(define playable-area
  (let ((top (rectangle SCREEN-WIDTH (/ SCREEN-HEIGHT 2) "solid" (make-color 20 20 20)))
        (middle (rectangle (/ (* 3 SCREEN-WIDTH) 4) (* 2 (/ SCREEN-HEIGHT 5)) "solid" (make-color 20 20 20)))
        (gate (rectangle (/ SCREEN-WIDTH 3) 5 "solid" (make-color 20 20 20))))
    (above top (above gate middle))))

(define (set-level-complete w)
  (make-world (make-game 'player-escaped) (world-player w) (world-officers w)))

(define (set-player-caught w)
  (make-world (make-game 'player-caught) (world-player w) (world-officers w)))

(define (level-completed? w)
  (eq? (game-status (world-game w)) 'player-escaped))

(define (player-caught? w)
  (eq? (game-status (world-game w)) 'player-caught))

; ------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;
;; COLLISION DETECTION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sprite->bounding-circle s)
  (make-bounding-circle (sprite-x s) (sprite-y s) (/ (max (image-width (sprite-image s)) (image-height (sprite-image s))) 2)))

(define (collided? s1 s2)
  (let ((b1 (sprite->bounding-circle s1))
        (b2 (sprite->bounding-circle s2)))
    (<= (distance-between-bounding-circles b1 b2)
        (+ (bounding-circle-radius b1) (bounding-circle-radius b2)))))

; ------------------------------
;;;;;;;;;;;;;;;;;;;;
;; UPDATE METHODS ;;
;;;;;;;;;;;;;;;;;;;;
(define (update-world w)
  (cond ((or (player-caught? w) (level-completed? w)) w)
        ((>= 0 (sprite-y (world-player w))) (set-level-complete w))
        (#t (let* ((officers (map update-sprite (world-officers w)))
                   (player (update-sprite (world-player w)))
                   (world (make-world (world-game w) player officers)))
              (if (any? (lambda (o) (collided? player o)) officers)
                  (set-player-caught world)
                  world)))))

;; If the Sprite is going to collide with something in the next move, stop it moving.
;; Otherwise move it in it's heading at the speed it should be travelling.
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
  (let* ((main-image (overlay/align "middle" "top" playable-area (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)))
         (main-image-with-officers (foldl render-sprite main-image (world-officers w))))
    (render-sprite (world-player w)
                   (cond ((level-completed? w) (overlay level-completed-text main-image-with-officers))
                         ((player-caught? w) (overlay player-caught-text main-image-with-officers))
                         (#t main-image-with-officers)))))

(define (render-sprite s scene)
  (place-image (sprite-image s) (sprite-x s) (sprite-y s) scene))

; ------------------------------
;;;;;;;;;;;;;;;;;;;;
;; INPUT HANDLERS ;;
;;;;;;;;;;;;;;;;;;;;
(define (handle-mouse-input w x y mouse-event)
  (if (mouse=? "button-up" mouse-event)
      (make-world (world-game w) (set-sprite-heading (world-player w) x y) (world-officers w))
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