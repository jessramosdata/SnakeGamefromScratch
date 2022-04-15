;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Snake Game|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)

(define width 480)
(define height 336)
(define cellsize 24)
(define scene (empty-scene width height "black"))


(define food (circle (/ cellsize 2) "solid" "yellow")) 
(define snake-live (circle (/ cellsize 2) "solid" "green"))
(define snake-dead (circle (/ cellsize 2) "solid" "pink" ))



;; snake-length : Snake -> Number
;; produces the number of segments in the snake

(check-expect (snake-length fatty) 2)

(define (snake-length a-snake)
  (length (snake-body a-snake)))


;; ***********************************
;; A List-of-posns is either:
;;  - empty
;;  - (cons Posn List-of-posns)
; !!! two arrows in this data definition !!! <--- note

; example:
(define LOP1 (cons (make-posn 4 5) (cons (make-posn 0 0) (cons (make-posn 10 2) empty))))

;; template
;(define (lop-func a-lop)
;  (cond [(empty? a-lop) ...]
;        [(cons? a-lop) (... (posn-func (first a-lop) ...)
;                            (lop-func (rest a-lop) ...) ...)]))




; A snake is a (make-snake List-of-Posn String Boolean)
;; interp. by the head being the posn on the end closest to the direction given
(define-struct snake (body direct dead?))
(define slim (make-snake (list (make-posn 40 60) (make-posn 40 80)) "right" false))
(define fatty (make-snake (list (make-posn 20 80) (make-posn 20 60)) "up" true))
(define boaboy (make-snake (list (make-posn 100 500) (make-posn 100 480)) "down" false))
(define noodle (make-snake (list (make-posn 500 400) (make-posn 480 400)) "left" false))
(define pasta (make-snake (list (make-posn 100 60) (make-posn 120 60) (make-posn 140 60)
                                (make-posn 140 80) (make-posn 120 80) (make-posn 100 80)
                                (make-posn 100 60) (make-posn 100 40)) "up" false))

;template
;(define (snake-func a-snake)
;  (... (lop-func (snake-body a-snake))
;       ...(snake-direct a-snake)
;       ...(snake-dead? a-snake)))



;A game is a (make-game snake List-of-posns Number Boolean)
;; interp. by a snake (defined above), foods (List-of-posns, and the number of
;ticks that have passed since the game started   
(define-struct game (snake foods ticks paused?))
(define game1 (make-game slim (list (make-posn 80 20) (make-posn 60 140))  45 false))
(define game2 (make-game fatty (list (make-posn 20 20) (make-posn 20 80)) 99 false))
(define game3 (make-game pasta (list (make-posn 80 20) (make-posn 60 140) (make-posn 20 20)) 88 false))
(define game4 (make-game noodle (list (make-posn 80 20) (make-posn 60 140) (make-posn 20 20)) 76 false))
(define game5 (make-game (make-snake (list (make-posn 20 20) (make-posn 40 20)) "right" false)
                         (list (make-posn 60 60) (make-posn 80 100)) 45 false))

;template
;(define (game-func a-game)
;  (... (snake-func (game-snake a-game))
;       ...(lop-func (game-foods a-game))
;       ...(game-ticks a-game)))


;Posns will represent snake and food location 
;ticks is a time counter that keeps track of how long the game has been going on



;render-game:Game ->Image
;makes an image from a game

(check-expect (render-game game1 )
              (render-snake (game-snake game1) 
                            (render-food (game-foods game1) 
                                         (render-score (score game1) scene)))) 

(define (render-game a-game)
  (overlay (if (game-over? a-game)
               (text "GAME OVER" 48 "red")
               (circle 0 "solid" "white"))
           (render-snake (game-snake a-game)
                         (render-food (game-foods a-game) 
                                      (render-score (score a-game) scene)))))
  


;render-snake: snake Image(scene)->Image
;puts a sname on an empty scene

(check-expect (render-snake slim (empty-scene 100 100))
              (place-image snake-live
                           40
                           60
                           (place-image snake-live
                                        40
                                        80
                                        (empty-scene 100 100))))

(check-expect (render-snake fatty scene)
              (place-image snake-dead
                           20
                           80
                           (place-image snake-dead
                                        20
                                        60
                                        scene)))

(define (render-snake a-snake bkg)
  (render-body (snake-body a-snake) (snake-dead? a-snake) bkg)) 


;; render-body : List-of-posn Boolean Image -> Image
(check-expect (render-body (list (make-posn 20 80) (make-posn 20 100))
                           true
                           (empty-scene 100 200))
              (place-image snake-dead
                           20
                           80
                           (place-image snake-dead
                                        20
                                        100
                                        (empty-scene 100 200))))

(check-expect (render-body (list (make-posn 20 80) (make-posn 20 100) (make-posn 20 120))
                           false
                           (empty-scene 100 200))
              (place-image snake-live
                           20
                           80
                           (place-image snake-live
                                        20
                                        100
                                        (place-image snake-live
                                                     20
                                                     120
                                                     (empty-scene 100 200)))))

                           
(define (render-body a-lop dead? bkg)
(foldr (lambda (x y) (if dead?
                           (place-image snake-dead
                                        (posn-x (first a-lop))
                                        (posn-y (first a-lop))
                                        (render-body (rest a-lop) dead? bkg))

                           (place-image snake-live
                                        (posn-x (first a-lop))
                                        (posn-y (first a-lop))
                                        (render-body (rest a-lop) dead? bkg))))
       bkg
       a-lop))



;render-food: List-of-food Image-> Image
;places food at the correct location on the scene

(check-expect (render-food (list (make-posn 45 66) (make-posn 431 110) (make-posn 222 345)) scene)
              (place-image food 45 66 (place-image food 431 110 (place-image food 222 345 scene))))

(check-expect (render-food (list (make-posn 99 66) (make-posn 110 37) (make-posn 444 217) (make-posn 444 27)) scene)
              (place-image food 99 66 (place-image food 110 37 (place-image food 444 217 (place-image food 444 27 scene)))))

(check-expect (render-food empty scene) scene)

(check-expect (render-food (list (make-posn 99 66)) scene) (place-image food 99 66 scene))





(define (render-food a-lof a-scene)
  (foldr (lambda (x y)
                (place-image food (posn-x (first a-lof)) (posn-y (first a-lof))
                  (render-food (rest a-lof) a-scene)))
                a-scene
                a-lof)) 



;render-score: Number (score) Image-> Image
;places the score on the scene

(check-expect (render-score 444 scene) (place-image (text (number->string 444) 48 "blue") 384 67.2 scene))
(check-expect (render-score 19 (empty-scene 100 100)) (place-image (text (number->string 19) 10 "blue") 80 20 (empty-scene 100 100)))

(define (render-score score a-scene) 
  (place-image (text (number->string score) (/ (image-width a-scene) 10) "blue")
               (* (/ 4 5) (image-width a-scene)) (/ (image-height a-scene) 5) a-scene))   
  


;; score : Game -> Number
;; produces the score for the current state of the game
 
(define (score a-game)
  (- (* 25 (snake-length (game-snake a-game)))
     (game-ticks a-game)))


;tick : Game -> Game
;produces ticks from a number, ;check is game paused,

(check-expect (tick game1)
              (move-snake game1 "right"))
               


(define (tick a-game)
(if (game-paused? a-game)
      a-game
      (move-snake a-game (snake-direct (game-snake a-game)))))



;run-into?: Snake -> Boolean
;determines if the snake has run into a wall or itself

(check-expect (run-into? boaboy) true)
(check-expect (run-into? noodle) true)
(check-expect (run-into? pasta) true)
(check-expect (run-into? slim) false)


(define (run-into? a-snake)
  (cond
    [(or (boolean=? true (not (distinct? (snake-body a-snake))))
         (boolean=? true (wall? a-snake))) 
     true] 
    [else false]))





;;distinct?: List-of-Posn-> Boolean
;; determines if the list of posns contain all distinct values

(check-expect (distinct? (list (make-posn 100 50) (make-posn 101 50) (make-posn 102 50)
                               (make-posn 102 51) (make-posn 102 52) (make-posn 101 52)
                               (make-posn 100 52) (make-posn 100 51) (make-posn 100 50)
                               (make-posn 100 49))) false)
(check-expect (distinct? (list (make-posn 100 50) (make-posn 101 50) (make-posn 102 50)
                               (make-posn 102 51) (make-posn 102 52) (make-posn 101 52)
                               (make-posn 100 52) (make-posn 100 51) (make-posn 100 49))) true)
(check-expect (distinct? empty) true)
(check-expect (distinct? (list (make-posn 100 50) (make-posn 101 50) (make-posn 101 50)))
              false) 



(define (distinct? a-lop)
  (andmap (lambda (b-posn)
            (= 1 (length (filter (lambda (target-posn)(posn=? target-posn b-posn)) a-lop)))) a-lop))   

;  (local
;    [
;     ;only-once?: Posn->Boolean
;     ; determines whether the target-posn occurs exactly once in a-lop
;     (define (only-once? target-posn)
;       (lambda (b-posn) (= 1 (length (filter (posn=? target-posn b-posn) a-lop)))))
;     ;;;;(posn=? a-posn b-posn))
;     ]  

;(cond 
;    [(empty? a-lop) true] 
;    [(= (length a-lop) 1) true]
;    [(cons? a-lop) (if (member (first a-lop) (rest a-lop))
;                       false
;                       true)]))  
                                   
                   

;posn=?: Posn Posn -> Boolean
;determines if 2 posns are equal

(check-expect (posn=? (make-posn 3 4) (make-posn 3 4)) true)
(check-expect (posn=? (make-posn 2 4) (make-posn 3 4)) false) 

(define (posn=? posn1 posn2)
  (and (= (posn-x posn1) (posn-x posn2)) (= (posn-y posn1) (posn-y posn2))))

;wall?: Snake -> Boolean
; determines if a snake has run into a wall

(check-expect (wall? slim) false)
(check-expect (wall? noodle) true)
(check-expect (wall? boaboy) true)

(define (wall? a-snake)
  (cond
    [(or (<= (posn-x (first (snake-body a-snake))) 0)
         (>= (posn-x (first (snake-body a-snake))) width)) true]
    [(or (<= (posn-y (first (snake-body a-snake))) 0)
         (>= (posn-y (first (snake-body a-snake))) height)) true]
    [else false]))


;change: Game KeyEvent->Game
;changes the direction of the snake

(check-expect (change game1 "left") (make-game (make-snake (list (make-posn 40 60) (make-posn 40 80)) "left" false)
                                              (game-foods game1) (game-ticks game1) (game-paused? game1)))
(check-expect (change game2 "down") (make-game (make-snake (list (make-posn 20 80) (make-posn 20 60)) "down" true)
                                               (game-foods game2) (game-ticks game2) (game-paused? game2)))
(check-expect (change game2 "a") game2)

(define (change a-game key)
  (cond
    [(or (string=? "up" key) (string=? "down" key) (string=? "left" key) (string=? "right" key))
  (make-game (make-snake  (snake-body (game-snake a-game)) key (snake-dead? (game-snake a-game))) 
             (game-foods a-game)
             (game-ticks a-game)
             (game-paused? a-game))]
 [(string=? " " key) (make-game (game-snake a-game)
                                (game-foods a-game)
                                (game-ticks a-game)
                                (not(game-paused? a-game)))]
 [else a-game]))
;same game but change pause to opp of what paused? was



;game-over?: Game -> Boolean
;determines whether the game is over or not

(check-expect (game-over? game1) false)
(check-expect (game-over? game2) true)
(check-expect (game-over? game3) false)
(check-expect (game-over? game4) true)

(define (game-over? a-game)
  (or (> 0 (score a-game)) (snake-dead? (game-snake a-game))))

 ; (if (or (boolean=? true (wall? (game-snake a-game)))
 ;         (boolean=? false (distinct? (snake-body (game-snake a-game)))))
 ;     true
 ;     false)) 


;move-direction: Snake Key->Snake
;moves the snake in the given direction

(define move-distance 20)

(check-expect (move-direction slim "right")
              (make-snake (list (make-posn 60 60) (make-posn 40 60)) "right" false))

(check-expect (move-direction (make-snake (list (make-posn 1 30) (make-posn 1 50)) "up" true) "up")
              (make-snake (list (make-posn 1 10) (make-posn 1 30)) "up" true))

(check-expect (move-direction (make-snake (list (make-posn 1 50) (make-posn 1 30)) "down" true) "down")
              (make-snake (list (make-posn 1 70) (make-posn 1 50)) "down" true))

(check-expect (move-direction (make-snake (list (make-posn 50 50) (make-posn 50 30)) "left" true) "left")
              (make-snake (list (make-posn 30 50) (make-posn 50 50)) "left" true))

 

(define (move-direction a-snake a-key) 
  (cond 
    [(string=? "up" a-key)
     (make-snake (cons (make-posn (posn-x (first (snake-body a-snake))) 
                      (- (posn-y (first (snake-body a-snake))) move-distance))
           (reverse (rest (reverse (snake-body a-snake)))))
                 (snake-direct a-snake) (snake-dead? a-snake))]
    
    [(string=? "down" a-key)
     (make-snake (cons (make-posn (posn-x (first (snake-body a-snake)))
                      (+ (posn-y (first (snake-body a-snake))) move-distance))
           (reverse (rest (reverse (snake-body a-snake)))))
                 (snake-direct a-snake) (snake-dead? a-snake))]
    
    [(string=? "right" a-key)
     (make-snake (cons (make-posn (+ (posn-x (first (snake-body a-snake))) move-distance)
                      (posn-y (first (snake-body a-snake))))
           (reverse (rest (reverse (snake-body a-snake)))))
                 (snake-direct a-snake) (snake-dead? a-snake))] 
    
    [(string=? "left" a-key)
     (make-snake (cons (make-posn (- (posn-x (first (snake-body a-snake))) move-distance)
                      (posn-y (first (snake-body a-snake))))
           (reverse (rest (reverse (snake-body a-snake)))))
                 (snake-direct a-snake) (snake-dead? a-snake))]
    [else a-snake]))


;;pause: Game -> Game
; pauses the game by pressing the spacebar

;(check-expect (pause game1)

;(define (pause a-game)
;) 






;move-snake: Game Key->Game
; moves the snake in a game in the correct direction depending on if the head is on a food posn or not

(check-expect (move-snake game1 "right")
              (make-game (make-snake (list (make-posn 60 60) (make-posn 40 60)) "right" false)
                         (list (make-posn 80 20) (make-posn 60 140)) 46 false))

(check-expect (move-snake (make-game (make-snake (list (make-posn 1 30) (make-posn 1 50)) "up" true)
                                     (list (make-posn 80 30) (make-posn 50 140)) 55 false) "up")
              (make-game (make-snake (list (make-posn 1 10) (make-posn 1 30)) "up" true)
                         (list (make-posn 80 30) (make-posn 50 140)) 56 false))

(check-expect (move-snake (make-game (make-snake (list (make-posn 1 50) (make-posn 1 30)) "down" true)
                                   (list (make-posn 99 444) (make-posn 44 22)) 99 false)  "down")
              (make-game (make-snake (list (make-posn 1 70) (make-posn 1 50)) "down" true)
                         (list (make-posn 99 444) (make-posn 44 22)) 100 false))

(check-expect (move-snake (make-game (make-snake (list (make-posn 50 50) (make-posn 50 30)) "left" true)
                                     (list (make-posn 4 222) (make-posn 334 66)) 99 false) "left")
              (make-game (make-snake (list (make-posn 30 50) (make-posn 50 50)) "left" true)
                         (list (make-posn 4 222) (make-posn 334 66)) 100 false))

(check-random (move-snake (make-game (make-snake (list (make-posn 50 50) (make-posn 50 30)) "left" true)
                                     (list (make-posn 50 50) (make-posn 334 66)) 99 false) "left")
              (make-game (make-snake (list (make-posn 30 50) (make-posn 50 50) (make-posn 50 30)) "left" true)
                         (list (random-posn width height cellsize) (make-posn 334 66)) 100 false))



(define (move-snake a-game a-key)
  (cond
    [(member? (snake-head (game-snake a-game)) (game-foods a-game))
              (make-game (check/make-dead (move-if-eats(game-snake a-game) a-key))
                                         (cons (random-posn width height cellsize) (remove-food (game-foods a-game)
                                                                                        (snake-head (game-snake a-game))))
                                               (add1 (game-ticks a-game)) (game-paused? a-game))]
     [else (make-game (check/make-dead (move-direction (game-snake a-game) a-key))
                                      (game-foods a-game) (add1 (game-ticks a-game)) (game-paused? a-game))]))
   

;; check/make-dead : Snake -> Snake

(check-expect (check/make-dead slim) (make-snake (snake-body slim) (snake-direct slim) false))
(check-expect (check/make-dead pasta) (make-snake (snake-body pasta) (snake-direct pasta) true))

(define (check/make-dead a-snake)
   (if (run-into? a-snake)
       (make-snake (snake-body a-snake) (snake-direct a-snake) true)
  a-snake))



;move-if-eats:  Snake Key-> Snake
;moves the snake in the given direction


(check-expect (move-if-eats (make-snake (list (make-posn 1 30) (make-posn 1 50)) "up" true) "up")
              (make-snake (list (make-posn 1 10) (make-posn 1 30) (make-posn 1 50)) "up" true))

(check-expect (move-if-eats (make-snake (list (make-posn 1 50) (make-posn 1 30)) "down" true) "down")
              (make-snake (list (make-posn 1 70) (make-posn 1 50) (make-posn 1 30)) "down" true))

(check-expect (move-if-eats (make-snake (list (make-posn 1 30) (make-posn 1 50)) "up" true) "right")
              (make-snake (list (make-posn 21 30) (make-posn 1 30) (make-posn 1 50)) "up" true)) 



(define (move-if-eats a-snake a-key) 
  (cond 
    [(string=? "up" a-key)
     (make-snake (cons (make-posn (posn-x (first (snake-body a-snake)))
                      (- (posn-y (first (snake-body a-snake))) 20))
                  (snake-body a-snake))
                 (snake-direct a-snake) (snake-dead? a-snake))] 
    
    [(string=? "down" a-key)
     (make-snake (cons (make-posn (posn-x (first (snake-body a-snake)))
                      (+ (posn-y (first (snake-body a-snake))) 20))
                       (snake-body a-snake))
                 (snake-direct a-snake) (snake-dead? a-snake))]
    
    [(string=? "right" a-key)
     (make-snake (cons (make-posn (+ (posn-x (first (snake-body a-snake))) 20)
                      (posn-y (first (snake-body a-snake))))
                       (snake-body a-snake))
                 (snake-direct a-snake) (snake-dead? a-snake))] 
    
    [(string=? "left" a-key)
     (make-snake (cons (make-posn (- (posn-x (first (snake-body a-snake))) 20)
                      (posn-y (first (snake-body a-snake))))
                       (snake-body a-snake))
                 (snake-direct a-snake) (snake-dead? a-snake))]))




     
;snake-head: Snake->Posn
;gives the the posn of a snake's head from a snake

(check-expect (snake-head slim) (make-posn 40 60))
(check-expect (snake-head fatty) (make-posn 20 80))
(check-expect (snake-head boaboy) (make-posn 100 500))

(define (snake-head a-snake)
  (first (snake-body a-snake)))

;remove-food: ListofPosn Posn-> ListofPosn
;removes the given posn from the list of posn

(check-expect (remove-food (list (make-posn 4 5) (make-posn 1 2)) (make-posn 1 2))
              (list (make-posn 4 5)))

(define (remove-food a-lop a-posn) 
  (filter (lambda (x) (not (posn=? x a-posn)))
            a-lop))


;op-direct: String->String
; produces the opposite direction

(check-expect (op-direct "right") "left")
(check-expect (op-direct "left") "right")
(check-expect (op-direct "up") "down")
(check-expect (op-direct "down") "up")

(define (op-direct a-dir)
  (cond
    [(string=? a-dir "right") "left"]
    [(string=? a-dir "left") "right"]
    [(string=? a-dir "up") "down"]
    [(string=? a-dir "down") "up"])) 


;; random-posn : Number Number Number -> Posn
;; to invent a new location for a piece of food on a board
;; of size (w,h) where the snake's segment size is cell-size

(define (random-posn w h cell-size)
  (make-posn (+ cell-size (* cell-size (random (- (/ w  cell-size) 1))))
             (+ cell-size (* cell-size (random (- (/ h cell-size) 1))))))



;run: Game -> Image
;creates a game with a snake, food, and ticks

(define (run a-game)
  (big-bang a-game
    (on-key  change)
    (on-tick tick 1/8)
    (to-draw render-game)
    (stop-when game-over? render-game)))

(define NEW-GAME (make-game (make-snake (list (make-posn 240 240) (make-posn 240 260)
                                              (make-posn 240 280)) "up" false)
                            (list (make-posn 40 20) (make-posn 320 400) (make-posn 320 100)
                                  (make-posn 20 420) (make-posn 80 200)) 0 false))  
 

(run NEW-GAME)
