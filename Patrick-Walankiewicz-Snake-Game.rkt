;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Patrick-Walankiewicz-Snake-Game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Patrick Walankiewicz

(require 2htdp/image)
(require 2htdp/universe)


(define HEIGHT 20)
(define WIDTH 30)
(define CELL-SIZE 15)
(define BACKGROUND (empty-scene (* WIDTH CELL-SIZE) (* HEIGHT CELL-SIZE)))

(define SNAKE-SEG (circle 7.5 'solid 'blue))
(define FOOD (circle 7.5 'solid 'teal))

;;[image, number, number, scene] -> [image]
;;Places image onto the grid
(define (place-image/grid obj x y bg)
  (place-image
   obj
   (+ (* x CELL-SIZE) (/ CELL-SIZE 2))
   (+ (* y CELL-SIZE) (/ CELL-SIZE 2))
   bg))

(check-expect (place-image/grid FOOD 10 0 BACKGROUND)
              (place-image FOOD 157.5 7.5 BACKGROUND))

(define (end w)
  (place-image/grid (text (string-append "Your Score is: " (number->string (length (snake-body (world-snake w))))) 40 'black)
                              15
                              10
                              BACKGROUND))

;;(define-struct world [Snake Posn Number]
;;Interpretation:
;;               Holds the snake structure, position of the food, and timer for snake turns
(define-struct world [snake food turn])

;;(defince-struct snake [String (list-of Posn)]
;;Interpretation:
;;               Holds the direction the snake is going and
;;               a list of positions of the snakes body
(define-struct snake [dir body])


(define (main w0)
  (big-bang (make-world
             (make-snake "right"
                         (list (make-posn 10 2)
                               (make-posn 9 2)
                               (make-posn 8 2)))
             (make-posn (random 29) (random 19))
             1)
            [on-tick update .1]
            [to-draw render]
            [on-key change]
            [stop-when stop end]))

;;[world, Key] -> [world]
;;Takes in a world and a key and returns a new world
;;based on what key was pressed
(define (change w key)
  (cond
    [(and (string=? key "left")
          (not (string=? (snake-dir (world-snake w)) "right"))
          (= (world-turn w) 1))
     (make-world
      (make-snake "left" (snake-body (world-snake w)))
      (world-food w)
      0)]
    [(and (string=? key "right")
          (not (string=? (snake-dir (world-snake w)) "left"))
          (= (world-turn w) 1))
     (make-world
      (make-snake "right" (snake-body (world-snake w)))
      (world-food w)
      0)]
    [(and (string=? key "up")
          (not (string=? (snake-dir (world-snake w)) "down"))
          (= (world-turn w) 1))
     (make-world
      (make-snake "up" (snake-body (world-snake w)))
      (world-food w)
      0)]
    [(and (string=? key "down")
          (not (string=? (snake-dir (world-snake w)) "up"))
          (= (world-turn w) 1))
     (make-world
      (make-snake "down" (snake-body (world-snake w)))
      (world-food w)
      0)]
    [else w]))

(check-expect (change (make-world (make-snake "up" (list (make-posn 2 2)))
                                  (make-posn 2 3)
                                  1)
                      "left")
              (make-world (make-snake "left" (list (make-posn 2 2)))
                          (make-posn 2 3)
                          0))

(check-expect (change (make-world (make-snake "up" (list (make-posn 2 2)))
                                  (make-posn 2 3)
                                  1)
                      "right")
              (make-world (make-snake "right" (list (make-posn 2 2)))
                          (make-posn 2 3)
                          0))

(check-expect (change (make-world (make-snake "right" (list (make-posn 2 2)))
                                  (make-posn 2 3)
                                  1)
                      "up")
              (make-world (make-snake "up" (list (make-posn 2 2)))
                          (make-posn 2 3)
                          0))

(check-expect (change (make-world (make-snake "right" (list (make-posn 2 2)))
                                  (make-posn 2 3)
                                  1)
                      "down")
              (make-world (make-snake "down" (list (make-posn 2 2)))
                          (make-posn 2 3)
                          0))

(check-expect (change (make-world (make-snake "left" (list (make-posn 2 2)))
                                  (make-posn 2 3)
                                  0)
                      " ")
              (make-world (make-snake "left" (list (make-posn 2 2)))
                          (make-posn 2 3)
                          0))


;;[world] -> [world]
;;Takes in a world and updates it based on the conditions
(define (update w)
  (cond
    [(food-contact? w) (make-world (add-segment (world-snake w))
                                   (make-posn (random 29) (random 19))
                                   1)]
    [else (make-world  
           (add-segment (make-snake
                          (snake-dir (world-snake w))(drop-last (snake-body (world-snake w)))))
           (world-food w)
           1)]))

(check-expect (update (make-world
                       (make-snake "left" (list (make-posn 3 3)
                                                (make-posn 3 4)
                                                (make-posn 3 5)))
                       (make-posn 6 6)
                       1))
              (make-world
               (make-snake "left" (list (make-posn 2 3)
                                        (make-posn 3 3)
                                        (make-posn 3 4)))
               (make-posn 6 6)
               1))

;;[world] -> [boolean]
;;Checks if the head of the snake has made contact with the food
(define (food-contact? w)
  (cond
    [(and (= (posn-x (first (snake-body (world-snake w))))
             (posn-x (world-food w)))
          (= (posn-y (first (snake-body (world-snake w))))
             (posn-y (world-food w))))
     #true]
    [else #false]))

(check-expect (food-contact? (make-world
                              (make-snake "left" (list (make-posn 3 3)
                                                       (make-posn 3 4)))
                              (make-posn 3 3)
                              1))
              #true)

(check-expect (food-contact? (make-world
                              (make-snake "left" (list (make-posn 3 3)
                                                       (make-posn 3 4)))
                              (make-posn 3 4)
                              1))
              #false)

(check-expect (food-contact? (make-world
                              (make-snake "left" (list (make-posn 3 3)
                                                       (make-posn 3 4)))
                              (make-posn 5 6)
                              1))
              #false)

;;[Snake] -> [Snake]
;;Takes in a snake and adds a new segment to it
(define (add-segment s)
  (cond
    [(string=? "left" (snake-dir s))
     (make-snake "left"
                 (cons (make-posn (- (posn-x (first (snake-body s))) 1)
                                  (posn-y (first (snake-body s))))
                       (snake-body s)))]
    [(string=? "right" (snake-dir s))
     (make-snake "right"
                 (cons (make-posn (+ (posn-x (first (snake-body s))) 1)
                                  (posn-y (first (snake-body s))))
                       (snake-body s)))]
    [(string=? "up" (snake-dir s))
     (make-snake "up"
                 (cons (make-posn (posn-x (first (snake-body s)))
                                  (- (posn-y (first (snake-body s))) 1))
                       (snake-body s)))]
    [(string=? "down" (snake-dir s))
     (make-snake "down"
                 (cons (make-posn (posn-x (first (snake-body s)))
                                  (+ (posn-y (first (snake-body s))) 1))
                       (snake-body s)))]))

(check-expect (add-segment (make-snake "left" (list (make-posn 10 10)
                                                    (make-posn 11 10)
                                                    (make-posn 12 10))))
              (make-snake "left" (list (make-posn 9 10)
                                       (make-posn 10 10)
                                       (make-posn 11 10)
                                       (make-posn 12 10))))

(check-expect (add-segment (make-snake "right" (list (make-posn 10 10)
                                                     (make-posn 9 10)
                                                     (make-posn 8 10))))
              (make-snake "right" (list (make-posn 11 10)
                                        (make-posn 10 10)
                                        (make-posn 9 10)
                                        (make-posn 8 10))))

(check-expect (add-segment (make-snake "up" (list (make-posn 10 10)
                                                  (make-posn 10 11)
                                                  (make-posn 10 12))))
              (make-snake "up" (list (make-posn 10 9)
                                     (make-posn 10 10)
                                     (make-posn 10 11)
                                     (make-posn 10 12))))

(check-expect (add-segment (make-snake "down" (list (make-posn 10 12)
                                                    (make-posn 10 11)
                                                    (make-posn 10 10))))
              (make-snake "down" (list (make-posn 10 13)
                                       (make-posn 10 12)
                                       (make-posn 10 11)
                                       (make-posn 10 10))))

;;[List-of Posn] -> [List-of Posn]
;;Takes in a list of posns and returns that list excluding the last posn
(define (drop-last s)
  (cond
    [(empty? (rest s)) empty]
    [else (cons (first s) (drop-last (rest s)))]))

(check-expect (drop-last (list (make-posn 2 5)
                               (make-posn 2 6)
                               (make-posn 2 7)))
              (list (make-posn 2 5)
                    (make-posn 2 6)))

;;[world] -> [boolean]
;;Takes in a world and checks to see if the snake is dead
(define (stop w)
  (cond
    [(or (< (posn-x (first (snake-body (world-snake w)))) 0)
         (> (posn-x (first (snake-body (world-snake w)))) 29)
         (< (posn-y (first (snake-body (world-snake w)))) 0)
         (> (posn-y (first (snake-body (world-snake w)))) 19))
     #true]
    [(hit-itself? w) #true]
    [else #false]))

(check-expect (stop (make-world
                     (make-snake "left" (list (make-posn -1 2)))
                     (make-posn 6 6)
                     1))
              #true)

(check-expect (stop (make-world
                     (make-snake "left" (list (make-posn 30 2)))
                     (make-posn 6 6)
                     1))
              #true)

(check-expect (stop (make-world
                     (make-snake "left" (list (make-posn 6 -1)))
                     (make-posn 6 6)
                     1))
              #true)

(check-expect (stop (make-world
                     (make-snake "left" (list (make-posn 6 20)))
                     (make-posn 6 6)
                     1))
              #true)

(check-expect (stop (make-world
                     (make-snake "left" (list (make-posn 0 0)))
                     (make-posn 6 6)
                     1))
              #false)

;;[world] -> [boolean]
;;Takes in a world and checks to see if the snake has it itself
(define (hit-itself? w)
  (ormap (lambda (s) (and (= (posn-x s)
                             (posn-x (first (snake-body (world-snake w)))))
                          (= (posn-y s)
                             (posn-y (first (snake-body (world-snake w)))))))
         (rest (snake-body (world-snake w)))))

(check-expect (hit-itself? (make-world
                            (make-snake "left" (list (make-posn 5 6)
                                                     (make-posn 6 6)
                                                     (make-posn 5 6)))
                            (make-posn 7 8)
                            1))
              #true)

;;[world] -> [image]
;;Takes in a world and returns the image of the world
(define (render w)
  (local (;[Posn, scene] -> [image]
          ;Takes in a posn and a scene and draws a segment
          ;on the scene at that posn
          (define (draw-one seg scene)
            (place-image/grid
             SNAKE-SEG
             (posn-x seg)
             (posn-y seg)
             scene)))
    (foldr draw-one (draw-food w) (snake-body (world-snake w)))))

;;[world] -> [image]
;;Takes in a world a places the food onto an empty scene
(define (draw-food w)
  (place-image/grid
   FOOD
   (posn-x (world-food w))
   (posn-y (world-food w))
   BACKGROUND))

(main 0)
  






