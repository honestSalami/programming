#lang racket
(provide (all-defined-out))
;;; A creature is an a-list of coordinates
;;; A live coordinate is the coordinate of a living cell
;;; Every coordinate in an a-list is a live coordinate
;;; No vector data type is used. The vectors are just ordered lists of numbers

(require 2htdp/image)
(require 2htdp/universe)

;; TRANSLATE a string code into living coordinates
(define (mkcreature str)
  (let loop ((y 0)
             (x 0)
             (shape (string->list str)))
    (cond ((null? shape)
           null)
          ((equal? #\d (car shape))
           (loop (+ y 1) 0 (cdr shape)))
          ((equal? #\e (car shape))
           (loop y (+ x 1) (cdr shape)))
          (else
           (cons (list y x)
                 (loop y (+ x 1) (cdr shape)))))))

;; MOVE a creature around the grid
(define (vector-add u v)
  (for/list ((i u)
             (j v))
    (+ i j)))

(define (teleport creature new-origin)
  (map (lambda (coordinate) 
         (vector-add new-origin coordinate))
       creature))

(define (virtual-origin creature)
    (list (apply min (map car creature))
          (apply min (map cadr creature))))

(define (backto-origin creature)
    (teleport creature
              (map - (virtual-origin creature))))

;; TURN several creatures into a single a-list of coordinates
(define (amalgamate . creatures)
  (apply append creatures))


;; Given an a-list of coordinates, make a grid
(define (mkgrid y x amalgam)
  (for/list ((ycoor (range y)))
    (for/list ((xcoor (range x)))
      (if (member (list ycoor xcoor) amalgam)
          1
          0))))

(define (height grid)
  (length grid))
(define (width grid)
  (length (list-ref grid 0)))

;; Get the value of a single coordinate on the grid
(define (grid-ref grid y x)
  (list-ref (list-ref grid y) x))

;; Get the value of SEVERAL coordinates, given as an a-list
(define (grid-polyref grid coords)
  (if (null? coords)
      null
      (let ((y (first (car coords)))
            (x (second (car coords))))
        (cons (grid-ref grid y x)
              (grid-polyref grid (cdr coords))))))

(define (-i+ i)
  (list (- i 1)
        i
        (+ i 1)))

; Is the coordinate y x outside the range of the grid?
(define (trespass grid y x)
  (if (or (< y 0)
          (< x 0)
          (>= y (height grid))
          (>= x (width grid)))
      #t
      #f))

; Destroy all coordinates outside the range of the grid.
(define (arrest grid coors)
  "under what charges? trespassing"
  (for/list ((i coors)
             #:when (not (trespass grid (car i) (cadr i))))
    i))

; How many of the neighbors of the y x coordinates are alive?
(define (neighbors grid y x)
  (- (apply + (grid-polyref grid (arrest grid (cartesian-product (-i+ y)
                                                                 (-i+ x)))))
     (grid-ref grid y x)))

; The rules of the game, IN LIST FORM!
(define conway '((0 0 0 1 0 0 0 0 0)
                 (0 0 1 1 0 0 0 0 0)))

; Will this cell live? or DIE? It all depends on its neighbors!
(define (death grid y x)
  (grid-ref conway (grid-ref grid y x)
                   (neighbors grid y x)))

; Is n equal to 1?
(define (one? n)
  (equal? n 1))

; After applying the rules, gimme the coordinates of the living cells
(define (survivors grid)
  (for*/list ((y (height grid))
              (x (width grid))
              #:when (one? (death grid y x)))
    (list y x)))

; Make the next state of the board
(define (generation grid)
  (mkgrid (height grid)
          (width grid)
          (survivors grid)))

(define (shooting grid turns)
  (reverse
   (for/fold ((grid (list grid)))
             ((i (range turns)))
     (cons (generation (car grid))
           grid))))

(define (develop grid)
  (let ((size 5))
    (for*/fold ((frame (rectangle (* size (width grid))
                                  (* size (height grid))
                                  'solid
                                  'white)))
               ((x (range (width grid)))
                (y (range (height grid)))
                #:when (one? (grid-ref grid y x)))
      (underlay/xy frame
                   (* x (+ 1 size))
                   (* y (+ 1 size))
                   (square (- size 2)
                           'solid
                           'black)))))

(define (develop-reel negatives)
  (map develop negatives))

(define (mkreel grid turns)
  (develop-reel (shooting grid turns)))

(define (run grid turns)
  (run-movie 0.075 (mkreel grid turns)))

; (mkreel (mkgrid 100 100 (teleport r-pentomino '(30 30))) 400)
; (mkreel (mkgrid 50 70 thunder) 500)
; (mkreel (mkgrid 100 100 (teleport (mkcreature gggun) '(10 50))) 100)
(define cross
  (let loop ([i 0] 
             [l '()])
    (if (>= i 80)
        (mkgrid 80 80 l)
        (loop (+ i 1) (cons (list i i)
                            (cons (list (- 79 i) i) 
                                  l))))))
 

(define blinker '((0 1) (1 1) (2 1)))
(define thunder '((30 19) (30 20) (30 21) (29 17) (30 17) (31 17)))
(define r-pentomino (mkcreature "ewwdwwedew"))
(define glider (mkcreature "ewdeewdwww"))
(define gggun
  (string-append 
    "eeeewwdeeeewwd"
    "dddddddd"
    "eeewwwdeeweeewdeweeeeewdeweeeeewdeeeewdeeweeewdeeewwwdeeeewd"
    "dd"
    "eeeeewwwdeeeeewwwdeeeeweeewddeeewweeewwd"
    "ddddddddd"
    "eeeeeewwdeeeeeeww"))

