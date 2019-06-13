(defpackage :15
  (:use :common-lisp))
(in-package :15)

(defvar +side+ 4)
(defvar +max+ (1- (* +side+ +side+))) ; 15


; make a matrix of dimensions +side+ by +side+ (4 by 4 in this case)
; and fill it with numbers,
; starting from 1 and ending in the biggest number that fits
; in this case, 1 - 15, and ending in 0
; this is the structure:
; ((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 0))

; seriously, comment your code!
; its so much easier to understand this data structure,
; than to deduce the entire thing from code!
; all code should be commented with the data it returns...
; also, I hate keywords. they should be used sparingly.
(defun make-board ()
  ; make the empty matrix
  (make-array (list +side+ +side+)
              :initial-contents
              (loop :for i :below +side+ :collecting
                 (loop :for j :below +side+ :collecting

                    ; this reminds me of the illusion of time.
                    ; at any (j i) moment, the matrix does not exist, only the expression we are evaluating at different nesting levels
                    ; but, the best way to understand it is by looking at the effect it has on the entire matrix, as if it was that that we were modyfing.
                    ; but this simplification, is an "illusion". Or at least, its a different kind of real.
                    ; in the real real, there  are only single numbers that are being modified
                    ; but the final result is easier to see as if the aggregate of numbers was being modified...

                    ; why is it easier to see the entire structure than each number, one by one?
                    ; because the entire matrix is a single state, made of 4 substates with a simple creation rule
                    ;   start at 0, add 1 for each cell, make a new sublist each time you reach 4 cells.
                    ; but, each cell is its own cell, and you must remember 16 of them to see any pattern at all
                    ; so, the state of the entire structure is smaller (cognitively) than the state of each cell summed up
                    ; whe whole is smaller than the sum of its parts XD

                    (mod ; this add a 0 in the last cell ((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 0))
                      (1+ ; add 1 to each cell in the matrix ((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))
                       ; add j to the weighted row. this numbers each cell in the matrix uniquely
                       (+ j ; 0*4+(0, 1, 2, 3), 1*4+(0, 1, 2, 3), making a matrix like this: ((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15))
                          ; get the row number and multiply it by the col number
                          (* i +side+) ) ) ; 0*4= 0, 1*4= 4, 2*4= 8, 3*4= 12
                      (1+ +max+) )

                    ))))

(defvar *board* (make-board))

; move the pieces around randomly
(defun shuffle-board (board)
  ; I've seen this loop in 99 bottles of beer
  ; for i (range cell-count 2 -1)
  (loop for i from (array-total-size board) downto 2
        ; exchange the values of these two places
        ; exchange each succesive cell in board, from the first to the last,
        ; with some other random cell in the board
     do (rotatef (row-major-aref board (random i)) ; a random cell
                 (row-major-aref board (1- i)))) ; each succesive cell
  board)
; is it just me, or are there just way too many specialized functions?
; why would you use something as unintuitive as row-major-aref?
; well, the coding is simpler. You don't need to produce a 2D number to access the elements
; but understanding it is harder!

; https://www.youtube.com/watch?v=1ustNEgfqeo
; baggers say that row-major-ref is good for waking through an entire array
; without commiting to a dimensionality...
; that sounds like a good use, generality.
; it is hard to make general things anyways, this seems like a simple concept
; for generalizing dimensionality

; print board
(defun pb (stream object &rest args)

  ; maybe that the arguments can be not used in the body?
  ; so that the compiler does not complain
  ; https://stackoverflow.com/questions/9457103/how-do-i-ask-the-lisp-compiler-to-ignore-a-label-variety-function
  ; the value of args might not be read in this code
  ; whether args is a function or just vales, or when args is used...
  ; ¯\_(ツ)_/¯
  (declare (ignorable args))

  ; get the y size of the object (which is an array)
  (loop for i below (car (array-dimensions object)) do
        ; get the x size of the object
       (loop for j below (cadr (array-dimensions object)) do
             ; the cell in position (ij)
            (let ((cell (aref object i j)))
              ; print the board with the weird format rules, one cell at a time
              (format stream "(~[  ~:;~:*~2d~])" cell)))
       ; print a new line?
       (format stream "~%")))
        ;what is the value of the stream in (format stream ...)

; if any cell has the same number it was created with, return NIL
; else, the board is sorted, so return t
(defun sortedp (board)
  ; there might be no board passed
  (declare (ignorable board))
  ; from 0 to +max+, 15 in this case
  (loop for i upto +max+
        ; if any cell has stored in it the same value as its cell number, 
        ; then the board is not sorted.
        ; all cells must have a number different form their setup/default number
     when (eq (row-major-aref board i) (mod (1+ i) 16)) do ; that test would be better as a single function, with a clear name
       (return-from sortedp nil))
  t)

; return the number of times that 
; any number on the list is equal or bigger than any number after itself
; the range of values goes from (sum (-1 (length lst))) to 0
(defun inversions (lst)
  ; base case,
  ; if it is over, or one more iteration from over
  ; stop, and return 0
  (if (or (null lst) (null (cdr lst)))
      0
      ; sequential binding
      (let* 
        (
         ; take the length of half the list, round up to the nearest integer
         (half (ceiling (/ (length lst) 2)))
         ; make the left of the list
         (left-list (subseq lst 0 half))
         ; make the right of the list
         (right-list (subseq lst half)) )
        ; add all the times where any element of the list is bigger than any
        ; other element of the list?
        (+ 
          ; sum all the elements in the left that are 
          ; bigger or equal to those on the right
          (loop for a in left-list
              summing 
              ; count all the values where a is >= than b,
              ; that is, all values where each element in the left of the list is
              ; bigger or equal than any element in the right of the list
              (loop for b in right-list 
                    counting 
                    (not (< a b)) ) )
          ; do the same for the sublists
          (inversions left-list) 
          (inversions right-list) ) ) ) )

; memory and pattern recognition are different abilities.
; you can offload memory to the computer, by writing lots of comments
; that remember what each line does for you
; and then YOU figure out the patterns by comparing the comments

; a program is 3 things:
; state, procedure, and process.
; you visualize the state, imagine how the procedure gives rise to the process,
; and you understand the program.

(defun solvablep (board)
  (let ((inv (inversions (loop for i upto +max+ collecting
                              (row-major-aref board i))))
        (row (- +side+ (first (board-position board 0)))))
    (or (and (oddp +side+)
             (evenp inv))
        (and (evenp +side+)
             (evenp row)
             (oddp inv))
        (and (evenp +side+)
             (oddp row)
             (evenp inv)))))

(defun board-position (board dig)
  (loop for i below (car (array-dimensions board)) do
       (loop for j below (cadr (array-dimensions board))
          when (eq dig (aref board i j)) do
          (return-from board-position (list i j)))))

(defun in-bounds (y x)
  (and (< -1 y +side+)
       (< -1 x +side+)))

(defun get-adjacents (board pos)
  (let ((adjacents ()) (y (first pos)) (x (second pos)))
    (if (in-bounds y (1+ x))
        (push (aref board y (1+ x)) adjacents))
    (if (in-bounds (1+ y) x)
        (push (aref board (1+ y) x) adjacents))
    (if (in-bounds y (1- x))
        (push (aref board y (1- x)) adjacents))
    (if (in-bounds (1- y) x)
        (push (aref board (1- y) x) adjacents))
    adjacents))

(defun main (&rest argv)
  (declare (ignorable argv))
  (setf *random-state* (make-random-state t))
  (loop until (solvablep *board*) do
       (shuffle-board *board*))
  (loop until (sortedp *board*) do
       (format t "~/15:pb/~%" *board*)
       (format t "Which number do you want to swap the blank with?~%> ")
       (let* ((in (read))
              (zpos (board-position *board* 0))
              (pos (board-position *board* in))
              (adj (get-adjacents *board* zpos)))
         (if (find in adj)
             (rotatef (aref *board* (first pos) (second pos))
                      (aref *board* (first zpos) (second zpos))))))
  (format t "You win!~%"))
