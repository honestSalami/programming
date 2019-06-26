
(defun our-member (obj lst)
  (if (null lst) ; base case
    nil
    (if (eql obj (car lst)) ; is member
      lst
      (our-member obj (cdr lst)) ) ) ) ; recurse

(defun askem (string)
  (format t "~A" string)
  ;; does not work if you insert a space
  (read) )

; recursive loop for number asking
(defun ask-number ()
  (format t "Please enter a number. ")
  ;; make handler "val"
  (let ((val (read)))
    (if (numberp val)
      val
      (ask-number) ) ) )
; validation with recursion for insistence
    
(defun show-squares (start end)
  (do ( (i start (+ i 1)) )
    ((> i end) 'done)
    (format t "~A ~A~%" i (* i i)) ) )

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)) )
    len ) )

(defun our-length (n lst)
  (if (not lst)
    n
    (our-length (+ 1 n) (cdr lst)) ) )

(defparameter *verse*
"~a bottle~:p of beer on the wall
~:*~a bottle~:p of beer
Take one down, pass it around
~a bottle~:p of beer on the wall~2%" )

(defun bottles (x)
  (loop for bottles from x downto 1
        do (format t *verse* bottles (1- bottles)) ) )

(defun elmayor (a b)
  (if (> a b)
    a
    b ) )

(defun enigma (x)
  ; if x is something and
  (and (not (null x))
       ; the first of x is null
       (or (null (car x))
           ; or null is anywhere else in x
           (enigma (cdr x)) ) ) )
; if a list have null in it
; if the list is not over, and the first of the list is null
; else, if there is a null in the rest of the list

; understanding bools is harder than ifs
(defun nigma (x)
  (if (not x)
    nil
    (if (not (car x))
      t
      (nigma (cdr x)) ) ) )
; remember to change the variable on each cycle

; count the times x occurs in list y
(defun mystery (x y)
  (if (null y)
    nil
    (if (eql (car y) x)
      0
      (let ((z (mystery x (cdr y))))
        (and z (+ z 1)) ) ) ) )

(defun tienelistap (lst)
  (if (not lst)
    nil 
    (if (listp (car lst)) 
      t
      (tienelistap (cdr lst)) ) ) )

(defun tienelistp (lst)
  (do ((i (car lst) (car lst)))
    ((not lst) nil) ) )

(defun puntos (n)
  (do ((i n (- i 1)))
    ((< i 1) t)
    (format t ".") ) )

(defun repuntos (n)
  (format t ".")
  (if (not (eql n 1))
    (repuntos (- n 1))
    t ) ) 

(defun cuenta (tar lst)
  (let ((Acc 0))
    (dolist (i lst)
      (setf Acc
            (+ Acc
               (if (eql i tar)
                 1
                 0 ) ) ) )
    Acc ) )

(defun recuenta (tar lst)
  (if lst
    (if (eql tar (car lst)) 
      (+ 1 (recuenta tar (cdr lst)))
      (recuenta tar (cdr lst)) )
    0 ) )


; remove is not destructive,
; and the list ir returns is not used for anything
(defun summit (lst)
  (apply #'+ (remove nil lst)) )

; the base case, for when the list is empty, was not included.
(defun summita (lst)
  (if lst 
    (let ((x (car lst)))
      (if (null x)
        (summita (cdr lst))
        (+ x (summita (cdr lst))) ) )
    0 ) )

(defun our-listp (x)
  (or (null x) (consp x)) )

(defun our-atom (x)
  (not (consp x)) )

; if all of the members of the conses, that are not conses, are identical, and in the same order
(defun our-equal (x y)
  ; they are identical
  (or (eql x y)
      ; they are both conses
      (and (consp x)
           (consp y)
           ; with the car equal
           (our-equal (car x) (car y))
           ; and the cdr equal
           (our-equal (cdr x) (cdr y)) ) ) )

(defun our-copy-list (lst)
  ; base case, end for atoms
  (if (atom lst)
    lst
    ; build a new structure with the same elements
    ; the new structure will not be identical to the old, even if its equal
    (cons (car lst)
          (our-copy-list (cdr lst)) ) ) )

(defun compress (x)
  (if (consp x)
    (compr (car x) 1 (cdr x))
    x ) )

(defun compr (elt n lst)
  ; if the list has ended, save the current element with its number
  (if (null lst)
    (list (n-elts elt n))
    ; if there is still list left, 
    (let ((next (car lst)))
      ; if the element repeats
      (if (eql next elt)
        ; increment its count
        (compr elt (+ n 1) (cdr lst))
        ; if it does not repeat, save the count of the element, and restart the count for a new element
        ; its saved as a deferred operation in the recursive stack
        ; to snap back on the end of the list
        (cons (n-elts elt n)
              (compr next 1 (cdr lst)) ) ) ) ) )

; simplify (1 elt) to (elt)
(defun n-elts (elt n)
  (if (> n 1)
    (list n elt)
    elt ) )

(defun uncompress (lst)
  ; stop if you left all the elements to uncompress in previous recursions
  (if (null lst)
    nil
    ; uncompress the first element
    (let ((elt (car lst))
          ; an ingenious way to unpack all the compressions of the lst
          ; rest will be the uncompressed list, ready for appending
          (rest (uncompress (cdr lst))) )
      ; if its the full, expand it
      (if (consp elt)
        (append (apply #'list-of elt)
                rest )
        ; if its the summarized, just cons it
        (cons elt rest) ) ) ) )

; expand a single element
(defun list-of (n elt)
  (if (zerop n)
    nil
    (cons elt (list-of (- n 1) elt)) ) )

(defun our-nthcdr (n lst)
  ; basecase, return what you have if you've reached the correct cdr
  (if (zerop n)
    lst
    ; recurse, advance 1 position along the list
    (our-nthcdr (- n 1) (cdr lst)) ) )

(defun our-copy-tree (tr)
  (if (atom tr)
    tr
    (cons (our-copy-tree (car tr))
          (our-copy-tree (cdr tr)) ) ) )

(defun our-subst (new old tree)
  (if (eql tree old)
    new
    (if (atom tree)
      tree
      (cons (our-subst new old (car tree))
            (our-subst new old (cdr tree)) ) ) ) )

(defun our-member-if (fn lst)
  (and (consp lst)
       (if (funcall fn (car lst))
         lst
         (our-member-if fn (cdr lst)) ) ) )

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (let ((mid (/ len 2)))
           (equal (subseq s 0 mid)
                  (reverse (subseq s mid)) ) ) ) ) )

(defun mir? (s)
  (equal s (reverse s)) )

(defun nthmost (n lst)
  (nth (- n 1)
       (sort (copy-list lst) #'>) ) )

(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (push elt acc) )
    acc ) )

; that is, is the last node in the list a nil?
; whether there are 0 or more nodes...
(defun proper-list? (x)
  ; terminating condition, is the last thing evaluated a nil?
  (or (null x)
      ; if its not over, is it a cons whose cdr fit the definition?
      (and (consp x)
           (proper-list? (cdr x)) ) ) )

; nil is weird... its not defined like the other lists.
; instead, a list is defined as a chain of 0 or more conses that ends in nil.
; there is a weird idea here... oh, no, tis quite normal:
; a list is a chain of conses.
; there are 3 kinds of lists:
; proper lists
    ; that end in nil
; dotted list
    ; that end in some other atom
; circular lists
    ; that end in the same cons they start
; from this definition, nil is barely a list. its a special thing used to define a kind of list
; its called nil by convenience.... or mnemonic. Because it reflects on its dual role:
; as an empty list, as False

(defun our-assoc (key alist)
  ; validation, the list is not over
  (and (consp alist) 
       ; mk handle of association
       (let ((pair (car alist)))
         ; test equality, if the car is equal to the key
         (if (eql key (car pair))
           pair
           ; recurse to the next element
           (our-assoc key (cdr alist)) ) ) ) )

(setf minimo '((a b c) (b c) (c d)))

; implement breath first search on a network NET to find the shortest path from node START to node END
(defun shortest-path (start end net)
  ; (bfs 'z '((a)) '((a b c) (b c) (c d) (d z)))
  ; the queue we start with is the minimal queue,
  ; with the first path being the node A and no explored paths
  ; the queue will grow with nodes as you traverse the network
  (bfs end (list (list start)) net) )


(defun bfs (end queue net)
  ; if the queue is empty, stop
  ; when would this happen?
  (if (null queue)
    nil
    ; the path is the first of the queue...
    ; the first list in the queue represents the next path to search
    (let ((path (car queue)))
      ; the first node in the path is the current node, which you'll compare with the goal END node.
      (let ((node (car path)))
        ; if the current node is the END node, stop and return the inverted path
        ; so that the starting node is up front, instead of at the end
        (if (eql node end)
          (reverse path)
          ; if it isn't the end node, 
          ; repeat the search
          (bfs end
               ; here comes the tricky bit:
               ; take out the latest path, and put all the other paths ahead of
               ; the paths that branch from the current path
               (append (cdr queue)
                       (new-paths path node net) )
               net ) ) ) ) ) )

; this one's harder because I don't have a firm grasp of the data structure
; the state is unclear, so the way the procedure modifies the state (what the process is) is unclear too.

; return a list of all branching paths, to be queued (that is, put at the end of the list
; (b a) b minimo -> ((c b a))
(defun new-paths (path node net)
  ; make a list of branching paths from the current path
  (mapcar 
    ; make a new path for each of the possible branching nodes
    #'(lambda (n)
        (cons n path)) 
    ; return all the nodes that NODE is connected with
    (cdr (assoc node net)) ) )

(defun .opu (La Le)
  (order-preserving-union La Le) )

; this one is shorter, and maybe easier to edit
; but its harder to understand the arguments
(defun order-preserving-union (La Le)
  (if Le
    ; I'm not sure if moving Le is better here or in the recurse
    (let ((E (car Le)) (Le (cdr Le)))
      (order-preserving-union
        ; replace La, with more of the same or an extension
        (if (member E La)
          La
          (append La (list E)) )
        Le ) )
    La ) )

; the repetition makes this one easier to compare
(defun _order-preserving-union (La Le)
  (if Le
    (let ((E (car Le)) (Le (cdr Le)))
      (if (member E La)
        (order-preserving-union 
          La 
          Le )
        (order-preserving-union 
          (append La (list E)) 
          Le ) ) )
    La ) )

(defun pos+ (Lst)
  (let ((out NIL))
    (do ((i 0 (+ i 1)))
      ((equal i (length Lst)) Lst)
      (setq out 
        (append out 
          (list 
            (+ 
              (nth i Lst) 
              i ) ) ) ) )
    out ) )

(defun .pos+ (Lst)
  (_pos+ 0 Lst) )

(defun _pos+ (Num Lst)
  (if Lst
    (cons (+ (car Lst) Num)
          (_pos+ (+ 1 Num) (cdr Lst)) )
    NIL ) )

(defun .pos+ (Lst)
  (let ((i -1))
    (mapcar
      (lambda (Num)
        (+
          Num
          (setf i (+ i 1)) ) )
      Lst ) ) )

; 4.1 searching a sorted list

(defun bin-search (obj vec)
  (let ((len (length vec)))
    ; validate that the vector has elements
    (and (not (zerop len))
         ; initialize finder
         ; the return value of length is too long for 0-starting indexing
         (finder obj vec 0 (- len 1)) ) ) )

; would that (- len 1) be an argument against exclusive counting?
; because, since you are using recursion, and functions that are unaware of your counting
; style, then you have to change the return value of other functions to suit your need.

; that is, since finder needs to go to the index of the last element, but 
; len returns the length of the list, which is 1 greater than the index of the element
; then you have to modify that len to get the index.
; if len returned the index, no problem would arise...
; it appears that the best counting inclusion strategy depends on the context, on what you
; are doing.
; does recursion go better with 1-starting end-inclusive counting?
; does iteration go better with 0-starting end-exclusive counting?
; one of the arguments by dijkstra was that the for could be easily composed

; --

; this method has a lookup range, which gets reduced each iteration until you run into
; the object in the middle of it, or the range is 0.
; its allas are numbers, the indexes of the elements of the vector,
; instead of the vector itself
; it is tail recursive, but that is just disguised iteration

; uppercase are the values of start, lowercase are the values of end
; A             B       C    d   a
; should be the other way around:
; a             b       c    D   A
; a    c   D    B                A
(defun finder (obj vec start end)
  (format t "~A~%" (subseq vec start (+ end 1)))
  ; how many elements will be checked
  (let ((range (- end start)))
    ; there are no elements to check
    (if (zerop range)
      ; if we found the obj, in the position we are in right now
      (if (eql obj (aref vec start))
        ; return the object
        obj
        ; return nil, the object is not inside the vector
        nil)
      ; if there are more than 1 number to check,
      ; find the middle of the two numbers
      (let ((mid (+ start (round (/ range 2)))))
        ; the object in the position between the start and end
        (let ((obj2 (aref vec mid)))
          ; if obj2 is bigger than obj, that means that obj is before obj2, so you should
          ; look before obj2
          (if (< obj obj2)
            (finder obj vec start (- mid 1))
            ; if obj is larger than obj2, then its after obj2, and you should check after it
            (if (> obj obj2)
              (finder obj vec (+ mid 1) end)
              ; if its neither smaller nor bigger, its equal, so its the object we were
              ; looking for. Return it.
              obj ) ) ) )  ) ) )

; its recursive, but the change happens in the range of elements that you'll 
; check, not in the data structure.
; ok, I think the thing that changes between iterations should have a special
; name... 
; allagi is change in greek, that might work
; allai singular, allas plural
; the allas are start and end
; static and dynamic might also work... but I don't want to keep reusing the
; same words with slightly different meanings over and over.
; I want many words, with very precise meanings.

; define alla(i|s)? : the variables (or data structures) that change each cycle
; of the iteration

(setq 
  *a* (vector 1 2 3 4 5) 
  *b* (aref *a* 2) )

; this line changes only the value of *b*, not the field in *a*
(setf *b* 10)
; this one does change *a* destructively
(setf (aref *a* 2) 10)

; and I find that annoying! depending on the situation,
; aref returns a value that can be stored, or a pointer that setf can modify
; its inconsistent

(defun mirror? (s)
  (let ((len (length s)))
    ;; validate that it can be mirrored, by having an even length
    ;; that is, "abba" is mirrored, but "aba" is not.
    (and (evenp len)
         ;; each step... ???
         ;; forward is static, but back is dynamic?
         ;; that is, back is allai and forward is not?
         (do ((forward 0 (+ forward 1))
              ; back reduces by 1 each turn
              (back (- len 1) (- back 1)) )

           ;; if the forward is greater than the back,
           ;; or the mirror elements are not equal
           ;; stop
           ((or (> forward back)
                ;; the mirror letters are not equal
                (not (eql (elt s forward)
                          (elt s back) ) ) )
            ;; and return the value of whether forward is greater than back...
            (> forward back) ) )  ) ) )

; if forward is greater than back, then the word is a mirror
; if forward is smaller than back, then the comparison stopped before the word ended,
; so its not a mirror.
; forward and back meet at the middle, so you only end up checking half the list
; well, half + 1

; this version would be very inefficient with lists, because you would have to travel
; the fist half of the list every time you wanted to retrieve the value of back.
; but its ok with vectors, because of the random access. You don't need to traverse to
; get to the end.

;; here, we can change arbitrary pointers in the string, by returning a field.
;; feels useful. Not sure if the arcane expansion of setf is worth it, though
;(let
;  ((a "hello world"))
;  (setf (subseq a 3 5) "LONG")
;  a )

(defun mirror? (s)
  ; handle the length
  (let ((len (length s)))
    ; its mirrorable, by being of even length
    (and (evenp len)
         ; with two cursors, starting at the beggining and end,
         ; and moving forward and backward
         (do ((forward 0 (+ forward 1))
              (back (- len 1) (- back 1)) )

           ; if forward is greater than back
           ((or (> forward back)
                ; or the element in forward is not equal to
                ; the corresponding element in back
                (not (eql (elt s forward)
                          (elt s back) ) ) )
                ; then its not a mirror
            ; but if it is, then its a mirror
            (> forward back) ) )  ) ) )



(defun second-word (str)
  ;; get the position of the first space
  (let ((p1 (+ (position #\  str) 1))) ; check for a space starting from the first place
    ;; make a subsequence, from the position of the first space to the position of the
    ;; second space. Or, the next space following the first one
    (subseq str p1 (position #\  str :start p1)) ) )


; parsing dates

; find what each line does literally
(defun tokens (str test start)
  ; find the position of the first element in str 
  ; satisfying the function test, 
  ; counting from the starting position start
  (let ((p1 (position-if test str :start start)))
    ; if there is an element that passes test,
    (if p1
      ; find the first element that does not pass the test in str,
      ; starting at position start
      (let ((p2 (position-if #'(lambda (c)
                                 (not (funcall test c)) )
                             str :start p1 ) ) )
        ; save recursively the satisfying part between p1 and p2, and
        (cons (subseq str p1 p2)
              ; if there is a rest of the string, then
              (if p2
                ; continue parsing from the end of the first token, the first substring
                (tokens str test p2)
                ; if not, just make all the deferred operations into a list
                nil ) ) )
      ; if there is no starting token, stop operations
      nil ) ) )

; there are two stops: if the token does not end, or if there are no tokens left
; save up the token, by 
    ; spliting the string in 
        ; the start and 
        ; end of the token, and 
    ; searching for the next token.

; find what each line does conceptually
(defun tokens (str test start)
  ; find the start of the token
  (let ((p1 (position-if test str :start start)))
    (if p1
      ; find the end of the token
      (let ((p2 (position-if #'(lambda (c)
                                 (not (funcall test c)) )
                             str :start p1 ) ) )
        ; save the token
        (cons (subseq str p1 p2)
              (if p2
                ; find the next token
                (tokens str test p2)
                ; stop if the token does not end
                nil ) ) )
      ; stop if there are no more token starts
      nil ) ) )

(defun constituent (c)
  ; c is both a graphic char and not whitespace
  (and (graphic-char-p c)
       (not (char= c #\  )) ) )
; graphic chars are characters we can see

(defun parse-date (str)
  ; return a list representing the numerical values of the day, month, year
  ; split the date into tokens, into individual words separated by whitespace
  (let ((toks (tokens str #'constituent 0)))
    ; the first word is the day
    (list (parse-integer    (first toks))
          ; the second word is the month,
          ; parse it with a special function, that maps month name to month num
          (parse-month      (second toks))
          ; the third word is the year
          (parse-integer    (third toks)) ) ) )

; it does not validate that the inputted month exists,
; or its known

;(defconstant month-names
;             #("jan" "feb" "mar" "apr" "may" "jun"
;               "jul" "aug" "sep" "oct" "nov" "dec" ) )

(defun parse-month (str)
  ; find the positon of str in month names
  (let ((p (position str month-names
                     :test #'string-equal ) ) )
    ; if there is a position, 
    (if p
      ; return that position + 1,
      ; because months start from 1, but indexes start from 0
      (+ p 1)
      ; if not, then that month is not defined, so return nil, that is false.
      nil ) ) )
; its curious where Graham does the processing.
; he finds the position in the let,
; and then conditionally returns it

; I would have had finding the position and returning it combined into one.

; the picolisp code would be (de month (Month) (index Month *Month-names))
; why is it so much simpler?
; One: the month position is not the same as the month number.
; they differ by one, so you have to add it.
; but, position returns nil if the month is not there, and 1 + Nil is an error,
; so, you need to check if the month is in there, and then you have to conditionally add a 1.
; if its not, you need to return nil. In picolisp, the second argument of if defaults to nil.
; if picolisp indexing started at 0, this is the code:
; (de parse-month (Month) (if (index Month *Month-names) (+ @ 1)))
; still simpler... because of the implicit assignment to @ as the control variable...

    ; implicit assignment to default variable @.
    ; starting from 1 instead of 0
    ; default return arguments
    ; those are the innovatins of picolisp, for this problem


(defun my-read-integer (str)
  ; if every character is a digit
  ; so validate the string
  (if (every #'digit-char-p str)
    ; start the accumulator
    (let ((accum 0))
      ; turn the string into a number, one digit at a time.
      ; from the highest order to the lowest order
      (dotimes (pos (length str))
        (setf accum (+ (* accum 10)
                       (digit-char-p (char str pos)) ) ) )
      ; return the integer.
      accum )
    nil ) )
; the function cannot be named read-integer, because it comes from a package already loaded,
; and that package is locked, meaning that functions from that package cannot be renamed.
; picolisp does not lock packages. I can redefine whatever I want. Though its easy to not do
; it, I can check if the variable already has a value associated with it in the repl.
; and there are some other things... I  think abu uses some functions to ensure that variables
; are not globally redefined, only locally redefined.

(defstruct polemic
  (type (progn
          (format t "what kind of polemic was it? ")
          (read) ) )
  (effect nil) )

(defstruct (point (:conc-name p)
                  (:print-function print-point) )
  (x 0)
  (y 0) )

(defun print-point (p stream depth)
  (format stream "#<~a, ~a>" (px p) (py p)) )

; structures, they have names and attributes. Each has their own type, and you can 
; access their elements by name
; defining a structure makes some structure acces and modification and creation functions
; automatically, like 'make-struct, struct-p, copy-struct, struct-element
; a little exaple of programs writing programs
; each struct has a default representation, which you can define by the function print-struct
; I'm not sure how it gets associated with the point


; make a structure node, that is printed as the root of the node
(defstruct (node (:print-function 
                   (lambda (n s d)
                     (format s "#<~a>" (node-elt n)) ) ) )
  ; it has 3 elements, elt (the root of the node), l (the left element), and r (the right elt)
  ; the leafs default to nil
  elt (l nil) (r nil) )

(defun bst-insert (obj bst <)
  ; if the tree is null, make it a one node tree
  (if (null bst)
    (make-node :elt obj)
    ; if the tree does exist, insert the element
    ; assign the root of the node
    (let ((elt (node-elt bst)))
      ; if the element is already in the tree
      (if (eql obj  elt)
        ; dont insert anything, and return the tree itself
        bst
        ; if the object is greater than the elemnt
        (if (funcall < obj elt)
          ; insert it to the left, and make the right of the object the right of the root
          ; make a pointer to the rest of the tree on the right of the object?
          (make-node
            :elt    elt
            :l      (bst-insert obj (node-l bst) <)
            :r      (node-r bst) )
          ; insert the object on the right, make the rest of the tree to the left
          (make-node
            :elt    elt
            :r      (bst-insert obj (node-r bst) <)
            :l      (node-l bst) ) ) ) ) ) )

; I'm not sure that the (node-l bst) line does... is it about copying the rest
; of the tree as the right of the object? that seems strange and wasteful,
; unless you are copying the reference, and all that's left is a pointer to the
; rest of the tree... but it still seems like it would make the tree super long
; without need.

; remake the binary tree, with the root as the root, obj either left or right,
; and the rest of the tree left or right, depending on the obj
(defun bst-insert (obj bst <)
  ; make the tree if it does not exist
  (if (null bst)
    (make-node :elt obj)
    ; make elt handler
    (let ((elt (node-elt bst)))
      ; if the object is already in the tree
      (if (eql obj  elt)
        bst
        ; make a new tree! with the root as elt,
        ; the left as obj, and the rest to the right, where it was before
        (if (funcall < obj elt)
          (make-node
            :elt    elt
            :l      (bst-insert obj (node-l bst) <)
            :r      (node-r bst) )
          ; same as before, but reversed
          (make-node
            :elt    elt
            :r      (bst-insert obj (node-r bst) <)
            :l      (node-l bst) ) ) ) ) ) )



; search for an element in the tree,
; return the branch with that element as its root
(defun bst-find (obj bst <)
  ; if the tree is null
  ; there is nothing to find
  (if (null bst)
    nil
    ; handle the root
    (let ((elt (node-elt bst)))
      ; if the root is the goal, return the tree from there
      (if (eql obj elt)
        bst
        ; if the object is smaller than the root
        (if (funcall < obj elt)
          ; search to the left, where all the smaller are
          (bst-find obj (node-l bst) <)
          ; if the obj is greater than the root, search to the right
          (bst-find obj (node-r bst) <) ) ) ) ) )

; go down the tree, to the lowest, leftest node
(defun bst-min (bst)
  ; the tree exists
  (and bst
       ; there are either more nodes to the left,
       ; so there are yet smaller nodes,
       ; or you've reached the end, so this leaf is the smallest node
       (or (bst-min (node-l bst)) bst) ) )

; search for the biggest node
(defun bst-max (bst)
  ; the tree exists
  (and bst
       ; there are more nodes to the right, or this is the biggest node
       (or (bst-max (node-r bst)) bst) ) )

; the annoying thing about this representation, like most structs and nodes, is that the
; structure of the tree is hidden inside the netword of nodes, and seeing the structure
; is a pain in the ass. If it were just a list, seeing the structure would be simply printing
; the list.



; search for an element in a list
; return the rest of the list
(defun our-member (obj lst)
  (if (null lst) ; base case
    nil
    (if (eql obj (car lst)) ; is member
      lst
      (our-member obj (cdr lst)) ) ) ) ; recurse

; search for an element in the tree,
; return the branch with that element as its root
(defun bst-find (obj bst <)
  (if (null bst)
    nil
    (let ((elt (node-elt bst)))
      (if (eql obj elt)
        bst
        (if (funcall < obj elt)
          (bst-find obj (node-l bst) <)
          (bst-find obj (node-r bst) <) ) ) ) ) )

; returning the entire branch lets us distinguish between no match (NIL),
; and finding the branch with root NIL ( #<NIL> )

; remove an element of the binary search tree,
; if its the root, percolate, if not search to the left, xor to the left
(defun bst-remove (obj bst <)
  ; if the bst is not, return nil
  ; that is, if the object is not in the tree, you cannot remove it
  (if (null bst)
    nil
    ; handle the root
    (let ((elt (node-elt bst)))
      ; if the root is the object to remove, run percolate
      (if (eql obj elt)
        (percolate bst)
        ; if not, search the tree by remaking it,
        (if (funcall < obj elt)
          ; search the left
          (make-node
            :elt elt
            :l (bst-remove obj (node-l bst) <)
            :r (node-r bst) )
          ; search the right
          (make-node
            :elt elt
            :r (bst-remove obj (node-r bst) <)
            :l (node-l bst) ) ) ) ) ) )

; move up roots to remake the tree, from either the left or the right
(defun percolate (bst)
  ; if the left is empty
  (cond ((null (node-l bst))
         ;and the right is empty
         (if (null (node-r bst))
           ; return nil, why?
           nil
           ; but if there IS to the right, right percolate the tree
           (rperc bst) ) )
        ; if right is empty, left percolate the tree
        ((null (node-r bst)) (lperc bst))
        ; if neither right or left is null 
        ; randomly left or right percolate the tree
        (t (if (zerop (random 2))
             (lperc bst)
             (rperc bst) ) ) ) )


; remake the bst
(defun rperc (bst)
  ; make the right root the new root
  (make-node :elt (node-elt (node-r bst))
             ; make the left the left of the tree
             :l (node-l bst)
             ; percolate the rest of the right,
             ; that is, move up nodes from left or right to root,
             ; and reorganize the nodes below it
             :r (percolate (node-r bst)) ) )

; the same as rperc, but reorganizing the left
(defun lperc (bst)
  (make-node :elt (node-elt (node-r bst))
             :l (percolate (node-r bst))
             :r (node-l bst) ) )

; double recursion! percolate calls rperc and lperc, and each of those call percolate!
; in this case, they COULD be one function, but combining those concerns would be too
; confusing.

; percolate is just flow control, who should you promote up or down.
; the real function is done in rperc and lperc. But then, you need to reorganize the branches
; inside the left and right, so you call the control flow function percolate.

  

; make a node struct, representing a binary tree, with these elements:
; root : elt, l : left tree, r : right tree
; you represent it like so: #<_elt_>
(defstruct (node (:print-function 
                   (lambda (n s d)
                     (format s "#<~a>" (node-elt n)) ) ) )
  elt (l nil) (r nil) )

; insert an obj in a balanced Binary Search Tree (bst).
(defun bst-insert (obj bst <)
  ; make the bst if its null
  ; if you get to a leaf, add it to the correct place on the leaf
  (if (null bst)
    (make-node :elt obj)
    ; handle the root
    (let ((elt (node-elt bst)))
      ; the obj is the root
      (if (eql obj  elt)
        bst ; return the hole branch rooted in obj
        ; if its smaller than the root
        (if (funcall < obj elt)
          ; search left
          (make-node
            :elt    elt
            :l      (bst-insert obj (node-l bst) <)
            :r      (node-r bst) )
          ; search right
          (make-node
            :elt    elt
            :r      (bst-insert obj (node-r bst) <)
            :l      (node-l bst) ) ) ) ) ) )


(defun bst-insert (obj bst <)
  ; Stop
  (if (null bst)
    (make-node :elt obj)
    (let ((elt (node-elt bst)))
      ; Found
      (if (eql obj  elt)
        ; Search
        (if (funcall < obj elt)
          ; left
          (make-node
            :elt    elt
            :l      (bst-insert obj (node-l bst) <)
            :r      (node-r bst) )
          ; right
          (make-node
            :elt    elt
            :r      (bst-insert obj (node-r bst) <)
            :l      (node-l bst) ) ) ) ) ) )


(defun bst-find (obj bst <)
  ; the dead leaf, its not on the tree
  ; its smaller or bigger than the last obj, but there is nothing to be found there
  (if (null bst)
    nil
    ; handle
    (let ((elt (node-elt bst)))
      ; you found it!
      (if (eql obj elt)
        bst
        ; keep searching
        (if (funcall < obj elt)
          ; left
          (bst-find obj (node-l bst) <)
          ; right
          (bst-find obj (node-r bst) <) ) ) ) ) )

(defun bst-min (bst)
  ; 
  (and bst
       (or (bst-min (node-l bst)) bst) ) )

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst) ) )

(defun my-bst-min (bst)
  ; (and bst
 (or (bst-min (node-l bst)) bst) )




(defun bst-find (obj bst <)
  ; stop
  (if (null bst)
    nil
    (let ((elt (node-elt bst)))
      ; find
      (if (eql obj elt)
        bst
        ; search
        (if (funcall < obj elt)
          (bst-find obj (node-l bst) <)
          (bst-find obj (node-r bst) <) ) ) ) ) )


(defun bst-remove (obj bst <)
  ; stop
  (if (null bst)
    nil
    (let ((elt (node-elt bst)))
      ; found
      (if (eql obj elt)
        (percolate bst)
        ; search
        (if (funcall < obj elt)
          ; left
          (make-node
            :elt elt
            :l (bst-remove obj (node-l bst) <)
            :r (node-r bst) )
          ; right
          (make-node
            :elt elt
            :r (bst-remove obj (node-r bst) <)
            :l (node-l bst) ) ) ) ) ) )

(defun percolate (bst)
  ; remake right
  (cond ((null (node-l bst))
         ; remake neither
         (if (null (node-r bst))
           nil
           (rperc bst) ) )
        ; remake left
        ((null (node-r bst)) (lperc bst))
        ; remake any
        (t (if (zerop (random 2))
             (lperc bst)
             (rperc bst) ) ) ) )

; how much does you audience know?
; how much familiarity must you expect?
; at least as much as you will have a month from now, 
; if you had not read your code in all that time

(defun rperc (bst)
  ; make the right node the new root
  (make-node :elt (node-elt (node-r bst))
             :l (node-l bst)
             ; move up all those child nodes to the right
             ; make right all to the right
             :r (percolate (node-r bst)) ) )

(defun lperc (bst)
  ; make the left node the new root
  (make-node :elt (node-elt (node-l bst))
             ; move up all the left child nodes
             :l (percolate (node-l bst))
             :r (node-r bst) ) )

; insert an obj in a balanced Binary Search Tree (bst).
(defun bst-insert (obj bst <)
  ; make the bst if its null
  ; if you get to a leaf, add it to the correct place on the leaf
  (if (null bst)
    (make-node :elt obj)
    ; handle the root
    (let ((elt (node-elt bst)))
      ; the obj is the root
      (if (eql obj  elt)
        bst ; return the hole branch rooted in obj
        ; if its smaller than the root
        (if (funcall < obj elt)
          ; search left
          (make-node
            :elt    elt
            :l      (bst-insert obj (node-l bst) <)
            :r      (node-r bst) )
          ; search right
          (make-node
            :elt    elt
            :r      (bst-insert obj (node-r bst) <)
            :l      (node-l bst) ) ) ) ) ) )

(setf nums nil)

(dolist (x '(5 8 4 2 1 9 6 7 3)) 
  (setf nums (bst-insert x nums #'<)) )


(defun bst-traverse (fn bst)
  ; while you have not reached the leaf
  (when bst
    ; move left
    (bst-traverse fn (node-l bst))
    ; print the function
    (funcall fn (node-elt bst))
    ; traverse right
    (bst-traverse fn (node-r bst)) ) )

; the order of the functions is what tells you the order of traversal.
; it first goes to the leftmost root, before printing the root one up.
