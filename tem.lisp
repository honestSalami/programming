
(defun our-member (obj lst)
  (if (null lst)
    nil
    (if (eql (car lst) obj)
      lst
      (our-member obj (cdr lst)) ) ) )

(defun askem (string)
  (format t "~A" string)
  (read) )

; recursive loop for number asking
(defun ask-number ()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
      val
      (ask-number) ) ) )
    
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
    (let ((E (car Le)) (Le (cdr Le)))
      (order-preserving-union
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
