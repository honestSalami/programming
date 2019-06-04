
(de memif (Fun Lst)
   (and (pair Lst)
      (if (eval (cons Fun (car Lst)))
      Lst
      (memif Fun (cdr Lst)) ) ) )

(de hell () (prin "hel"))
(de worl () (prin "wor"))

(de .ga (Lst) (greater-after Lst))
(de greater-after (Lst)
   (let (A (++ Lst))
      (cnt
         '((N)
            (>= A N) )
         Lst ) ) )

(de .aga (Lst) (all-greater-after Lst))
(de all-greater-after (Lst)
   (apply +
      (maplist
         greater-after
         Lst ) ) )

(de greater-after (Lst)
   (let (A (++ Lst))
      (cnt '((N) (>= A N)) Lst) ) )

(de all-greater-after (Lst)
   (apply +
      (maplist greater-after Lst) ) )

(de .occ (Lst) 
   (_most-frequent-first
      (occurrences Lst) ) )

# how often does each element occurr in a list
(de occurrences (Lst)
   (let (Counter)
      (for E Lst
         (if (assoc E Counter)
            (inc (cdr @))
            (push 'Counter (list E 1)) ) )
      Counter ) )

# sort the occurences of the elements by most frequent
(de _most-frequent-first (Lst)
   (by 
      cadr 
      '((L) 
         (sort L >) )
      Lst ) )

(de pos+ (Lst)
   (make
      (for (C . Num) Lst
         (link
            (+ C Num) ) ) ) )

(de pos+ (Lst)
   (let Num 1
      (recur (Num Lst)
         (if Lst
            (cons (+ (car Lst) Num)
               (recurse (inc Num) (cdr Lst)) ) ) ) ) )

(de pos+ (Lst)
   (mapcar +
      Lst
      (range 1 (length Lst)) ) )


