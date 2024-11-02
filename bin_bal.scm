;; here is the statement of the problem we considered today in class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; balanced sequences of parentheses

; for this problem, we will think of 0 as a convenient substitute for a left bracket,
; and 1 as a convenient substitute for a right bracket. 

; a sequence of 0s and 1s is said to be balanced if (i) every 0 is later
; closed by some 1, and (ii) every 1 closes a previous 0.

; thus ((0 1)) is the list of all balanced sequences of length 2, and
; ( (0 0 1 1) (0 1 0 1) ) is the list of all balanced sequences of
; length 4

; Assuming n is a positive even integer, write and prove correct
; a function bal so that (bal n) returns a list - without duplicates -
; of all balanced sequences of 0s and 1s of length n.  Your program
; should be as efficient as you can make it: in particular, it should not generate
; any unbalanced sequences in the course of computing the final result.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Flow of bal(n):
;;
;; Input: 
;; - A positive even integer `n`, which is the length of the binary sequence.
;;
;; Output:
;; - A list of all unique balanced sequences of length `n`.
;;
;; Base Case:
;; - If `n = 0`, return an empty list '().
;; - If `n = 2`, return the only balanced sequence of length 2: '((0 1)).
;;
;; Recursive Case:
;; - If `n > 2`, the function generates balanced sequences using two operations:
;; 
;; 1. `wrap`: Wraps each balanced sequence of length `n-2` with `0` and `1`, 
;;    producing new balanced sequences.
;;
;; 2. `allConcats`: Finds all ways to concatenate two balanced sub-sequences whose
;;    combined length equals `n`. These sub-sequences are created by partitioning `n`
;;    into two non-trivial even parts and calling `bal` on each part.
;;
;; Intermediate Steps:
;; - `wrap`: Adds a `0` at the front and a `1` at the end of each balanced sequence of length `n-2`.
;; - `allConcats`: Concatenates balanced sequences of lengths `p` and `q`, where `p + q = n`.
;; - `removeDuplicates`: Ensures the final list contains no duplicate sequences.
;;
;; Example:
;; For `n = 4`, `bal` will call `wrap(bal(2))` and `allConcats(bal(2), bal(2))`, 
;; resulting in `((0 0 1 1) (0 1 0 1))`.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Design Ideas for bal(n):
;;
;; 1. Recursive Decomposition:
;;    - The problem is broken into smaller subproblems by reducing `n` to `n-2` in each step.
;;    - This allows us to build balanced sequences incrementally, using balanced sequences of shorter lengths.
;;
;; 2. Avoiding Unnecessary Work:
;;    - The function only generates valid balanced sequences at every step:
;;      - `wrap` ensures that balanced pairs are added around smaller sequences.
;;      - `allConcats` guarantees that only valid combinations of balanced sub-sequences are considered.
;;    - This avoids the generation of unbalanced sequences, making the algorithm more efficient.
;;
;; 3. Set Operations:
;;    - The function `removeDuplicates` ensures that no duplicate sequences are included in the final result.
;;    - This step is necessary because concatenating balanced sub-sequences might produce duplicates.
;;
;; 4. Non-trivial Even Partitions:
;;    - The function `nonTrivialEvenPartitions` generates pairs `(p, q)` where `p + q = n` and both are even.
;;    - This ensures that only valid sub-sequence pairs are considered for concatenation, avoiding trivial or redundant cases.
;;
;; 5. Modularity:
;;    - The use of helper functions like `wrap`, `product`, and `flatten` makes the design modular.
;;    - Each function has a clear, specific task:
;;      - `wrap`: Adds structure to the balanced sequences.
;;      - `product`: Combines sequences from two lists.
;;      - `flatten`: Converts a list of lists into a flat list.
;;      - `removeDuplicates`: Removes duplicate sequences.
;;
;; 6. Efficiency:
;;    - The design ensures efficiency by not generating unbalanced sequences at any point.
;;    - `allConcats` uses valid partitions and balanced sub-sequences to ensure correctness and efficiency.
;;
;; Example:
;; For `n = 6`, the function will:
;; - Wrap the result of `bal(4)` using `wrap`.
;; - Concatenate balanced sequences of lengths `(2, 4)` and `(4, 2)` using `allConcats`.
;; - Remove duplicates using `removeDuplicates`.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; questions for you:

;;  does it return only balanced binary lists?

;;  does it return all balanced binary lists of length n? 


;;  can you prove its correctness? -


;;  does it in fact return a duplicate-free list?  you can check (for example) that

;;    (= (length (bal 8)) (length (removeduplicates (bal 8))))

;; returns #t, but this is of course not a proof. 


;; can you see how to use bal (and perhaps a function union-sets) to compute the list of all balanced binary lists
;; of length <= n?


;; why is flatten needed?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bal n)
  (cond ((= n 0) '())
        ((= n 2) '((0 1)))
        (else (append (wrap (bal (- n 2))) (allConcats n)))))

(define (wrap listOfLists)
  (map (lambda (lst) (append (list 0) lst (list 1))) listOfLists))

(define (allConcats n)
  (removeDuplicates
   (flatten
    (map (lambda (pr)
           (let ((p (car pr))
                 (q (cadr pr)))
             (product (bal p) (bal q))))
         (nonTrivialEvenPartitions n)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulate op init (cdr seq))))))


(define (flatten lst)
  (accumulate append '() lst))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (elementOf? e l)
  (cond ((null? l) #f)
        ((equal? e (car l)) #t)
        (else (elementOf? e (cdr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (removeDuplicates lst)

  (cond ((null? lst) '())
        ((elementOf? (car lst) (cdr lst)) (removeDuplicates (cdr lst)))
        (else (cons (car lst) (removeDuplicates (cdr lst)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lst1 and lst2 are lists of lists

(define (product lst1 lst2)
  (flatten
   (map (lambda (l1)
          (map (lambda (l2) (append l1 l2))
               lst2))
        lst1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nonTrivialEvenPartitions n)
  
  (define (neitherIsZero p q)
    (and (not (zero? p)) (not (zero? q))))
  
  (define (bothAreEven p q)
    (and (even? p) (even? q)))
  
  (define (sumIs n p q)
    (= n (+ p q)))
  
  (define (enumerate-interval a b)
    (cond ((> a b) '())
          (else (cons a (enumerate-interval (+ a 1) b)))))

  (define (allPairs n)
    (flatten
     (map (lambda (i)
            (map (lambda (j) (list i j))
                 (enumerate-interval 1 n)))
          (enumerate-interval 1 n))))

  ; body of nonTrivialEvenPartitions
  (filter (lambda (pr)
            (let ((p (car pr))
                  (q (cadr pr)))
              (and (neitherIsZero p q) (bothAreEven p q) (sumIs n p q))))
          (allPairs n))
  )


(define (bin-to-bal sym-list)
  (let ((binary-list
         (map (lambda (sym)
                (cond ((equal? sym '( ')) 0)  ;; '(' becomes 0
                      ((equal? sym ')') 1)))  ;; ')' becomes 1
              sym-list)))
    ;; Now apply bal to generate all possible balanced sequences of the same length
    (bal (length binary-list))))

;; Example usage:
;; (bin-to-bal '(()())) ; Converts symbol list '(()()) into a balanced list using bal