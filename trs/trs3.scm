(load "../lib/faster-minikanren/mk-vicare.scm")
(load "../lib/faster-minikanren/mk.scm")
(load "../lib/faster-minikanren/test-check.scm")

;; Another attempt at the term rewriting macro, with Michael
;; Ballantyne on 28 Apr 2024, expanding upon `trs2.scm`.

;; Arunava Gantait's original specification:
;;
;; (trs reduces (* + exp log)
;;   ((+ x x) -> (* 2 x))
;;   ((exp (log x)) -> x))
;;
;; should expand to a definition that is equivalent to:
;;
;; (define reduces
;;   (lambda (a b)
;;     (conde
;;       ((fresh (x)
;;          (== a `(+ ,x ,x))
;;          (== b `(* 2 ,x))))
;;       ((fresh (x)
;;          (== a `(exp (log ,x)))
;;          (== b x))))))

;; In `trs2.scm` we implemented the simpler:
;;
;; (term-pattern (+ * exp)
;;   (+ x x))
;; =>
;; `(+ ,x ,x)
;;
;; Now let's add the `fresh` binding for the variables introduced in
;; the pattern, along with the outer `lambda`, and a unification of
;; the formal parameter of the `lambda` to the quasiquoted pattern:
;;
;; (term-pattern (+ * exp)
;;   (+ x x))
;; =>
;; (lambda (a) (fresh (x) (== a `(+ ,x ,x))))

#|
Grammar for a pattern:

pat ::= <num>  |
        <bool> |
        ()     |
        <id> where <id> is in ops |
        <id> where <id> is not in ops |
        (<pat> . <pat>)
|#

(define-syntax term-pattern  
  (lambda (stx)
    ;; Put `f` inside the `(lambda (stx) ...)` so that this code works
    ;; with implicit phasing (Chez) and explicit phasing (Racket)

    ;; `delete-duplicates` is adapted from the SRFI-1 reference
    ;; implementation.
    (define (delete-duplicates elt= lis)
      (let recur ((lis lis))
        (if (null? lis)
            lis
	    (let* ((x (car lis))
		   (tail (cdr lis))
		   (new-tail (recur (filter (lambda (y) (not (elt= x y))) tail))))
	      (if (eq? tail new-tail) lis (cons x new-tail))))))
    (define f
      (lambda (
               ops ;; list of identifiers
               pat ;; syntax-object representing a pattern
               )
        (syntax-case pat ()
          [() #'(() ())]
          [x (and (identifier? #'x)
                  (memp
                    (lambda (op) (bound-identifier=? #'x op))
                    ops))
           #'(() x)]
          [x (identifier? #'x)           
           #'((x) ,x)]
          [(a . d)  ;; alternative approach
           (with-syntax ((((ax ...) a) (f ops #'a))
                         (((dx ...) d) (f ops #'d)))
             #'((ax ... dx ...) (a . d)))]
          [c
           (let ((c (syntax->datum #'c)))
             (or (number? c) (boolean? c)))
           #'(() c)]
          )))
    (syntax-case stx ()
      [(_ (op ...) pat)
       (with-syntax ([((x ...) quasi-term)
                      (f
                       (syntax->list #'(op ...))
                       ;; `syntax->list` makes this work in Racket as well as Chez,
                       ;; although `syntax->list` is not defined in R6RS (but it is
                       ;; easy to write)
                       #'pat)])
         (with-syntax ([(x ...)
                        (delete-duplicates bound-identifier=? #'(x ...))])
           #'(lambda (a) (fresh (x ...) (== `quasi-term a)))))])))

(term-pattern (+ * exp)
  (+ x x))
#|
=>
(lambda (a)
  (fresh (x)
    (== `(+ ,x ,x) a)))

(modulo gensyming and further expansion to core Scheme/Racket)

You can verify this by changing

  #'(lambda

to

  #''(lambda

in the macro definition above, which will cause the macro call to
expand to the quoted list, without automatic gensyming.
|#

(test "trs3-1"
  (let ((p (term-pattern (+ * exp)
             (+ x x))))
    (run* (q)
      (p q)))
  '((+ _.0 _.0)))
