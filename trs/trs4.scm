(load "../lib/faster-minikanren/mk-vicare.scm")
(load "../lib/faster-minikanren/mk.scm")
(load "../lib/faster-minikanren/test-check.scm")

;; Friday, May 3, 2024 -- WEB
;;
;; Based on trs3.scm with Michael Ballantyne from 28 Apr 2024

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

;; In trs3.scm we address the simpler problem, which is really the
;; core problem:
;;
;; (term-pattern (+ * exp)
;;   (+ x x))
;; =>
;; (lambda (a) (fresh (x) (== a `(+ ,x ,x))))
;;

;; Let's expand the macro a little to give a name to the generated
;; procedure, and to handle a single pattern with left-hand and
;; right-hand patterns separated by the auxilliary keyword `->`:
;;
;; (trs reduces (* + exp log)
;;   ((+ x x) -> (* 2 x)))
;; =>
;; (define reduces
;;   (lambda (a b)
;;     (fresh (x)
;;       (== `(+ ,x ,x) a)
;;       (== `(* 2 ,x) b))))


#|
Grammar for a pattern:

pat ::= <num>  |
        <bool> |
        ()     |
        <id> where <id> is in ops |
        <id> where <id> is not in ops |
        (<pat> . <pat>)
|#

(define-syntax trs
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
          [(a . d)
           (with-syntax ((((ax ...) a) (f ops #'a))
                         (((dx ...) d) (f ops #'d)))
             #'((ax ... dx ...) (a . d)))]
          [c
           (let ((c (syntax->datum #'c)))
             (or (number? c) (boolean? c)))
           #'(() c)]
          )))
    (syntax-case stx (->)
      [(_ name (op ...) (lhs-pat -> rhs-pat))
       (with-syntax ([((x ...) lhs-quasi-term)
                      (f
                       (syntax->list #'(op ...))
                       ;; `syntax->list` makes this work in Racket as well as Chez,
                       ;; although `syntax->list` is not defined in R6RS (but it is
                       ;; easy to write)
                       #'lhs-pat)]
                     [((y ...) rhs-quasi-term)
                      (f
                       (syntax->list #'(op ...))
                       ;; `syntax->list` makes this work in Racket as well as Chez,
                       ;; although `syntax->list` is not defined in R6RS (but it is
                       ;; easy to write)
                       #'rhs-pat)])
         (with-syntax ([(x ...)
                        (delete-duplicates bound-identifier=?
                                           (append (syntax->list #'(x ...))
                                                   (syntax->list #'(y ...))))])
           #'(define name
               (lambda (a b)
                 (fresh (x ...)
                   (== `lhs-quasi-term a)
                   (== `rhs-quasi-term b))))))])))

(trs reduces (* + exp log)
  ((+ x x) -> (* 2 x)))
#|
=>
(define reduces
  (lambda (a b)
    (fresh (x)
      (== `(+ ,x ,x) a)
      (== `(* 2 ,x) b))))

(modulo gensyming and further expansion to core Scheme/Racket)

You can verify this by changing

 #'(define name

to

 #''(define name

in the macro definition above, which will cause the macro call to
expand to the quoted list, without automatic gensyming.
|#

(test "trs4-1"
  (run* (x y) (reduces x y))
  '(((+ _.0 _.0) (* 2 _.0))))


(trs reduces2 (* + exp log)
  ((exp (log x)) -> x))
#|
=>
(define reduces2
  (lambda (a b)
    (fresh (x)
      (== `(exp (log ,x)) a)
      (== `,x b))))
|#

(test "trs4-2"
  (run* (x y) (reduces2 x y))
  '(((exp (log _.0)) _.0)))
