(load "../lib/faster-minikanren/mk-vicare.scm")
(load "../lib/faster-minikanren/mk.scm")
(load "../lib/faster-minikanren/test-check.scm")

;; Sunday, May 5, 2024 -- WEB

;; Cleaning up the macro definition in trs5.scm, based on two
;; suggestions from Michael Ballantyne:
;;
;; 1. rename `f` to `compile-pattern`
;;
;; 2. lift out `(lambda (pat) ...)` into a new named helper, `compile-clause`

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
    (define compile-pattern
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
           (with-syntax ((((ax ...) a) (compile-pattern ops #'a))
                         (((dx ...) d) (compile-pattern ops #'d)))
             #'((ax ... dx ...) (a . d)))]
          [c
           (let ((c (syntax->datum #'c)))
             (or (number? c) (boolean? c)))
           #'(() c)])))
    (syntax-case stx (->)
      [(_ name (op ...) pat pat* ...)
       #`(define name
           (lambda (a b)
             (conde
               #,@(let ((compile-clause
                         (lambda (pat)
                           (syntax-case pat (->)
                             [(lhs-pat -> rhs-pat)
                              (with-syntax ([((x ...) lhs-quasi-term)
                                             (compile-pattern
                                              (syntax->list #'(op ...))
                                              ;; `syntax->list` makes this work in Racket as well as Chez,
                                              ;; although `syntax->list` is not defined in R6RS (but it is
                                              ;; easy to write)
                                              #'lhs-pat)]
                                            [((y ...) rhs-quasi-term)
                                             (compile-pattern
                                              (syntax->list #'(op ...))
                                              ;; `syntax->list` makes this work in Racket as well as Chez,
                                              ;; although `syntax->list` is not defined in R6RS (but it is
                                              ;; easy to write)
                                              #'rhs-pat)])
                                (with-syntax ([(x ...)
                                               (delete-duplicates bound-identifier=?
                                                                  (append (syntax->list #'(x ...))
                                                                          (syntax->list #'(y ...))))])
                                  #'((fresh (x ...)
                                       (== `lhs-quasi-term a)
                                       (== `rhs-quasi-term b)))))]))))
                    (map
                     compile-clause
                     (syntax->list #'(pat pat* ...)))))))])))

(trs reduces (* + exp log)
  ((+ x x) -> (* 2 x))
  ((exp (log x)) -> x))
#|
=>
(define reduces
  (lambda (a b)
    (conde
      ((fresh (x) (== `(+ ,x ,x) a) (== `(* 2 ,x) b)))
      ((fresh (x) (== `(exp (log ,x)) a) (== `,x b))))))

(modulo gensyming and further expansion to core Scheme/Racket)

You can verify this by changing

 #`(define name

to

 #`'(define name

in the macro definition above, which will cause the macro call to
expand to the quoted list, without automatic gensyming.
|#

(test "trs6-1"
  (run* (x y) (reduces x y))
  '(((+ _.0 _.0) (* 2 _.0))
    ((exp (log _.0)) _.0)))



(trs reduces2 (* + exp log)
  ((expt (log x)) -> x)
  ((+ x x y x) -> (+ (* 3 x) y)))
#|
=>
(define reduces2
  (lambda (a b)
    (conde
      ((fresh (expt x)
         (== `(,expt (log ,x)) a)
         (== `,x b)))
      ((fresh (x y)
         (== `(+ ,x ,x ,y ,x) a)
         (== `(+ (* 3 ,x) ,y) b))))))
|#

(test "trs6-2"
  (run* (x y) (reduces2 x y))
  '(((_.0 (log _.1)) _.1)
    ((+ _.0 _.0 _.1 _.0) (+ (* 3 _.0) _.1))))


#|
Make sure we really check for `->` as an auxilliary keyword.
(trs reduces3 (* + exp log)
  ((+ x x) foo (* 2 x))
  ((exp (log x)) -> x))
=>
Exception: invalid syntax ((+ x x) foo (* 2 x)) at line 181, char 3 of trs5.scm
|#
