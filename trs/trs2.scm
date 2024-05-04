(load "../lib/faster-minikanren/mk-vicare.scm")
(load "../lib/faster-minikanren/mk.scm")
(load "../lib/faster-minikanren/test-check.scm")

;; Another attempt at the term rewriting macro, with Michael
;; Ballantyne on 28 Apr 2024.

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

;; Let's start with the simpler problem:
;;
;; (term-pattern (+ * exp)
;;   (+ x x))
;; =>
;; `(+ ,x ,x)

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
    (define f
      (lambda (
               ops ;; list of identifiers
               pat ;; syntax-object representing a pattern
               )
        (syntax-case pat ()
          [() #'()]
          [x (and (identifier? #'x)
                  (memp
                    (lambda (op) (bound-identifier=? #'x op))
                    ops))
           #'x]
          [x (identifier? #'x)
           #',x]
          #;[(a . d)
          ;;
          ;; Alternative approach to the pair case immmediately below.
          ;; We can use `with-syntax` to give names to the results of the
          ;; calls to `f`, rather than inlining the results using #` and #,
          ;;
           (with-syntax ((a (f ops #'a))
                         (d (f ops #'d)))
             #'(a . d))]
          [(a . d)
           ;; writing #'ops would be a big mistake,
           ;; since `ops` is not a pattern variable
           #`(#,(f ops #'a) . #,(f ops #'d))]
          [c
           (let ((c (syntax->datum #'c)))
             (or (number? c) (boolean? c)))
           #'c]          
          )))
    (syntax-case stx ()
      [(_ (op ...) pat)
       (with-syntax ([quasi-term
                      (f
                       (syntax->list #'(op ...))
                       ;; `syntax->list` makes this work in Racket as well as Chez,
                       ;; although `syntax->list` is not defined in R6RS (but it is
                       ;; easy to write)
                       #'pat)])
         #''`quasi-term)])))

(test "trs2-1"
  (term-pattern (+ * exp)
    (+ x x))
  '`(+ ,x ,x))
#|
Here we are checking that

  (term-pattern (+ * exp)
    (+ x x))

expands to

  `(+ ,x ,x)

Since we are not yet introducing a binding for `x`, we add an extra
quote to the macro expansion:

  #''`quasi-term

rather than

  #'`quasi-term

so we can see a quoted list representing the expansion (modulo
gensymming).

Once we introduce the binding for `x`, in the form of a `(fresh (x) ...)`,
we will be able to remove the extra quote without getting the exception:

  Exception: variable x is not bound

when we call the macro.
|#
