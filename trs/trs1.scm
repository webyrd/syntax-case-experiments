(load "../lib/faster-minikanren/mk-vicare.scm")
(load "../lib/faster-minikanren/mk.scm")
(load "../lib/faster-minikanren/test-check.scm")

;; WEB's attempt to write a simple `syntax-rules` term-rewriting
;; macro, based on Arunava's specification, Arpil 17, 2024.

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

;; I didn't know how to write this macro, at least in a reasonable
;; way, using `syntax-rules`, so I simplified the problem in two ways:
;;
;; 1. I changed the syntax so that the user explicitly lists the
;; pattern variables for each clause;
;;
;; 2. I assumed that the operator names -- (* + exp log) in the
;; example above -- only appear in head position in an application.
;; So, `(* 2 x)` would be okay, but `(x 2 *)` would not

(define-syntax trs
  (syntax-rules (->)
    [(_ name (op ...) ((x ...) lhs -> rhs) ...)
     (define name
       (let ((op (lambda args (cons 'op args))) ...) ;; nice trick.
         (lambda (a b)
           (conde
             ((fresh (x ...) ;; here's the problem!  what name to use?  how many variables to fresh?
                (== lhs a)
                (== rhs b)))
             ...))))]))


(trs commutes (+)
  ((x y) (+ x y) -> (+ y x)))

(test "trs1-1"
  (run* (u v) (commutes u v))
  '(((+ _.0 _.1) (+ _.1 _.0))))


(trs reduces (* + exp log)
  ((x) (+ x x) -> (* 2 x))
  ((x) (exp (log x)) -> x))

(test "trs1-2"
  (run* (u v) (reduces u v))
  '(((+ _.0 _.0) (* 2 _.0))
    ((exp (log _.0)) _.0)))
