;; Church Booleans
(define tru
  (lambda (t) (lambda (f) t)))

(define fls
  (lambda (t) (lambda (f) f)))

(define to_bool
  (lambda (b) (b #t #f)))

;; Pairs
(define pair
  (lambda (f) (lambda (s) (lambda (b) ((b f) s)))))

(define fst
  (lambda (p) (p tru)))

(define snd
  (lambda (p) (p fls)))

(define to_tuple
  (lambda (p) (cons (fst p) (snd p))))

(define c0
  (lambda (s) (lambda (z) z)))

(define c1
  (lambda (s) (lambda (z) (s z))))

(define c2
  (lambda (s) (lambda (z) (s (s z)))))

(define scc
  (lambda (n) (lambda (s)  (lambda (z)  (s ((n s) z))))))

(define to_int
  (lambda (n) ((n (lambda (v) (+ v 1))) 0)))

(define iszro
  (lambda (n) ((n (lambda (x) fls)) tru)))

(define plus
  (lambda (m) (lambda (n) (lambda (s) (lambda (z) ((m s) ((n s) z)))))))

(define zz
  ((pair c0) c0))

(define ss
  (lambda (p) ((pair (snd p)) ((plus c1) (snd p)))))

(define prd
  (lambda (n) (fst ((n ss) zz))))

(define prd-alt
  (lambda (n) (lambda (s) (lambda (z) (((n (lambda (g) (lambda (h) (h (g s))))) (lambda (u) z)) (lambda (u) u))))))
