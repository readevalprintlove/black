(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (delay (cons a b)))))

(define stream-car
  (lambda (s)
    (car (force s))))

(define stream-cdr
  (lambda (s)
    (cdr (force s))))

(define head stream-car)
(define tail stream-cdr)
