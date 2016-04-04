(define zip
  (lambda (xs ys)
    (cond
      ((or (null? xs) (null? ys))
       '())
      (else
       (cons
        (cons (car xs) (car ys))
        (zip (cdr xs) (cdr ys)))))))

(define cnv2
  (lambda (xs ys)
    (define walk
      (lambda (xs k)
        (cond
          ((null? xs)
           (k '() ys))
          (else
           (walk (cdr xs)
                 (lambda (r ys) (k (cons (cons (car xs) (car ys)) r)
                              (cdr ys))))))))
    (walk xs (lambda (r ys) r))))

(define cnv3
  (lambda (xs ys)
    (define walk
      (lambda (xs)
        (cond
          ((null? xs)
           (cons '() ys))
          (else
           (let ((rys (walk (cdr xs))))
             (let ((r (car rys))
                   (ys (cdr rys)))
               (cons (cons (cons (car xs) (car ys)) r)
                     (cdr ys))))))))
    (car (walk xs))))

#;
(begin
  (zip '(1 2 3) '(a b c))
  (cnv2 '(1 2 3) '(a b c))
  (cnv3 '(1 2 3) '(a b c))
)
