(define pal_c
  (lambda (xs)
    (define walk
      (lambda (xs1 xs2 k)
        (cond
          ((and (null? xs2))
           (k xs1))
          ((null? (cdr xs2))
           (k (cdr xs1)))
          (else
           (walk
            (cdr xs1) (cdr (cdr xs2))
            (lambda (ys)
              (and (equal? (car xs1) (car ys))
                   (k (cdr ys)))))))))
    (walk xs xs (lambda (v) #t))))

#;
(begin
  (pal_c '())
  (pal_c '(1))
  (pal_c '(1 2))
  (pal_c '(1 2 2 1))
  (pal_c '(1 2 1))
  (pal_c '(1 2 3 1))
)
