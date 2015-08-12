(define print-stack
  (lambda (es)
    (let ((m "~25a~25a\n"))
      (map
       (lambda (e)
         (let ((n (car e))
               (down (car (cdr e)))
               (up (car (cdr (cdr e)))))
           
           (printf m (cons n down) up)
           (printf m "|" "^")
           (printf m "V" "|")))
       es)

      (printf "~26,,,'_a\n" ""))))
