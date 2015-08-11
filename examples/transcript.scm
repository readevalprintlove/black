;; start running from the parent directory
(load "env.scm")
(load "stream.scm")
(define scheme-apply apply)
(load "black.scm")
(black)

(exec-at-metalevel
 (let ((old-eval base-eval))
   (set! base-eval (lambda (exp env cont)
                     (write 'trace:) (write exp) (newline)
                     (old-eval exp env cont)))))

(car (cons 1 2))

(exit 'bye)

(exec-at-metalevel
 (load "break.blk"))

(inspect base-eval) ;; works in Chez but not MIT or Chicken Scheme
base-eval
(exit 'good-bye)
(old-cont 'hello)
(+ 1 2)

;; resume with a different function at the lower level
(define inc (lambda (x) (+ x 1)))
(incr 2) ;; typo
(base-eval 'inc old-env (lambda (x) x)) ;; see what inc is
(base-eval 'inc old-env old-cont) ;; resume with inc for incr

(load "examples/start.scm")

;; instrumentation
(exec-at-metalevel
 (load "examples/instr.blk"))
;; or
(exec-at-metalevel
 (load "examples/instr2.blk"))
(load "examples/church.scm")
(instr (prd c2))
(instr (prd-alt c2))
(instr (to_int (prd-alt c2)))
(instr (to_int (prd c2)))

;; taba

(load "examples/start.scm")
(EM (load "examples/utils.blk"))

(EM (load "examples/taba.blk"))
;; or
(EM (load "examples/taba2.blk"))
;; or
;; NB: this version requires adding printf as a primitive
(EM (load "examples/taba3.blk"))

(load "examples/cnv.scm")
(taba (cnv3 walk) (cnv3 '(1 2 3) '(a b c)))

(load "examples/pal.scm")
(taba (pal_c walk) (pal_c '(1 2 2 1)))
