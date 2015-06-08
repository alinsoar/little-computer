#lang racket

;;; DEBUGGING TOOLS

(define strcat (lambda (ss) (foldr string-append "" (map ~a ss))))

(define (q x y)
  (lambda m
    (and (eq? x y)
         (apply p (append m '("\n")))
         (void))))

(define W! (lambda msg (list (apply ~a (cons "WARNING! " msg)))))
(define E! (lambda msg (list (apply ~a (cons "ERROR! " msg)))))
(define MSG (lambda msg (list (apply ~a msg))))

(define (d . args)
  "generic display"
  (define (iter a co)
    (if (null? a)
        (co (lambda () (newline) (flush-output)))
        (iter (cdr a)
              (lambda (x)
                (co (lambda ()
                      (display " ")
                      (display (car a))
                      (x)))))))
  (void (iter args (lambda (x) (x)))))
(define (p . args) (void (map display args) (flush-output)))

;;; COUNTERS
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define counter
  (let ((counters (make-hash)))
    (define get (lambda (id) (hash-ref! counters id (lambda () 0))))
    (define (1+ id) (hash-set! counters id (add1 (get id))))
    (define (reset id n) (hash-set! counters id n))
    (lambda (perm) (perm get 1+ reset))))
(define counter/get
  (lambda (id)
    (counter (lambda (get _ __) (get id)))))
(define 1+
  (counter (lambda (_ 1+ __) 1+)))
(define counter/reset
  (counter (lambda (_ __ r) r)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define o
  (lambda fs
    ((lambda (s) (s s fs identity))
     (lambda (s l co)
       (if (empty? (cdr l))
           (co (car l))
           (s s (cdr l)
              (lambda (w)
                (co ((car l) w)))))))))

;;; dec-bin translators
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define d2b
  (lambda (width n)
    (define iter
      (lambda (n w co)
        (if (zero? n)
            (co w)
            (iter (quotient n 2)
                  (cdr w)
                  (lambda (r)
                    (co (cons (remainder n 2)
                              r)))))))
    (iter n (make-list width 0)
          (lambda (x) x))))

(define b2d
  (lambda (bin-list)
    (foldr (lambda (x y) (+ x (* 2 y))) 0 bin-list)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-syntax def-from-pair
  (syntax-rules ()
    ((def-from-pair (fun params) pair)
     (define-values (fun params) (values (car pair) (cdr pair))))))

(define rev-vector
  (lambda (vec)
   (o list->vector
      reverse
      vector->list
      vec)))

;;; Export definitions
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(provide 1+
         counter/get
         counter/reset
         (rename-out (d2b dec->bin)
                     (b2d bin->dec))
         d
         p
         W!
         E!
         MSG
         q
         strcat
         def-from-pair
         o
         rev-vector
         )
