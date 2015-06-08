#lang racket ; -*- mode:racket ; buffer-read-only:t -*-

;;; A few generic tools used in the Little Computer.

;;; Copyright (C) 2015 Alin C Soare

;;; This file is part of the Little Computer.

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; DEBUGGING TOOLS
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
        (co (lambda ()
              (newline)
              (flush-output)))
        (iter (cdr a)
              (lambda (x)
                (co (lambda ()
                      (display " ")
                      (display (car a))
                      (x)))))))
  (void (iter args (lambda (x) (x)))))
(define (p . args)
  (void (map display args) (flush-output)))


;;; COUNTERS
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define counter
  (let ((counters (make-hash)))
    "Donec eris felix, multos numerabis amicos."
    (define (get id)
      (hash-ref! counters id (lambda () 0)))
    (define (1+ id)
      (hash-set! counters id (add1 (get id)))
      (get id))
    (define (reset id n)
      (hash-set! counters id n))
    (lambda (perm)
      (perm get 1+ reset))))
(define counter/get
  (lambda (id)
    (counter (lambda (get _ __) (get id)))))
(define 1+
  (counter (lambda (_ 1+ __) 1+)))
(define counter/reset
  (counter (lambda (_ __ r) r)))


;;; Haskell style
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

;;; often used assignment
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-syntax def-from-pair
  (syntax-rules ()
    ((def-from-pair (fun params) pair)
     (define-values (fun params) (values (car pair) (cdr pair))))))

;;; vectors
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define rev-vector
  (lambda (vec)
   (o list->vector
      reverse
      vector->list
      vec)))

(define vector-split
  (lambda (v ranges)
    (cond ((empty? ranges) '())
          (else
           (define-values (f r) (vector-split-at v (car ranges)))
           (cons f (vector-split r (cdr ranges)))))))

(define strvec
  ((lambda (s) (lambda (v) (s s (vector->list v) "")))
   (lambda (s l str)
     (if (empty? l)
         str
         (s s (rest l) (~a str (car l)))))))


;;; strings
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define strcat
  (lambda (ss) (foldr string-append "" (map ~a ss))))

(define ~a4
  (lambda (s)
    (~a s #:width 4)))

(define ~a6
  (lambda (s)
    (~a s #:width 6)))

;;; Cantor's Pairing function
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define sqrt
  (lambda (k)
    "Newton's method."
    (define (average x y)
      (exact-floor (/ (+ x y) 2)))

    (define square
      (lambda (x)
        (* x x)))

    (define (sqrt-improve guess x)
      (average guess (/ x guess)))

    (define (sqrt-stream0 x)
      (define guesses
        (stream-cons 1
                     (stream-map (lambda (guess)
                                   (sqrt-improve guess x))
                                 guesses)))
      guesses)
    
    (define (fixed-point s prev)
      (define next (stream-first s))
      (if (= prev next)
          prev
          (fixed-point (stream-rest s) next)))

    (fixed-point (sqrt-stream0 k) -1)))

(define pair2
  (lambda (a b)
    "The Cantor's pairing function."
    (let ((s (+ a b)))
      (+ (/ (* s (add1 s)) 2)
         b))))
(define unpair2
  (lambda (n)
    "Inverse of the Cantor's pairing function."
    (let* ((w (exact-floor (/ (sub1 (sqrt (add1 (* 8 n))))
                              2)))
           (t (/ (+ w (* w w)) 2))
           (b (- n t))
           (a (- w b)))
      (cons a b))))

(define pairn
  (lambda ns
    ((lambda (s) (s s ns (lambda (x) x)))
     (lambda (s ns c)
       (cond ((null? ns)
              (c 1))
             (else
              (s s (cdr ns)
                 (lambda (acc)
                   (c (pair2 (car ns) acc))))))))))
(define unpairn
  (lambda (n yield)
    ((lambda (s) (s s n yield))
     (lambda (s n y)
       (cond ((= n 1)
              (y 'done 'done))
             (else
              (define pair (unpair2 n))
              (y (car pair)
                 (lambda (y)
                   (s s (cdr pair) y)))))))))

(define pair2/assert
  (lambda (x y)
    (equal? (unpair2 (pair2 x y))
            (cons x y))))
(define pair3-assert
  (lambda l3
    (define a (first l3))
    (define b (second l3))
    (define c (third l3))
    (or
     (equal?
      (cons a (cons b (cons c 'done)))
      (unpairn
       (pairn a b c)
       (lambda (v next)
         (cons
          v
          (next (lambda (v next)
                  (cons
                   v
                   (next (lambda (v next)
                           (cons
                            v
                            (next (lambda (v next)
                                    v))))))))))))
     (error "pair3 error --" a b c))))

;;; NUMERIC CONVERSION TOOLS
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

;;; TODO

(define dec->boolvec
  (lambda (n w)
    (vector-map positive? (dec->binvec n w))))
(define boolvec->dec
  (let ((pow2 (build-vector 16 (lambda (n) (expt 2 n)))))
    (lambda (vec)
      ((lambda (s) (s s (sub1 (vector-length vec)) 0))
       (lambda (s i sum)
         (cond ((negative? i)
                sum)
               ((vector-ref vec i)
                (s s (sub1 i) (+ sum (vector-ref pow2 i))))
               (else
                (s s (sub1 i) sum))))))))
(define binvec->dec
  (let ((pow2 (build-vector 16 (lambda (n) (expt 2 n)))))
    (lambda (vec)
      ((lambda (s) (s s (sub1 (vector-length vec)) 0))
       (lambda (s i sum)
         (cond ((negative? i)
                sum)
               ((zero? (vector-ref vec i))
                (s s (sub1 i) sum))
               (else
                (s s (sub1 i) (+ sum (vector-ref pow2 i))))))))))
(define binvec->integer
  (lambda (v)
    (define sign (vector-ref v 15))
    (if (zero? sign)
        (binvec->dec v)
        (- (add1 (binvec->dec (vector-map (lambda (x) (- 1 x)) v)))))))

(define dec->binvec
  (lambda (n w)
    (list->vector (d2b w n))))

(define boolvec->binvec
  (lambda (v)
    (vector-map (lambda (x) (if x 1 0)) v)))


;;; dec-bin translators
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define d2b
  (lambda (width n)
    (define iter
      (lambda (n w co)
        (if (or (empty? w) (zero? n))
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

(define operator car)
(define operands cdr)
(define tagged?
  (lambda (tag l . op)
    (and (pair? l)
         ((if (null? op) eq? (car op))
          (operator l) tag))))

(define tok-type
  (lambda (p)
    (and (pair? p)
         (car p))))
(define tok-val
  (lambda (p)
    (and (pair? p)
         (cdr p))))

(define tok-ref
  (lambda (stream . n)
    ((lambda (s) (s s (if (null?  n) 0 (car n))
               stream))
     (lambda (s n toks)
       (cond ((null? toks)
              '(false))
             ((zero? n) (car toks))
             (else
              (s s (sub1 n) (cdr toks))))))))
(define tok-drop
  (lambda (stream . n)
    ((lambda (s) (s s (if (null?  n) 1 (car n))
               stream))
     (lambda (s n toks)
       (if (zero? n)
           toks
           (s s (sub1 n) (cdr toks)))))))

(define ~key?
  (lambda (code)
    (and (pair? code)
         (pair? (tok-ref code))
         (eq? (tok-type (tok-ref code)) 'key))))
(define ~id?
  (lambda (code)
    (and (pair? (tok-ref code))
         (eq? (tok-type (tok-ref code)) 'id))))

;;; Godel Encoding of C-declarations.  We use a variant of the
;;; encoding of elements of a set that Godel used in his theorem of
;;; incompleteness.  While we could use this method to encode each of
;;; the statements of any formal expression as a primitive recursive
;;; function, here we limit its use to conveniently encode a subset of
;;; C-declarations we are interested in.

(define debug-godel-sets false)

(define divisible?
  (lambda (n m) (zero? (remainder n m))))
(define eratosthenes
  (lambda (n stream)
    "the sieve of eratosthenes"
    (stream-cons
     n
     (eratosthenes (stream-first stream)
                   (stream-filter
                    (lambda (x)
                      (not (divisible? x (stream-first stream))))
                    (stream-rest stream))))))
(define odd-integers
  ((lambda (s) (s s 3))
   (lambda (s n)
     (stream-cons n (s s (+ 2 n))))))

;; (define (queue:make . id)
;;   (let ((items '()))
;;     (define last '())
;;     (lambda (? . data)
;;       (case ?
;;         ('+
;;          (set! items (append items (list (car data)))))
;;         ('- (let ((first (if (null? items)
;;                              (error "empty queue")
;;                              (car items))))
;;               (set! items (cdr items))
;;               first))
;;         ;; ('del (set! items (delq (car data) items)))
;;         ('empty? (null? items))
;;         ('str (map (lambda (x) (cons (x 'STR) (x 'GET/K))) items))))))

(define (queue:make . ID)
  (let ((ALLOC 4))
    (define items (make-vector ALLOC))
    (define COUNTER 0)
    (define HEAD 0)
    (define TAIL 0)
    (define NEXT (lambda (W) (if (< (+ 1 W) ALLOC) (+ 1 W) 0)))

    (lambda (m . data)
      (case m
        ('+
         (cond ((= COUNTER ALLOC)
                (set! items (vector-append (vector-drop items HEAD)
                                           (vector-take items HEAD)
                                           (vector (car data))
                                           (make-vector (- ALLOC 1))))
                (set! TAIL 0)
                (set! HEAD (+ 1 ALLOC))
                (set! ALLOC (* 2 ALLOC)))
               (else
                (vector-set! items HEAD (car data))
                (set! HEAD (NEXT HEAD))))
         (set! COUNTER (+ 1 COUNTER)))
        ('-
         (and (zero? COUNTER) (error "BAD USE OF QUEUE"))
         (let ((item (vector-ref items TAIL)))
           (set! TAIL (NEXT TAIL))
           (set! COUNTER (- COUNTER 1))
           item))
        ('empty?
         (zero? COUNTER))
        ('len
         COUNTER)
        (else
         (error "QUEUE:unknown message" m))))))

;;; =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
;;; =~=~=~=~=~=~=~=~=~=~=~=~ GODEL SETS ~=~=~=~=~=~=~=~=~=~=~=~=
;;; =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

(define make-godel-set
  (lambda (elts)
    "sets with unique and duplicated elements."
    (define codes
      (map
       (lambda (n)
         (stream-ref (eratosthenes 2 odd-integers) n))
       (range (length elts))))
    (define encode
      (let ((hash (make-hash (map cons elts codes))))
        (lambda (x)
          (hash-ref hash x (lambda () false)))))
    (define decode
      (let ((hash (make-hash (map cons codes elts))))
        (lambda (x)
          (hash-ref hash x))))
    (define get-set
      (lambda (godel/n)
        (define (iter n k)
          (cond ((= n 1) '())
                ((null? k) '())
                ((divisible? n (car k))
                 (cons (decode (car k))
                       (iter (/ n (car k)) k)))
                (else
                 (iter n (cdr k)))))
        (iter godel/n codes)))
    
    (define check/e!
      (lambda (e m)
        (or (memq e elts)
            (error "bad use of godel sets --" m))))
    (define check/o!
      (lambda (o m)
        (or (eq? (o 'elts) elts)
            (error "bad use of godel sets --" m))))
    (define check/b!
      (lambda (c m)
        (or c (error "bad use of godel sets -- " m))))
    
    (define new
      (lambda (name . es)
        ;; (define self (lambda () (void)))
        (define godel-number 1)
        (define cardinal 0)

        (define add!
          (lambda (self)
            (lambda (e)
              (check/e! e 'add)
              (and debug-godel-sets
                   (p "add "  e
                      " [" (encode e) "] "
                      "to `" name "`\n"))
              (define e0 (encode e))
              (check/b! e0 'hash)
              (set! godel-number (* godel-number e0))
              (set! cardinal (add1 cardinal))
              self)))
        (define str
          (lambda ()
            (define (iter n k)
              (cond ((= n 1) "")
                    ((null? k) "")
                    ((divisible? n (car k))
                     (~a (decode (car k))
                         " "
                         (iter (/ n (car k)) k)))
                    (else
                     (iter n (cdr k)))))
            (~a name ": " (string-trim (iter godel-number codes)))))
        (define intersect-uniq
          (lambda (other)
            (check/o! other 'intersection)
            (new 'intersection
                 (get-set (gcd (other 'get) godel-number)))))
        (define intersect-dups
          (lambda (other)
            (check/o! other 'intersection)
            (define iu (godel-get (intersect-uniq other)))
            (define (iter g k)
              (cond ((= g 1) '())
                    ((null? k) '())
                    ((and (divisible? g (car k))
                          (divisible? iu (car k)))
                     (cons (decode (car k))
                           (iter (/ g (car k)) k)))
                    ((divisible? g (car k))
                     (iter (/ g (car k)) k))
                    (else
                     (iter g (cdr k)))))
            (new 'intersection
                 (iter godel-number codes))))
        (define =?
          (lambda (other)
            (check/o! other 'equal?)
            (and (= godel-number (other 'get)))))
        
        (map (add! 'none) (if (null? es) es (car es)))
        ((lambda (s) (s s))
         (lambda (self)
           (lambda (m)
             (case m
               ('str            (str))
               ('add!           (add! (self self)))
               ('get            godel-number)
               ('get-set        (get-set godel-number))
               ('get-set0       get-set)
               ('intersect-uniq intersect-uniq)
               ('intersect-dups intersect-dups)
               ('elts           elts)
               ('name           name)
               ('cardinal       cardinal)
               ('equal?         =?)
               (else
                (lambda _
                  (d (~a "bad use of the godel sets: "
                         m "; " name "\n" elts))
                  (exit 1)))))))))
    new))

;;; 0 OP
(define godel-str
  (lambda (gs)
    (gs 'str)))
(define godel-get
  (lambda (gs)
    (gs 'get)))
(define godel-get-set
  (lambda (gs)
    (gs 'get-set)))
(define godel-get-set0
  (lambda (gs n)
    ((gs 'get-set0) n)))
(define godel-elts
  (lambda (gs)
    (gs 'elts)))
(define godel-name
  (lambda (gs)
    (gs 'name)))
(define godel-cardinal
  (lambda (gs)
    (gs 'cardinal)))

;;; 1 OP
(define godel-add
  (lambda (gs n)
    ((gs 'add!) n)))
(define godel-intersect-uniq
  (lambda (gs1 gs2)
    ((gs1 'intersect-uniq) gs2)))
(define godel-intersect-dups
  (lambda (gs1 gs2)
    ((gs1 'intersect-dups) gs2)))
(define godel=?
  (lambda (gs1 gs2)
    ((gs1 'equal?) gs2)))

;;; Export definitions
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(provide 1+
         counter/get
         counter/reset
         
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
         vector-split
         strvec
         ~a4
         ~a6
         
         
         pairn
         
         
         (rename-out (d2b dec->bin)
                     (b2d bin->dec))
         boolvec->binvec
         dec->binvec
         binvec->integer
         
         dec->boolvec
         boolvec->dec

         binvec->dec
         
         
         operator
         operands
         tagged?
         tok-type
         tok-val
         tok-ref
         tok-drop
         ~key?
         ~id?
         
         make-godel-set
         godel-str
         godel-add
         godel-get
         godel-get-set
         godel-get-set0
         godel-intersect-uniq
         godel-intersect-dups
         godel-elts
         godel-name
         godel=?
         godel-cardinal
         
         queue:make
         
         )



