#lang racket ; -*- mode:racket ; buffer-read-only:t -*-

;;; Parse into tokens a file whose syntax is specified by regexps.

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

(require "tools.rkt")

;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~= BNF compiler

(define atom?
  (lambda (e)
    (char? e)))

(define all-chars
  (append (map integer->char (range 32 128))
          (map integer->char (range 7 16))))

(define select?
  (lambda (e)
    (tagged? 'SELECT e)))
(define eval-select
  (lambda (expr)
    "select the first longest match"
    (cons 'select
          (map eval expr))))

(define union?
  (lambda (e)
    (tagged? 'UNION e)))
(define eval-union
  (lambda (expr)
    "the alternative operator"
    (cons 'union (map eval expr))))

(define variable?
  (lambda (e)
    (symbol? e)))
(define lexical-environment 'nil)
(define lookup
  (lambda (id)
    "find definition of a variable in the global lexical environment"
    ((lambda (w)
       (if w
           (cdr w)
           (begin
             (d "unknown variable:" id)
             (exit 1))))
     (assoc id lexical-environment))))
(define eval-variable
  (lambda (expr)
    (eval (lookup expr))))

(define concatenation?
  (lambda (expr)
    (tagged? 'CONCAT expr)))
(define eval-concatenation
  (lambda (expr)
    "the concat operator"
    (cons 'concat (map eval (operands expr)))))

(define repeat?
  (lambda (e)
    (tagged? 'REPEAT e)))
(define eval-repeat
  (lambda (expr)
    "the kleene star operator and concatenation of finite repetitions"
    (define min (car expr))
    (define max (cadr expr))
    (define op (map eval (drop expr 2)))
    (define kleene
      (lambda ()
        (cons 'kleene op)))
    (cond ((and (eq? min 0) (eq? max 'INF))
           (kleene))
          ((eq? max 'INF)
           (define n-times
             (lambda (n)
               (foldr append '() (make-list n op))))
           (append '(concat)
                   (n-times min)
                   (list (kleene))))
          (else
           (error "kleene -- to do" min max)))))

(define complement?
  (lambda (e)
    (tagged? 'COMPLEMENT e)))
(define eval-complement
  (lambda (expr)
    "the complement of a set of characters"
    (cons 'union (remove* expr all-chars))))

(define regex?
  (lambda (e)
    (string? e)))
(define eval-regular-expression
  (lambda (expr)
    "from the regular expression representation to operators."
    (re/compile (string->list expr))))

;;; A compiler from the language of the regular expressions to a
;;; sublanguage of combinators of the regular expressions algebra.
(define CONCAT
  '((POP Y)
    (POP X)
    (JOIN X Y)
    (PUSH X)))
(define NEW
  (lambda (char)
    `((NEW X)
      (SET X ,char)
      (PUSH X))))
(define UNION
  '((POP Y)
    (POP X)
    (ALTERNATIVE X Y)
    (PUSH X)))
(define KLEENE
  '((POP X)
    (KLEENE X)
    (PUSH X)))

(define re/compile
  (lambda (e)
    (re/alternative e
                    (lambda (re rest)
                      (or (null? rest)
                          (error "bad regular expression" e))
                      (eval
                       (re/processor re))))))
(define re/stream/assert
  (lambda (stream c)
    (or (eq? (car stream) c)
        (error "bad rex" stream))))
(define re/code
  (lambda pieces
    "append the pieces of code in a single linear piece"
    ((lambda (u) (u u pieces))
     (lambda (u p)
       (cond ((null? p)
              '())
             ((symbol? (car p))
              "cases like (NEW X)."
              (list p))
             ((eq? true (car p))
              "if (car p) is true ignore it."
              (u u (cdr p)))
             (else
              (append (u u (car p))
                      (u u (cdr p)))))))))
(define re/alternative
  (lambda (e y)
    ((lambda (u) (u u e y))
     (lambda (u s co)
       (re/concat s
                  (lambda (re1 rest1)
                    (cond ((null? rest1)
                           (co re1 rest1))
                          ((char=? (car rest1) #\|)
                           (u u (cdr rest1)
                              (lambda (re2 rest2)
                                (co (re/code re1 re2
                                             UNION)
                                    rest2))))
                          (else
                           ;; invalid regex
                           (co re1 rest1)))))))))
(define re/concat
  (lambda (e y)
    ((lambda (u) (u u e '()))
     (lambda (u s acc)
       (if (or (null? s)
               (memq (car s) '(#\| #\))))
           (y acc s)
           (re/kleene s
                      (lambda (t rest)
                        (u u rest
                           (re/code t acc
                                    (or (null? acc) CONCAT))))))))))
(define re/kleene
  (lambda (e y)
    (re/term e
             (lambda (term rest)
               (if (and (pair? rest)
                        (char=? #\* (car rest)))
                   (y (re/code term KLEENE) (cdr rest))
                   (y term rest))))))
(define re/term
  (lambda (e y)
    (cond ((null? e)
           (y 'void '()))
          ((char=? #\[ (car e))
           (re/complex/char e
                            (lambda (cc rest)
                              (y cc rest))))
          ((char=? #\( (car e))
           (re/alternative (cdr e)
                           (lambda (alt rest)
                             (re/stream/assert rest #\))
                             (y alt (cdr rest)))))
          (else
           (re/new/char e
                        (lambda (c rest)
                          (y c rest)))))))
(define re/complex/char
  (lambda (e y)
    (re/stream/assert e #\[)
    (define negation?
      (char=? #\^ (cadr e)))
    ((lambda (u)
       (u u
          ((if negation? cddr cdr) e)
          (lambda (cc rest)
            (cond
             (negation?
              (define complement
                (remove* (filter char? (flatten cc))
                         all-chars))
              ((lambda (u) (u u complement '()))
               (lambda (u w acc)
                 (if (null? w)
                     ;; return the complement
                     (y acc rest)
                     (u u
                        (cdr w)
                        (re/code (NEW (car w)) acc
                                 (or (null? acc) UNION)))))))
             (else
              ;; return the result
              (y cc rest))))))
     (lambda (u s c)
       (cond ((and (not (eq? c y))
                   (char=? (car s) #\]))
              (c '() (cdr s)))
             ((char=? (cadr s) #\-)
              (define first (car s))
              (define last (caddr s))
              (define (1+ a)
                (integer->char
                 (add1 (char->integer a))))
              (u u
                 (cdddr s)
                 (lambda (acc rest)
                   (define interval
                     ((lambda (u) (u u first '()))
                      (lambda (u x acc)
                        (if (char<=? x last)
                            (u u
                               (1+ x)
                               (re/code (NEW x) acc
                                        (or (char=? x last) UNION)))
                            acc))))
                   (c (re/code acc interval
                               (or (null? acc) UNION))
                      rest))))
             (else
              (u u
                 (cdr s)
                 (lambda (acc rest)
                   ;; single char
                   (c (re/code (NEW (car s)) acc
                               (or (null? acc) UNION))
                      rest)))))))))
(define re/new/char
  (lambda (e y)
    (y (NEW (car e))
       (cdr e))))
(define re/processor
  (lambda (code)
    (define op?
      (lambda (op)
        (lambda (code)
          (eq? (caar code) op))))

    (define POP?
      (op? 'POP))
    (define PUSH?
      (op? 'PUSH))
    (define SET?
      (op? 'SET))
    (define NEW?
      (op? 'NEW))
    (define JOIN?
      (op? 'JOIN))
    (define ALTERNATIVE?
      (op? 'ALTERNATIVE))
    (define KLEENE?
      (op? 'KLEENE))

    (define NEW
      (lambda ()
        '$))

    ((lambda (u) (u u code '() (NEW) (NEW)))
     (lambda (u code stack x y)
       (cond ((null? code)
              (car stack))
             ((POP? code)
              (define reg (cadar code))
              (case reg
                ('X (u u (cdr code) (cdr stack) (car stack) y))
                ('Y (u u (cdr code) (cdr stack) x (car stack)))))
             ((PUSH? code)
              (define reg (cadar code))
              (case reg
                ('X (u u (cdr code) (cons x stack) x y))
                ('Y (u u (cdr code) (cons y stack) x y))))
             ((NEW? code)
              (define reg (cadar code))
              (case reg
                ('X (u u (cdr code) stack (NEW) y))
                ('Y (u u (cdr code) stack x (NEW)))))
             ((SET? code)
              (define reg (cadar code))
              (define v (caddar code))
              (case reg
                ('X (u u (cdr code) stack v y))
                ('Y (u u (cdr code) stack x v))))
             ((JOIN? code)
              (define regA (cadar code))
              (define regB (caddar code))
              (define v0 (case regA ('X x) ('Y y)))
              (define v1 (case regB ('X x) ('Y y)))
              (define v `(CONCAT ,v1 ,v0))
              (case regA
                ('X (u u (cdr code) stack v y))
                ('Y (u u (cdr code) stack x v))))
             ((ALTERNATIVE? code)
              (define regA (cadar code))
              (define regB (caddar code))
              (define v0 (case regA ('X x) ('Y y)))
              (define v1 (case regB ('X x) ('Y y)))
              (define v `(UNION ,v1 ,v0))
              (case regA
                ('X (u u (cdr code) stack v y))
                ('Y (u u (cdr code) stack x v))))
             ((KLEENE? code)
              (define reg (cadar code))
              (define v0 (case reg ('X x) ('Y y)))
              (define v `(REPEAT 0 INF ,v0))
              (case reg
                ('X (u u (cdr code) stack v y))
                ('Y (u u (cdr code) stack x v))))
             (else
              (error "regular expression processor:"
                     'ALERT!!! code)))))))

;;;  A compiler from the Backus-Naur form to a language that encodes
;;;  something similar to a shift-reduce lr table.
(define eval
  (lambda (expr)
    "We have some abstractions in variables.  No need to dynamically
define other abstractions (analogous of groups in more general
regexps).  So the environment does not change."
    (cond
     ((atom? expr)          expr)
     ((variable? expr)      (eval-variable expr))
     ((regex? expr)         (eval-regular-expression expr))
     ((concatenation? expr) (eval-concatenation expr))
     ((union? expr)         (eval-union (operands expr)))
     ((select? expr)        (eval-select (operands expr)))
     ((repeat? expr)        (eval-repeat (operands expr)))
     ((complement? expr)    (eval-complement (operands expr)))
     (else (apply expr)))))

(define apply
  (lambda (expr)
    "APPLY makes a reduction in the input stream.  All the other
combinators make a shift from the stream to the accumulator."
    (list 'reduce
          (second expr)
          (eval (car expr)))))

;;; =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
;;; =~=~=~=~=~=~=~=~=~=~=~=~=~= A LR PARSER ~=~=~=~=~=~=~=~=~=~=~=~=~=~=
;;; =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

(define lr/select
  (lambda (code acc stream yield f)
    (define token '())
    ((lambda (u) (u u code))
     (lambda (u code)
       (cond ((and (empty? code) (not (empty? token)))
              (yield token))
             ((empty? code)
              (f))
             (else
              (lr (car code)
                   acc
                   stream
                   (lambda (v next)
                     (and (> (length v) (length token))
                          (set! token v))
                     (next))
                   (lambda ()
                     (u u (cdr code))))))))))

(define lr/union
  (lambda (code acc stream s f)
    ((lambda (u) (u u code))
     (lambda (u code)
       (if (empty? code)
           (f)
           (lr (car code)
                acc
                stream
                s
                (lambda ()
                  (u u
                     (cdr code)))))))))

(define lr/reduce
  (lambda (code acc stream s f)
    (define token (car code))
    (define code-reduce (cadr code))
    (lr code-reduce
         acc
         stream
         (lambda (v f2)
           (s (cons token v) f2))
         f)))

(define lr/concat
  (lambda (code acc stream s f)
    ((lambda (u) (u u code '() stream
               (lambda (a fx)
                 (s (append a acc) fx))
               f))
     (lambda (u code acc0 stream0 s0 f0)
       (if (empty? code)
           (s0 acc0 f0)
           (lr (car code)
                acc0
                stream0
                (lambda (v f2)
                  (u u
                     (cdr code)
                     v
                     (drop stream (length v))
                     s0
                     f2))
                f0))))))

(define lr/kleene
  (lambda (code acc stream s f)
    (define code-kleene (car code))
    ((lambda (u) (u u '() stream f))
     (lambda (u acc0 stream0 f0)
       (lr code-kleene
            acc0
            stream0
            (lambda (v f2)
              (u u
                 v
                 (drop stream (length v))
                 f2))
            (lambda ()
              (s (append acc0 acc)
                 f0)))))))

(define lr
  (lambda (code acc stream s b)
    (cond ((and (atom? code)
                (char=? code (car stream)))
           (s (cons (car stream) acc)
              b))
          ((atom? code)
           (b))
          (else
           ((case (operator code)
              ('select lr/select)
              ('union  lr/union)
              ('reduce lr/reduce)
              ('concat lr/concat)
              ('kleene lr/kleene))
            (operands code)
            acc
            stream
            s
            b)))))

;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~= LEX

(define new-token
  (lambda (type name)

    (define digit-value
      (let ((vals
             (make-hash
              '((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4)
                (#\5 . 5) (#\6 . 6) (#\7 . 7) (#\8 . 8) (#\9 . 9)
                (#\a . 10) (#\b . 11) (#\c . 12)
                (#\d . 13) (#\e . 14) (#\f . 15)
                (#\A . 10) (#\B . 11) (#\C . 12)
                (#\D . 13) (#\E . 14) (#\F . 15)))))
        (lambda (symbol)
          (hash-ref vals symbol))))
    (define base->dec
      (lambda (data base)
        (foldl (lambda (digit y)
                 (+ (* base y)
                    (digit-value digit)))
               0 data)))

    (define hex-constant?
      (lambda (data)
        (and (eq? (car data) #\0)
             (> (length data) 1)
             (memq (cadr data) '(#\x #\X)))))
    (define octal-constant?
      (lambda (data)
        (eq? (car data) #\0)))
    (define dec-constant?
      (lambda (data)
        (memq (car data) '(#\1 #\2 #\3
                           #\4 #\5 #\6
                           #\7 #\8 #\9))))
    (define escape-seq?
      (lambda (data)
        (and (= (length data) 4)
             (eq? (car data) #\')
             (eq? (cadr data) #\\)
             (eq? (last data) #\'))))
    (define char-constant?
      (lambda (data)
        (and (= (length data) 3)
             (eq? (car data) #\')
             (eq? (last data) #\'))))

    (define make/constant
      (lambda (data)
        (cond ((hex-constant? data)
               ;; HEX
               (base->dec (drop data 2) 16))
              ((octal-constant? data)
               ;; OCTAL
               (base->dec (drop data 1) 8))
              ((dec-constant? data)
               ;; DEC
               (string->number datastr))
              ((escape-seq? data)
               ;; ESCAPE SEQUENCE
               (case (third data)
                 ((#\n)  10)           ; newline
                 ((#\t)   9)           ; tab
                 (else (error "char-constant: todo" type name))))
              ((char-constant? data)
               ;; CHAR
               (char->integer (second data)))
              (else
               (error "constant: todo" type name)))))
    (define make/string
      (lambda ()
        (string-trim datastr #:left? true #:right? true "\"")))

    (define datastr (foldl ~a "" name))
    (define datasym (string->symbol datastr))
    (case type
      ('@blank   false)
      ('@comment false)
      ('@keyword (cons 'key    datasym))
      ('@id      (cons 'id     datasym))
      ('@punct   (cons 'punct  datasym))
      ('@const   (cons 'const  (make/constant (reverse name))))
      ('@string  (cons 'string (make/string)))
      (else (error "token: TODO" type name)))))

(define token-generator
  (lambda (stream start-symbol env)
    (lambda (return/writer)
      (set! lexical-environment env)
      (define bnf/grammar/code (eval start-symbol))
      ((lambda (s)
         (lambda ()
           (s s stream)))
       (lambda (s stream)
         (if (empty? stream)
             (return/writer (lambda () 'done) 'EOS)
             (lr bnf/grammar/code
                 '()
                 stream
                 (lambda (tok/str)
                   (let ((TOK (new-token (car tok/str) (cdr tok/str))))
                     (if TOK
                         (return/writer
                          (lambda () (s s (drop stream (length (cdr tok/str)))))
                          TOK)
                         (s s (drop stream (length (cdr tok/str)))))))
                 (lambda ()
                   (d "--\n"
                      (list->string stream)
                      "--")
                   (error "token error -- cannot continue")))))))))

(define lex-pipe
      (lambda (return)
        "like in a UNIX pipe or thread, a process produces and the
other consumes symbols."
        (define yield 'nil)
        (define resume 'nil)
        (define (writer co v)
          (set! resume co)
          (yield v))
        (define (reader co)
          (set! yield co)
          (resume))
        (set! resume (return writer))
        reader))

(define lex
  (lambda (stream start-symbol env)
    (let ((K (lex-pipe (token-generator stream start-symbol env))))
      (define back (list))
      (define forw (list))
      (define read-more
        (lambda (m)
          ((lambda (s)
             (s s m))
           (lambda (s c)
             (K
              (lambda (tok)
                (cond ((eq? 'EOS tok)
                       (set! forw (append forw (list 'END-OF-STREAM))))
                      ((zero? c)
                       (set! forw (append forw (list tok))))
                      (else
                       (set! forw (append forw (list tok)))
                       (s s (- c 1))))))))))
      (define STR
        (lambda ()
          (~a "FORW=" forw "  ::  BACK=" back) 'S))
      (define NULL?
        (lambda () (eq? (car forw) 'END-OF-STREAM)))
      (define PUSH
        (lambda (k)
          (and (null? forw) (read-more 1))
          (set! back (cons (car forw) back))
          (set! forw (cdr forw))
          (or (zero? k)
              (PUSH (- k 1)))))
      (define GET
        (lambda (k)
          (and (>= k (length forw))
               (read-more (add1 (- k (length forw)))))
          (car (drop forw k))))
      ((lambda (s) (s s))
       (lambda (s)
         (lambda (m . data)
           (case m
             ('PUSH
              (PUSH (sub1 (if (null? data) 1 (car data))))
              (s s))
             ('GET
              (GET (car data)))
             ('NULL?
              (and (null? forw) (read-more 1))
              (NULL?))
             ('FAIL
              (read-more -1)
              forw)
             (else
              (error "LEX -- unknown message" m)))))))))

;;;

(provide lex)


