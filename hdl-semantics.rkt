#lang racket ; -*- mode:racket ; buffer-read-only:t -*-

;;; The semantics of the hardware description language.

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

;;; DEBUGGING

(define DEBUG/SEMANT false)

;;; Helpers

(define at?
  (lambda (ch)
    (char=? #\@ ch)))

(define lisp/instrument?
  (lambda (sym)
    (let ((sname (symbol->string sym)))
      (at? (string-ref sname 0)))))

(define lisp-instrument-name
  (lambda (sym)
    (let ((sname (symbol->string sym)))
      (and (at? (string-ref sname 0))
           (string->symbol (substring sname 1))))))

(define is/O?
  (lambda (w)
    (eq? 'O w)))

(define is/I?
  (lambda (w)
    (eq? 'I w)))

(define is/0/1?
  (lambda (w)
    (pair? (memq w '(GND VDD CLK)))))

(define diffset
  (lambda (set1 set2)
    "elements in set1 that are not in set2"
    (filter (lambda (x) (not (member x set2))) set1)))

;;; Stage 2 ---

(define s2/bytecode-generator
  (lambda (body c co)
    "Quot linguas calles, tot homines vales"

    (define input (car body))
    (define output (cadr body))
    (define instruments (caddr body))
    (define intern (cadddr body))
    (define boxes (cadr (cdddr body)))

    (define io/header (append input output))
    
    (define idx
      (lambda (l e)
        (define (iter l n)
          (cond ((null? l) false)
                ((equal? e (car l)) n)
                (else (iter (cdr l) (add1 n)))))
        (iter l 0)))

    (define get-pointer
      (lambda (id)
        (cond ((eq? id 'GND) 'GND)
              ((eq? id 'VDD) 'VDD)
              ((eq? id 'CLK) 'CLK)
              ((idx io/header id))
              ((- -1 (idx intern id))))))

    (define update/structure
      (lambda (params co)
        ((lambda (s) (s s params
                   (lambda (pointers)
                     (co pointers))))
         (lambda (s params co)
           (if (empty? params)
               (co '())
               (s s (cdr params)
                  (lambda (pointers)
                    (co (cons (get-pointer (car params))
                              pointers)))))))))

    ((lambda (s i o)
       (s s boxes
          (lambda (bc/s)
            (i i instruments bc/s
               (lambda (bc/i)
                 (o o output
                    (lambda (bc/o)
                      (co (list (length intern)
                                bc/o
                                bc/i)))))))))
     (lambda (s structures co)
       (if (empty? structures)
           (co '())
           (s s
              (cdr structures)
              (lambda (struct0)
                (define function (caar structures))
                (define params (cdar structures))
                (define input (car params))
                (define output (cdr params))
                (update/structure (append input output)
                                  (lambda (struct)
                                    (co (cons (cons function (list->vector struct))
                                              struct0))))))))
     (lambda (s instruments bc/structs co)
       (if (empty? instruments)
           (co bc/structs)
           (s s
              (cdr instruments)
              bc/structs
              (lambda (instr0+boxes0)
                (define instrument (car instruments))
                (define function (car instrument))
                (define args (cdr instrument))
                (update/structure args
                                  (lambda (instr)
                                    (co (cons (cons function (list->vector instr))
                                              instr0+boxes0))))))))
     (lambda (s o co)
       (update/structure
        o
        (lambda (s)
          (define indexof
            (lambda (e l i)
              (cond ((null? l) false)
                    ((eq? e (car l)) i)
                    (else (indexof e (cdr l) (+ 1 i))))))
          ;; the outputs from this list will be unified by the
          ;; hardware compiler when the current structure is expanded
          ;; by some combination.
          (co (filter
               pair?
               (map (lambda (e k)
                      (let ((w (indexof e s 0)))
                        (if (= w k)
                            'not-interesting
                            (cons (+ (length input) k) (+ (length input) w)))))
                    s
                    (range (length s)))))))))))

(define s2/unify
  (lambda (body out co)

    (define initial-signature '())

    (define output/group/generator
      (lambda (group)
        ((lambda (s)
           (if (empty? group)
               'GND
               (s s group
                  (lambda (out/wire)
                    ;; if there is an output wire in the group, we
                    ;; give it priority, otherwise we select the first
                    ;; element of the group as the generator.
                    (or out/wire (car group))))))
         (lambda (s els co)
           (if (empty? els)
               (co false)
               (s s
                  (cdr els)
                  (lambda (e)
                    (if (member (car els) out)
                        (co (car els))
                        (co e)))))))))

    (define update/signature/group
      (lambda (group generator sigma co)
        ((lambda (s) (s s group
                   (lambda (sigma)
                     (co sigma))))
         (lambda (s els co)
           (if (empty? els)
               (co sigma)
               (s s
                  (cdr els)
                  (lambda (sigma)
                    (if (is/0/1? (car els))
                        (co sigma)
                        (co (cons (cons (car els) generator)
                                  sigma))))))))))

    (define update/signature/structure
      (lambda (o/ports sigma)
        ((lambda (s) (s s o/ports (lambda (x) x)))
         (lambda (s groups co)
           (if (empty? groups)
               (co sigma)
               (s s
                  (cdr groups)
                  (lambda (sigma)
                    (co
                     (update/signature/group
                      (car groups)
                      (output/group/generator (car groups))
                      sigma
                      (lambda (s) s))))))))))

    ((lambda (s) (s s
               body
               (lambda (sig)
                 (co sig))))
     (lambda (s structures co)
       (if (empty? structures)
           (co initial-signature)
           (s s
              (cdr structures)
              (lambda (sig)
                (let ((o/ports (caddar structures)))
                  (co
                   (update/signature/structure o/ports sig))))))))))

(define s2/apply/signature/fundef
  (lambda (sigma structures instruments out co)
    (define S
      (lambda (e)
        (cond
         ;; some output pin was not initialized.
         ;; we generate GND by default
         ((empty? e) 'GND)
         ;; 0 -- right value
         ((eq? e 'GND) 'GND)
         ;; 1 -- right value
         ((eq? e 'VDD) 'VDD)
         ;; system clock -- right value
         ((eq? e 'CLK) 'CLK)
         ;; defined by the output port of some box
         ((assoc e sigma) (cdr (assoc e sigma)))
         ;; defined in the input of some gate
         (else e))))

    (define out/update
      (map S out))

    (define update/structure
      (lambda (ip op co)
        (define ip/update (map S ip))

        (define op/generators
          (map (lambda (x) (if (pair? x) (car x) x)) op))

        (define op/update
          (map S op/generators))

        (co (cons ip/update
                  op/update)
            (list->set
             (filter (lambda (x) (not (or (member x out/update)
                                     (is/0/1? x))))
                     op/update)))))
    ((lambda (s i) (s s
                 structures
                 (lambda (updated/structures iw)
                   (i i
                      instruments
                      (lambda (updated/instruments)
                        (co updated/structures
                            updated/instruments
                            out/update
                            iw))))))
     (lambda (s structures co)
       (if (empty? structures)
           (co '() (list->set '()))
           (s s
              (cdr structures)
              (lambda (up/struct0 iw0)
                (define next (car structures))
                (define function (car next))
                (define params (cdr next))
                (define i/ports (car params))
                (define o/ports (cadr params))
                (update/structure
                 i/ports
                 o/ports
                 (lambda (struct iw)
                   (co (cons (cons function struct)
                             up/struct0)
                       (set-union iw iw0))))))))
     (lambda (s instruments co)
       (if (empty? instruments)
           (co '())
           (s s
              (cdr instruments)
              (lambda (up/instrum0)
                (define instrument (caar instruments))
                (define args (cdar instruments))
                (co (cons (cons instrument (map S args))
                          up/instrum0)))))))))

;;; Stage 1 ---

(define LOG
  ;; Collect the log messages in register `M`, errors in register
  ;; `E` and warnings in register `W`."
  (let ((M '())
        (W '())
        (E '())
        (FILE 'nil)
        (sexp# 0))

    (define E+
      (lambda (ERR-TYPE ret . data)
        "Cujusvis hominis est errare"
        (define e
          (case ERR-TYPE
            ('BUS-INDEX (~a "Subbus index error -- " (car data) ":" (cadr data) "."))
            ('UNKNOWN-LV (~a "Unknown left value -- " (car data) "."))
            ('BUS-WIDTH (~a "Bus width does not match "
                            " -- " (car data)
                            " -- " (cadr data)
                            " -- " (caddr data) "."))
            ('MULTI-INPUT-SAME-PIN (~a "Cannot input multiple wires in the same pin"
                                       " -- " (car data)
                                       " -- " (cadr data) "."))
            ('MULTI-DEF (~a "The wire " (car data) " was defined multiple times."))
            ('UNDEF-INPUT (~a (map (lambda (x) (~a "Undefined input wires --" x "."))
                                   (car data))))
            ('UNDEF-INSTR-ARG (~a (map (lambda (x) (~a "undef instrument argument -- " x "."))
                                       (car data))))
            ('UNKNOWN-GATE (~a "Cannot find the gate definition for " (car data) "."))
            ('HDL-SYNTAX (error (~a (car data)
                                    " -- hdl syntax error -- assignment params must be pairs -- "
                                    (cadr data))))
            (else (error "unknown error" ERR-TYPE))))
        (set! E (cons (~a FILE ":" sexp# ":" e) E))
        (ret)))
    (define W+
      (lambda (WARN-TYPE ret . data)
        (define w
          (case WARN-TYPE
            ('FILENAME-GATENAME-MISMATCH (~a "file/chip mismatch " (car data) " vs. " (cadr data) "."))
            (else (error "unknown warning" WARN-TYPE))))
        (set! W (cons (list FILE sexp# w) W))))
    (define M+
      (lambda (LOG-TYPE ret . data)
        (define m
          (case LOG-TYPE
            ('LOAD-CHIP (~a "Loading chip " (car data) "."))
            (else (error "unknown log" LOG-TYPE))))
        (set! M (cons (list FILE sexp# m) M))))

    (define 1+sexp
      (lambda ()
        "this is useful for debugging. it signals the number of the
sexp that might be incorrect. --- TODO"
        (set! sexp# (add1 sexp#))))
    
    (lambda (m TYPE)
      (case m
        ('+ (case TYPE
              ('E E+)
              ('W W+)
              ('M M+)))
        ('? (case TYPE
              ('E (lambda () E))
              ('W (lambda () W))
              ('M (lambda () M))))
        ('SET-FILE! (lambda (name) (set! FILE name)))
        ('1+SEXP    1+sexp)))))
(define M+
  (LOG '+ 'M))
(define M?
  (LOG '? 'M))
(define W+
  (LOG '+ 'W))
(define W?
  (LOG '? 'W))
(define E+
  (LOG '+ 'E))
(define E?
  (LOG '? 'E))
(define SET-FILE!
  (LOG 'SET-FILE! '_))

(define s1/expand/subbus/pins
  (lambda (sym i j success failure)
    (if (> i j)
        (E+ 'BUS-INDEX failure i j)
        (success
         (map (lambda (x) (cons sym x))
              (range i (add1 j)))))))

(define s1/preprocess/io/header
  (lambda (pins next)
    (if (empty? pins)
        (next '())
        (s1/preprocess/io/header
         (cdr pins)
         (lambda (v0)
           (let* ((bus (car pins)))
             (s1/expand/subbus/pins
              (car bus)
              0
              (sub1 (second bus))
              (lambda (v) (next (append v v0)))
              (lambda () (next v0)))))))))

(define s1/load/chip
  (lambda (file col)
    (define s1/check/filename/chipname
      (lambda (name)
        (or (equal? (path->string file) (~a name ".HDL"))
            (W+ 'FILENAME-GATENAME-MISMATCH
                (lambda _ _)
                (path->string file)
                (~a name ".HDL")))))

    (and DEBUG/SEMANT
         (M+ 'LOAD-CHIP (lambda _ _) file))
    
    (let ((name (string->symbol
                 (substring (path->string file)
                            0
                            (- (string-length (path->string file)) 4)))))
      
      (s1/check/filename/chipname name)

      (define INx 'nil)
      (define OUTx 'nil)
      (define BODYxi 'nil)
      (define BODYxc 'nil)

      (define-syntax defgate
        (syntax-rules (IN OUT)
          ((defgate gatename (IN i ...) (OUT o ...) k ...)
           (let ((io (lambda (data) (map (lambda (x) (if (symbol? x) (list x 1) x)) data))))
             (and (equal? 'gatename name)
                  (set! INx (io '(i ... ))))
             (and (equal? 'gatename name)
                  (set! OUTx (io '(o ... ))))
             (and (equal? 'gatename name)
                  (set! BODYxi
                        (map (lambda (a) (cons (lisp-instrument-name (car a))
                                          (cdr a)))
                             (filter (lambda (a) (lisp/instrument? (car a)))
                                     '(k ... ))))
                  (set! BODYxc
                        (filter (lambda (e)
                                  (and (false? (lisp/instrument? (car e)))
                                       (for-each
                                        (lambda (a)
                                          (or (and (pair? a)
                                                   (= 2 (length a)))
                                              (E+ 'HDL-SYNTAX (lambda _ _) name e)))
                                        (cdr e))))
                                '(k ... ))))))))

      ;; MIT/SCHEME
      (include "./hdl-hack/RAM8.HDL")
      (include "./hdl-hack/And.HDL")
      (include "./hdl-hack/DMux8Way.HDL")
      (include "./hdl-hack/PC.HDL")
      (include "./hdl-hack/Bit.HDL")
      (include "./hdl-hack/Or.HDL")
      (include "./hdl-hack/Not16.HDL")
      (include "./hdl-hack/FullAdder.HDL")
      (include "./hdl-hack/RSLatch.HDL")
      (include "./hdl-hack/Computer.HDL")
      (include "./hdl-hack/Or8Way.HDL")
      (include "./hdl-hack/Register.HDL")
      (include "./hdl-hack/RAM64.HDL")
      (include "./hdl-hack/RAM4K.HDL")
      (include "./hdl-hack/Inc16.HDL")
      (include "./hdl-hack/Zero16.HDL")
      (include "./hdl-hack/Xor.HDL")
      (include "./hdl-hack/RAM16K.HDL")
      (include "./hdl-hack/CondNeg16.HDL")
      (include "./hdl-hack/Keyboard.HDL")
      (include "./hdl-hack/HalfAdder.HDL")
      (include "./hdl-hack/ALU.HDL")
      (include "./hdl-hack/Mux8Way16.HDL")
      (include "./hdl-hack/Mux4Way16.HDL")
      (include "./hdl-hack/ROM32K.HDL")
      (include "./hdl-hack/DLatch.HDL")
      (include "./hdl-hack/Xor16.HDL")
      (include "./hdl-hack/Nand.HDL")
      (include "./hdl-hack/DMux4Way.HDL")
      (include "./hdl-hack/Screen.HDL")
      (include "./hdl-hack/And16.HDL")
      (include "./hdl-hack/Mux16.HDL")
      (include "./hdl-hack/Or16.HDL")
      (include "./hdl-hack/Mux.HDL")
      (include "./hdl-hack/DMux.HDL")
      (include "./hdl-hack/Add16.HDL")
      (include "./hdl-hack/DFF.HDL")
      (include "./hdl-hack/RAM512.HDL")
      (include "./hdl-hack/CPU.HDL")
      (include "./hdl-hack/RSFlipFlop.HDL")
      (include "./hdl-hack/Memory.HDL")
      (include "./hdl-hack/Not.HDL")

      (s1/preprocess/io/header          ; >> INPUT HEADER
       INx
       (lambda (input)                       ; << INPUT
         (s1/preprocess/io/header       ; >> OUTPUT HEADER
          OUTx
          (lambda (output)                   ; <<OUTPUT
            (col name input output BODYxc BODYxi))))))))

(define s1/resolve/left/value
  (lambda (sym fun/header/in fun/header/out succ fail)
    (define get/addr
      (lambda (ID co)
        "given the name of a symbol, it checks the function input
header and makes a list with the addresses in the header which
corresponds to the given symbol. If SYM is not found in input header,
it tries in output header. If it still fails to find the address, it
signals an error."
        ((lambda (s p)
           (let ((Q (if (pair? ID) p s)))
             (Q Q fun/header/in
                0
                (lambda (i)
                  (if (empty? i)
                      (Q Q fun/header/out
                         0
                         (lambda (o)
                           (if (empty? o)
                               (E+ 'UNKNOWN-LV fail ID)
                               (co 'O o))))
                      (co 'I i))))))
         (lambda (s header/pins index co)
           "SYM is a SYMBOL"
           (cond
            ;; NOT FOUND
            ((empty? header/pins)
             (co '()))
            ;; FOUND
            ((eq? ID (caar header/pins))
             (s s
                (cdr header/pins)
                (add1 index)
                (lambda (idx)
                  (co (cons index idx)))))
            ;; LOOP AND COLLECT
            (else
             (s s
                (cdr header/pins)
                (add1 index)
                (lambda (idx)
                  (co idx))))))
         (lambda (s header/pins index co)
           "SYM IS A PAIR"
           (cond
            ;; NOT FOUND
            ((empty? header/pins)
             (co '()))
            ;; FOUND
            ((equal? ID (car header/pins))
             (co (list index)))
            ;; TRY NEXT
            (else
             (s s
                (cdr header/pins)
                (add1 index)
                (lambda (idx)
                  (co idx)))))))))

    (cond
     ;; Collect all the pins named `name` from Inputs or Output of the
     ;; structure being analysed.  If none is found in inputs or
     ;; outputs, then returns 'Undefined. If sym is a symbol, collect
     ;; a set of pins whose name is SYM-NAME.  Otherwise look only for
     ;; 1 pin in in/out that has the same name and index as SYM.
     ((symbol? sym)
      (get/addr sym (lambda (p/t a) (succ p/t a))))
     (else
      (define R ((if (= 2 (length sym)) cadr caddr) sym))
      (define L (cadr sym))
      (s1/expand/subbus/pins
       (car sym) L R
       (lambda (x)
         ((lambda (s) (s s x
                    (lambda (addr)
                      (let ((type (list->set (map car addr)))
                            (addr (map cadr addr)))
                        (if (= 1 (set-count type))
                            (succ (set-first type) addr)
                            (error "never here"))))))
          (lambda (s l co) (if (empty? l)
                          (co '())
                          (s s
                             (cdr l)
                             (lambda (address/list)
                               (get/addr
                                (car l)
                                (lambda (p/t a)
                                  (co (cons (cons p/t a) address/list))))))))))
       fail)))))

(define s1/resolve/right/value
  (lambda (right/value port/type length/left/value succ fail)
    " If we have a symbol of type Any/GND/VDD/CLK in the right side of an
assignment, it can be either a single wire, or it can be a bus. The
bus width is decided by the width of the left side, that at this point
is already known.
"
    (cond ((and (symbol? right/value)
                (memq right/value '(GND VDD CLK)))
           ;; SPECIAL GND VDD CLK
           (succ (make-list length/left/value right/value)))
          ((symbol? right/value)
           ;; ANY -- WIRE or BUS
           (s1/expand/subbus/pins
            right/value 0 (sub1 length/left/value)
            (lambda (v) (succ v))
            fail))
          (else
           ;; BUS
           (define R ((if (= 2 (length right/value)) cadr caddr)
                      right/value))
           (define L (cadr right/value))
           (if (= length/left/value (add1 (- R L)))
               (s1/expand/subbus/pins
                (car right/value) L R
                (lambda (v) (succ v))
                fail)
               (E+ 'BUS-WIDTH fail port/type length/left/value right/value))))))

(define s1/preprocess/assignment/list
  (lambda (header/in header/out environment undefined assignment/list IN OUT co)
    "Preprocess the means of combination.

           The internals of a structure (box).
          +------------------------------------------------+
          |    BOX (STRUCTURE) Internals                   |
          |                                                |
    IN0--->                                                ---> OUT0
          |    <--------------------+                      |
          |                         |                      |
    IN1--->        +---------- ENVIRONMENT-----------+     ---> OUT1
          |        |                                 |     |
          |        V  +---------------------------+  V     |
    IN2--->           |     |               |     |        ---> OUT2
          |       LV1 ~ RV1 |               | RVx ~ LVx    |
          |       LV2 ~ RV2 |               | RVy ~ LVy    |
    ...   |       LV3 ~ RV3 |               | RVz ~ LVz    |    ...
          |           | ... |  SOME         | ... |        |
          |           |     |  INTERNAL     |     |        |
          |           | I   |  STRUCTURE    | O   |        |
          |           | N P |  (BLACK BOX)  | U   |        |
          |           | P O |               | T P |        |
          |           | U R |               | P O |        |
          |           | T T |               | U R |        |
          |           |   S |               | T T |        |
          |           |     |               |   S |        |
          |           |     |               |     |        |
          |           +---------------------------+        |
          |                                                |
          +------------------------------------------------+

"
    (define input/vector (make-vector (length header/in) false))
    (define output/vector (make-vector (length header/out) '()))
    ((lambda (s) (s s assignment/list
               (lambda (updated/env updated/undef)
                 (co updated/env
                     updated/undef
                     (vector->list input/vector)
                     (vector->list output/vector)))))
     (lambda (s params co)
       (if (empty? params)
           (co environment undefined)
           (s s (cdr params)
              (lambda (env undef)
                (define assignment (car params))
                (define left/value (car assignment))
                (define right/value (cadr assignment))
                ;; ----------------------------------------- >> LEFT VALUE
                (s1/resolve/left/value
                 left/value
                 header/in
                 header/out
                 ;; ---------------------------------------- << ADDRESS
                 (lambda (port/type address)
                   (define LEFT/BUS/WIDTH (length address))
                   ;; -------------------------------------- >> RIGHT VALUE
                   (s1/resolve/right/value
                    right/value
                    port/type
                    LEFT/BUS/WIDTH
                    ;; ------------------------------------- << VALUE
                    (lambda (value)
                      ((lambda (s) (s s address value
                                 (lambda (up/env up/undef)
                                   (co up/env up/undef))))
                       (lambda (s addr/list val/list co)
                         ;; loop over addr/value pairs and populate
                         ;; the vectors that represent the input and
                         ;; output ports of FUNCTION.
                         ;;
                         ;; Each input port can be connected to a
                         ;; single wire, while output ports can have
                         ;; multiple wires.  For output ports, only 1
                         ;; such wire will be considered to generate
                         ;; the signal for the group of wires
                         ;; connected to that output port.  The other
                         ;; wires are considered aliases for that
                         ;; output port.  The unification will replace
                         ;; all the aliases with the generator of each
                         ;; group.
                         (cond
                          ((empty? addr/list)
                           (co env undef))
                          ((and (is/I? port/type)
                                (vector-ref input/vector (car addr/list)))
                           (s s (cdr addr/list) (cdr val/list)
                              (lambda (env undef)
                                (E+ 'MULTI-INPUT-SAME-PIN
                                    (lambda ()
                                      (co env undef))
                                    (car addr/list)
                                    (list-ref header/in (car addr/list))))))
                          ((is/I? port/type)
                           (s s (cdr addr/list) (cdr val/list)
                              (lambda (env undef)
                                (define a (car addr/list))
                                (define v (car val/list))
                                ;; SET UP THE `A` ENTRY IN INPUT PORTS
                                (vector-set! input/vector a v)
                                ;; add V to unknownks if v is not in environment.
                                (define unknowns
                                  (if (member v env)
                                      undef
                                      (cons v undef)))
                                (co env unknowns))))
                          ((and (is/O? port/type)
                                (not (is/0/1? (car val/list)))
                                (pair? (member (car val/list) env)))
                           (s s (cdr addr/list) (cdr val/list)
                              (lambda (env undef)
                                (E+ 'MULTI-DEF
                                    (lambda ()
                                      (co env undef))
                                    (car val/list)))))
                          ((is/O? port/type)
                           (s s (cdr addr/list) (cdr val/list)
                              (lambda (env undef)
                                (define a (car addr/list))
                                (define v (car val/list))
                                (define prev/val (vector-ref output/vector a))
                                ;; SET UP THE `A` ENTRY in OUTPUT PORTS
                                (vector-set! output/vector a (cons v prev/val))
                                ;; add V to environment and remove V from unknowns
                                (co (cons v env) (remove* (list v) undef)))))))))
                    (lambda ()
                      ;; in case left value is not valid, we go on
                      ;; collecting more errors, but we are no more
                      ;; interested about the value of i/o.
                      (co env undef))))
                 (lambda ()
                   (co env undef))))))))))

(define s1/preprocess/lisp/instruments
  (lambda (instruments env succ fail)
    (define p (q 'ok (or (empty? instruments) 'ok)))
    ((lambda (s) (s s instruments
               (lambda (~ f)
                 (define failed/args (apply append f))
                 (if (empty? failed/args)
                     (succ ~)
                     (fail failed/args)))))
     (lambda (s instr co)
       (cond ((empty? instr)
              (co '()'()))
             (else
              (define instrument (car instr))
              (define lisp-fun-id (car instrument))
              (define arguments (cdr instrument))
              (s s
                 (cdr instr)
                 (lambda (instr0 fail0)
                   ((lambda (s) (s s arguments
                              (lambda (a f)
                                (co (cons (cons lisp-fun-id
                                                (apply append a))
                                          instr0)
                                    (cons f fail0)))))
                    (lambda (s args co)
                      (if (empty? args)
                          (co '()'())
                          (s s
                             (cdr args)
                             (lambda (addr0 failed/pins)
                               (define arg (car args))
                               (s1/resolve/left/value
                                arg env '()
                                (lambda (_ address)
                                  (define subbus (map (lambda (i) (list-ref env i))
                                                      address))
                                  (co (cons subbus addr0)
                                      failed/pins))
                                (lambda ()
                                  (co addr0
                                      (cons arg failed/pins)))))))))))))))))

(define s1/preprocess/body/unification
  (lambda (IN OUT BOXES INSTRUMENTS in/headers out/headers co)
    "Preprocess the means of abstraction."
    (define initial-environment (append IN '(VDD GND CLK)))

    ((lambda (s)
       (s s BOXES
          (lambda (env undefined/input/wires i/gen-o)
            (if (empty? undefined/input/wires)
                ;; we can preprocess the instruments only after we
                ;; know the whole environment local to a gate.
                (s1/preprocess/lisp/instruments
                 INSTRUMENTS
                 ;; avoid CLK GND VDD as arg of some lisp instrument
                 (let ((NOT-SPECIAL-WIRE (diffset env '(VDD GND CLK))))
                   ;; only for lisp instruments allow access to
                   ;; non-initialized output wires.  we take care not
                   ;; to duplicate the outputs.  this is useful for
                   ;; the keyboard bus.
                   (let ((NOT-INIT-OUT-FOR-LISP-INSTRUM (diffset OUT NOT-SPECIAL-WIRE)))
                     (append NOT-SPECIAL-WIRE NOT-INIT-OUT-FOR-LISP-INSTRUM)))
                 (lambda (instruments)
                   ;; the i/o ports of each structure are processed
                   ;; after we finished to pre-process all the
                   ;; internal structures
                   (s2/unify i/gen-o                           ; >> UNIFICATION
                             OUT
                             (lambda (sigma)                        ; << SIGNATURE
                               (co sigma instruments i/gen-o))))
                 (lambda (undefined/instrument/argument)
                   (E+ 'UNDEF-INSTR-ARG
                       (lambda () (co '() '()'()))
                       undefined/instrument/argument)))
                (E+ 'UNDEF-INPUT
                    (lambda () (co '() '() '()))
                    undefined/input/wires)))))
     (lambda (s structure co)
       (if (empty? structure)
           (co initial-environment '() '())
           (s s
              (cdr structure)
              (lambda (env undef i/o0)
                (define next (car structure))
                (define fun/name (car next))
                (define params (cdr next))
                (if (pair? (assoc fun/name in/headers))
                    (s1/preprocess/assignment/list
                     (cdr (assoc fun/name in/headers))
                     (cdr (assoc fun/name out/headers))
                     env undef params IN OUT
                     (lambda (up/env undef i o)
                       (co up/env undef (cons (list fun/name i o) i/o0))))
                    (E+ 'UNKNOWN-GATE
                        (lambda () (co env undef '()))
                        fun/name)))))))))

(define s1/initialize/gates
  (lambda (success fatal)
    (define hdl/file? (lambda (f) (and (equal? #"HDL" (filename-extension f)) f)))
    (define hdl/files (filter hdl/file? (directory-list "./hdl-hack/")))
    ((lambda (s)
       (s s hdl/files '()'()'()'()'()
          (lambda (IDs in out body instrument)
            ;; after all the inputs and outputs were preprocessed, we
            ;; start preprocessing the body. During body preprocessing
            ;; we need to know the i/o for all the gates.
            ((lambda (s) (s s IDs '()'()
                       (lambda (structures bytecode)
                         (if (null? (E?))
                             (success structures bytecode)
                             (fatal)))))
             (lambda (s chip/list structures bytecode co)
               (if (empty? chip/list)
                   (co structures bytecode)
                   (let* ((chip (car chip/list)))
                     (define chip/in (cdr (assoc chip in)))
                     (define chip/out (cdr (assoc chip out)))
                     (define chip/body (cdr (assoc chip body)))
                     (define chip/instr (cdr (assoc chip instrument)))

                     ;; (d "........................................" chip)
                     (SET-FILE! chip)

                     (s1/preprocess/body/unification ; >> Symbolic representation
                      chip/in
                      chip/out
                      chip/body
                      chip/instr
                      in out
                      (lambda (sigma instruments i/gen-o)
                        (s2/apply/signature/fundef          ; >> APPLY SIGMA DEF
                         sigma
                         i/gen-o
                         instruments
                         chip/out
                         (lambda (body/structs                   ; << APPLY
                             instrum
                             out
                             internal/wires)
                           (let ((body/structures (list chip/in
                                                        out
                                                        instrum
                                                        (set->list internal/wires)
                                                        body/structs)))
                             (if (null? (E?))
                                 (s2/bytecode-generator ; >> Bytecode representation
                                  body/structures
                                  chip
                                  (lambda (bc)
                                    (s s
                                       (cdr chip/list)
                                       (cons (cons chip body/structures) structures)
                                       (cons (cons chip bc) bytecode)
                                       co)))
                                 (s s (cdr chip/list) '() '() co))))))))))))))
     (lambda (s files IDs in0 out0 body0 instrument0 co)
       (if (empty? files)
           (co IDs in0 out0 body0 instrument0)
           (s1/load/chip (car files)
                         (lambda (id in out structs instruments)
                           (s s
                              (cdr files)
                              (cons id IDs)
                              (cons (cons id in) in0)
                              (cons (cons id out) out0)
                              (cons (cons id structs) body0)
                              (cons (cons id instruments) instrument0)
                              co))))))))

(define bytecode
  (s1/initialize/gates
   (lambda (structures bytecode)
     (map (lambda (x) (d "MM" x)) (M?))
     (map (lambda (x) (d "WW" x)) (W?))
     (p "Loaded ")
     (for-each (lambda (x) (p x ", ")) (map car structures))
     (p "total " (length structures) " gates defined.\n")
     (cons structures bytecode))
   (lambda ()
     (map (lambda (x) (d "MM" x)) (M?))
     (map (lambda (x) (d "WW" x)) (W?))
     (map (lambda (x) (d ">>" x)) (E?))
     (d (length (E?)) "errors found.")
     'fail)))

(provide (rename-out (bytecode sem/bytecode)))
