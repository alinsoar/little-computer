#lang racket ; -*- mode:racket ; buffer-read-only:t -*-

;;; Compiler from the language of hdl equations to the language of
;;; hardware structures.

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

(require "hdl-semantics.rkt")
(require "tools.rkt")

;;; `MODULE-LIST` is a list of atomic structures used as output of
;;; compilation.  Each module in this list is compiled as an atom.  By
;;; default there is a single atom -- Nand.  The compilation of each
;;; structure will be made only in terms of Nand and those structures
;;; present in this list.
(define MODULE-LIST
  '(
    Bit
    Register
    RAM16K
    RAM4K
    RAM512
    RAM8
    RAM64
    CPU
    Inc16
    Add16
    Mux
    ;; ALU
    ;; Not
    ))

(define global/environment
  (or (eq? sem/bytecode 'fail)
      (make-immutable-hash (cdr sem/bytecode))))
(define global/dbg/symbols
  (or (eq? sem/bytecode 'fail)
      (make-immutable-hash (car sem/bytecode))))
(define lookup
  (lambda (id)
    (let ((def (hash-ref global/environment id)))
      (values (first def) (third def)))))
(define lookup/alias
  (lambda (id)
    (let ((a (hash-ref global/environment id (lambda () false))))
      (and a (cadr a)))))
(define lookup/dbg/sym
  (lambda (id)
    (let ((def (hash-ref global/dbg/symbols id)))
      (fourth def))))

;;; Atomic Gates

(define Nand
  (lambda (io mod)
    (mod/add mod 'Nand io)))

(define make/s-table
  (lambda (code co)
    "make the symbol table for the code of a given module"
    (define input-syms (vector->list (cdar code)))
    (define instr (cdr code))
    (define IOLEN (length input-syms))
    (define s-table (make-hash
                     (map cons
                          (cons 'GND
                                (cons 'VDD
                                      (cons 'CLK
                                            input-syms)))
                          (cons 'GND
                                (cons 'VDD
                                      (cons 'CLK
                                            (range IOLEN)))))))
    
    (counter/reset 'xyz IOLEN)
    
    (map (lambda (i)
           (define fun (car i))
           (define args (cdr i))
           (vector-map (lambda (a) (hash-ref! s-table
                                         a
                                         (lambda () (sub1 (1+ 'xyz)))))
                       args))
         instr)
    (co (- (counter/get 'xyz) IOLEN)
        instr
        (lambda (id)
          (hash-ref s-table id)))))
(define subst
  (lambda (s-table instr)
    (map (lambda (i)
           (define fun (car i))
           (define args (cdr i))
           (cons fun (vector-map s-table args)))
         instr)))

(define hw/modules
  (let ((modules (make-hash)) (aliases (make-hash)))
    (define solve-alias
      (lambda (instr mod)
        (let ((mod/aliases (hash-ref aliases mod)))
          (cons (car instr)
                (vector-map! (lambda (x)
                               (hash-ref mod/aliases x (lambda () x)))
                             (cdr instr))))))
    (define get-bytecode
      (lambda ()
        (hash-map modules
                  (lambda (mod v)
                    ((lambda (s)
                       (make/s-table (s s v '())
                                     (lambda (len i s)
                                       (list mod len
                                             (subst s i)))))
                     (lambda (s instr r)
                       (if (empty? instr)
                           r
                           (s s
                              (rest instr)
                              (cons (solve-alias (car instr) mod)
                                    r)))))))))
    (lambda (m)
      (define (m/data) (hash-ref! modules m (lambda () '())))
      (case m
        ;; class data
        ('$ALL-IDS (hash-keys modules))
        ('$BYTECODE (get-bytecode))
        (else
         (lambda (msg)
           (case msg
             ;; instance data
             ('ADD (lambda (fid as)
                     (hash-set! modules m (cons (cons fid as) (m/data)))))
             ('SHOW (m/data))
             ('NEW (lambda (args)
                     (hash-set! modules m (list (cons "env" args)))
                     (hash-set! aliases m (make-hash))))
             ('ALIAS (lambda (a b) (hash-set! (hash-ref aliases m) a b))))))))))
(define mod/all
  (lambda ()
    (hw/modules '$ALL-IDS)))
(define mod/get/bytecode
  (lambda ()
    (hw/modules '$BYTECODE)))
(define mod/new
  (lambda (fid args)
    (let ((new (hw/modules fid)))
      ((new 'NEW) args)
      new)))
(define mod/show
  (lambda (mod)
    (mod 'SHOW)))
(define mod/add
  (lambda (mod fId args)
    ((mod 'ADD) fId args)))
(define mod/alias
  (lambda (mod x y)
    ((mod 'ALIAS) x y)))

;;; PREDICATES

(define Nand?
  (lambda (id)
    (eq? id 'Nand)))
(define module?
  (lambda (id)
    (member id MODULE-LIST)))
(define lisp/instrument?
  (lambda (sym)
    (char-lower-case? (string-ref (symbol->string sym) 0))))
(define module-already-expanded?
  (lambda (id)
    (and (module? id) (> (counter/get id) 1))))

;;; HARDWARE BUILDER

(define unit
  (lambda (x)
    (lambda (M)
      (max x M))))

(define >>=
  (lambda (ma next)
    "CONTROL  --  checks  for  cyclic  definitions  such  as
     A(..., X, ...)
     X(..., A, ...)."
    (lambda (global/deep)
      (let ((control (ma global/deep)))
        (if (and (number? control)
                 (< control 100))
            ((next) control)
            'circulus-vitiosus)))))

(define new/internal/symbol
  (lambda _
    (1+ 'internal/symbols)))

(define eval
  (lambda (funID env mod deep)
    "like in lambda calculus, EVAL computes the value of an object."
    (1+ funID)
    (cond ((Nand? funID)
           (Nand env mod)
           (unit deep))
          ((module-already-expanded? funID)
           ;; the modules are memoized -- module was already computed.
           (mod/add mod funID env)
           (unit deep))
          (else
           (define-values (intern/len sem/bc) (lookup funID))
           (apply sem/bc
                  env
                  (build-vector intern/len new/internal/symbol)
                  (cond ((module? funID)
                         ;; compile a module (as an atomic structure).
                         (mod/add mod funID env)
                         (mod/new funID env))
                        (else
                         ;; expand mod in smaller pieces.
                         mod))
                  deep)))))

(define apply
  (lambda (bc args intern module deep)
    "like in lambda calculus, APPLY substitutes the values computed by
eval for their names."
    (define (get/w idx)
      (define io? (lambda () (or (positive? idx) (zero? idx))))
      (define in? (lambda () (negative? idx)))
      (cond ((eq? idx 'GND) 'GND)
            ((eq? idx 'VDD) 'VDD)
            ((eq? idx 'CLK) 'CLK)
            ((io?) (vector-ref args idx))
            ((in?) (vector-ref intern (- (add1 idx))))))
    ((lambda (s) (s s bc))
     (lambda (s bc)
       (cond ((empty? bc)
              (unit deep))
             (else
              (define funID (caar bc))
              (define input/output/params (cdar bc))
              (define IO (vector-map get/w input/output/params))
              (define alias (lookup/alias funID))
              (and alias
                   (for-each (lambda (out)
                               (mod/alias module
                                          (vector-ref IO (car out))
                                          (vector-ref IO (cdr out))))
                             alias))
              (cond ((lisp/instrument? funID)
                     (mod/add module funID IO)
                     (s s (rest bc)))
                    (else
                     (>>= (>>= (lambda (g/m) (max g/m deep))
                               (lambda ()
                                 "populate MODULE with a structure isomorphic with funID"
                                 ;; funID will allocate its internal
                                 ;; wires and will connect its input
                                 ;; and output to these provided IO
                                 ;; params. The element funID will be
                                 ;; expanded in module as a structure
                                 ;; isomorphic with the computation
                                 ;; structure funID.
                                 (eval funID IO module (add1 deep))))
                          (lambda ()
                            (s s (rest bc))))))))))))

;;; USER INTERFACE

(define make/hw/files
  (lambda ()
    (define compiled/modules (mod/get/bytecode))
    (define remove-old-compiled-files
      (lambda ()
        (for-each delete-file
                  (find-files (lambda (x)
                                (and (bytes? (filename-extension x))
                                     (bytes=? (filename-extension x) #"hw")))
                              "./hw"))))
    (define STDOUT
      p)
    (define FILE
      (let ((FILE 'nihil)
            (modname 'nihil))
        (define OPEN
          (lambda (name)
            (set! modname name)
            (set! FILE
                  (open-output-file
                   (string-append
                    "./hw/"
                    (symbol->string name) ".hw")
                   #:exists 'truncate))))
        (define CLOSE
          (lambda ()
            (close-output-port FILE)))
        (define SAVE
          (lambda (m)
            (display m FILE)
            (newline FILE)))
        (define HEADER
          (lambda (internal/wires)
            (SAVE "-- This file was automatically generated by hardware compiler.")
            (SAVE modname)
            (SAVE internal/wires)))
        (lambda (op . data)
          (case op
            ('HEADER (HEADER (car data)))
            ('SAVE (SAVE (car data)))
            ('CLOSE CLOSE)
            ('OPEN (OPEN (car data)))))))
    (remove-old-compiled-files)
    (STDOUT "Saving compiled modules: ")
    (map (lambda (mod)
           (define modname (car mod))
           (define internal/wires (cadr mod))
           (define instr (caddr mod))
           (STDOUT modname "; ")
           (FILE 'OPEN modname)
           (FILE 'HEADER internal/wires)
           (map
            (lambda (i)
              (define fun (car i))
              (define args (vector->list (cdr i)))
              (define k (foldl (lambda (x r) (~a r " " x))
                               fun
                               args))
              (FILE 'SAVE k))
            instr)
           (FILE 'CLOSE))
         compiled/modules)
    (newline)
    'done))

(define compile
  (lambda (module . wires)
    
    (define (eva-lu-ator s f funId . args)
      (let ((as (list->vector (flatten args))))
        (let ((result
               ((eval funId
                      as
                      (mod/new funId as)
                      0)
                0)))
          (if (number? result) (s result) f))))
    
    (cond
     ((hash? global/environment)
      (d "Compiling" module)
      ((eva-lu-ator (lambda (maxdef)
                      (lambda ()
                        (d (~a "Longest definition: " maxdef))
                        (make/hw/files)
                        (newline)))
                    (lambda ()
                      (d "There is a cyclic definition in the .hdl files."))
                    module
                    wires)))
     (else
      (d "Cannot continue.")))))

(compile 'Computer 'reset)
