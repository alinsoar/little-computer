#lang racket

(require "semantics.rkt")
(require "tools.rkt")
(require data/queue)

;;; COMPILER from the language of HDL EQUATIONS to the language of
;;; HARDWARE STRUCTURES.

;;; `MODULE-LIST` is a list of atomic structures used as output of
;;; compilation.  Each module in this list is compiled as an atom.  By
;;; default there is a single atom -- Nand.  The compilation of each
;;; structure will be made only in terms of Nand and those structures
;;; present in this list.
(define MODULE-LIST
  '(
    ;; Bit
    ;; RAM512
    RAM8
    CPU
    ;; Keyboard
    ;; RAM4K
    ;; HalfAdder
    ;; FullAdder
    ;; RSLatch
    ;; DLatch
    ;; DFF
    ;; Inc16
    ;; Add16
    ;; PC
    ;; ALU
    ;; RAM16K
    ;; RAM4K
    ;; Register
    ;; And
    ;; Mux
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
      (values (car def) (cdr def)))))
(define lookup/dbg/sym
  (lambda (id)
    (let ((def (hash-ref global/dbg/symbols id)))
      (fourth def))))

;;; Atomic Gates

(define Nand
  (lambda (io mod)
    (or (= 3 (vector-length io))
        (error "!!! wrong NAND call."))
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
           (vector-map (lambda (a)
                         (hash-ref! s-table a (lambda ()
                                                (define k (counter/get 'xyz))
                                                (1+ 'xyz)
                                                k)))
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
  (let ((modules (make-hash)))
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
                              (cons (first instr) r)))))))))
    (lambda (m)
      (case m
        ;; class data
        ('ALL-IDS (hash-keys modules))
        ('BYTECODE (get-bytecode))
        (else
         ;; instance data
         (lambda (msg)
           (define data (hash-ref! modules m (lambda () '())))
           (case msg
             ('ADD (lambda (fid as)
                     (hash-set! modules m (cons (cons fid as) data))))
             ('SHOW data)
             ('NEW (lambda (args) (hash-set! modules m (list (cons "env" args))))))))))))
(define mod/all
  (lambda ()
    (hw/modules 'ALL-IDS)))
(define mod/get/bytecode
  (lambda ()
    (hw/modules 'BYTECODE)))
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

(define (evaluate funId . args)
  " do not use EVAL & APPLY unless you know how to use them for
development purposes. Use this function only to compile hdl
equations."
  (let ((as (list->vector (flatten args))))
    (let ((result
           ((eval funId
                  as
                  (mod/new funId as)
                  0)
            0)))
      (if (eq? result 'stop)
          (begin (cons "There is a cyclic definition in the .hdl files"
                       false))
          (begin (cons (~a "Longest definition: " result)
                       true))))))

(define unit
  (lambda ()
    identity))

(define >>=
  (lambda (ma next)
    "CONTROL  --  checks  for  cyclic  definitions  such  as
     A(..., X, ...)
     X(..., A, ...)."
    (lambda (deep)
      (let ((control (ma deep)))
        (if (eq? 'stop control)
            'stop
            (let ((mb (next)))
              (mb control)))))))

(define new/internal/symbol
  (lambda (fid)
    (lambda (idx)
      (define dbg/syms (lookup/dbg/sym fid))
      (define i/sym (list-ref dbg/syms idx))
      (1+ 'internal/symbols)
      (~a fid "/"
          (counter/get fid)
          "/"
          (counter/get 'internal/symbols) ":"
          (if (pair? i/sym)
              (~a (car i/sym) "." (cdr i/sym))
              i/sym)))))

(define eval
  (lambda (funID env mod deep)
    "like in lambda calculus, EVAL returns the value of an object."
    (1+ funID)
    (cond ((Nand? funID)
           (Nand env mod)
           (unit))
          ((module-already-expanded? funID)
           ;; the modules are memoized -- module was already computed.
           (mod/add mod funID env)
           (unit))
          (else
           (define-values (intern/len sem/bc) (lookup funID))
           (apply sem/bc
                  env
                  (build-vector intern/len
                                (new/internal/symbol funID))
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
      (define io? (lambda (x) (or (positive? x) (zero? x))))
      (define in? (lambda (x) (negative? idx)))
      (cond ((eq? idx 'GND) 'GND)
            ((eq? idx 'VDD) 'VDD)
            ((eq? idx 'CLK) 'CLK)
            ((io? idx) (vector-ref args idx))
            ((in? idx) (vector-ref intern (- (add1 idx))))
            (else (error "!!! invalid index" idx))))
    ((lambda (s) (s s bc))
     (lambda (s bc)
       (cond ((empty? bc)
              (unit))
             (else
              (define call (first bc))
              (def-from-pair (funID params) call)
              (define a (vector-map get/w params))
              (cond ((lisp/instrument? funID)
                     (mod/add module funID a)
                     (s s (rest bc)))
                    (else
                     (>>=
                      (>>=
                       (lambda (x)
                         (if (> x 200)
                             'stop
                             (max deep x)))
                       (lambda ()
                         (eval funID a module (add1 deep))))
                      (lambda ()
                        (s s (rest bc))))))))))))

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
    (define OPEN
      (lambda (name)
        (open-output-file (string-append "./hw/" (symbol->string name) ".hw")
                          #:exists 'truncate)))
    (define CLOSE
      (lambda (file)
        (close-output-port file)))
    (define SAVE
      (lambda (m file)
        (display m file)
        (newline file)))
    (define STDOUT
      p)
    (remove-old-compiled-files)
    (STDOUT "Saving compiled modules: ")
    (map (lambda (mod)
           (define name (first mod))
           (define internal/wires (second mod))
           (define instr (caddr mod))
           (define file (OPEN name))
           (STDOUT name "; ")
           (SAVE name file)
           (SAVE internal/wires file)
           (map
            (lambda (i)
              (define fun (car i))
              (define args (cdr i))
              (SAVE ((lambda (s) (s s args
                               (lambda (x) (string-append (symbol->string fun) " " x))))
                     (lambda (s as out) (if (zero? (vector-length as))
                                       (out "")
                                       (s s
                                          (vector-drop as 1)
                                          (lambda (w)
                                            (out (~a (vector-ref as 0) " " w)))))))
                    file))
            instr)
           (CLOSE file))
         compiled/modules)
    (newline)
    'done))

(define compile
  (lambda (module . wires)
    (cond
     ((hash? global/environment)
      (d "Compiling" module)
      (define result (evaluate module wires))
      (d (car result))
      (and (cdr result)
           (make/hw/files))
      (newline))
     (else
      (d "Cannot continue.")))))

(compile 'Computer 'reset)

(module+ test
  "RAM4K"
  (evaluate 'RAM4K
            'x15 'x14 'x13 'x12 'x11 'x10 'x9 'x8 'x7 'x6 'x5 'x4 'x3 'x2 'x1 'x0
            'load
            'y11 'y10 'y9 'y8 'y7 'y6 'y5 'y4 'y3 'y2 'y1 'y0
            'o15 'o14 'o13 'o12 'o11 'o10 'o9 'o8 'o7 'o6 'o5 'o4 'o3 'o2 'o1 'o0)
  "RAM16K"
  (evaluate 'RAM16K
            'x15 'x14 'x13 'x12 'x11 'x10 'x9 'x8 'x7 'x6 'x5 'x4 'x3 'x2 'x1 'x0
            'load
            'x13 'x12 'x11 'x10 'x9 'x8 'x7 'x6 'x5 'x4 'x3 'x2 'x1 'x0
            'o15 'o14 'o13 'o12 'o11 'o10 'o9 'o8 'o7 'o6 'o5 'o4 'o3 'o2 'o1 'o0)
  "Register"
  (evaluate 'Register
            'x15 'x14 'x13 'x12 'x11 'x10 'x9 'x8 'x7 'x6 'x5 'x4 'x3 'x2 'x1 'x0
            'load
            'o15 'o14 'o13 'o12 'o11 'o10 'o9 'o8 'o7 'o6 'o5 'o4 'o3 'o2 'o1 'o0)
  "RAM512"
  (evaluate 'RAM512
            'x15 'x14 'x13 'x12 'x11 'x10 'x9 'x8 'x7 'x6 'x5 'x4 'x3 'x2 'x1 'x0
            'load
            'x8 'x7 'x6 'x5 'x4 'x3 'x2 'x1 'x0
            'o15 'o14 'o13 'o12 'o11 'o10 'o9 'o8 'o7 'o6 'o5 'o4 'o3 'o2 'o1 'o0)
  "NOT"
  (evaluate 'Not 'in 'out)
  "AND"
  (evaluate 'And 'a 'b 'out)
  "MUX"
  (evaluate 'Mux 'a 'b 'sel 'out)
  "DFF"
  (evaluate 'DFF 'in 'out)
  'done)

