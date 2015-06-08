#lang racket

(require "parser.rkt")
(require "tools.rkt")

;;; DEBUGGING

(define DEBUG/SEMANT false)

(define debug/msg/load-chip
  (lambda (x) (if DEBUG/SEMANT
             (~a "Loading chip " x ".")
             '())))

(define s1/sem/print/body
  (lambda (body)
    (if (empty? body)
        'done
        (let ((next (first body)))
          (let ((i (car next))
                (param/assignment (cdr next)))
            (d (~a "* " i
                   #:pad-string "."
                   #:width 20
                   #:align 'left))
            (for-each (lambda (x)
                        (let ((type/left  (car x))
                              (address    (cadr x)))
                          (p (~a type/left " / "
                                 (~a address
                                     #:pad-string "_"
                                     #:width 2
                                     #:align 'right)
                                 #:pad-string " "
                                 #:width 20
                                 #:align 'right)
                             " <-- ")
                          (cond ((eq? false (cddr x)) (d 'GND))
                                ((eq? true (cddr x)) (d 'VDD))
                                (else (let ((type/right (caddr x))
                                            (value      (cdddr x)))
                                        (d type/right " / " value))))))
                      param/assignment))
          (s1/sem/print/body (rest body))))))

(define s2/print/bytecode
  (lambda (bc)
    (map d bc)))

(define s2/sem/print/body
  (lambda (chip body)
    (ast/print/with/title chip (lambda (_) 'void) 'void)

    (define-values (input output internal boxes)
      (vector->values (vector-map
                       (lambda (sel) (sel body))
                       (vector first second third fourth))))
    
    (define str/wire
      (lambda (x)
        (~a (car x) "." (cdr x) #:width 20)))

    (define r
      (lambda (c n title)
        (d (~a (make-string n c) title))))

    (define (print/header title h)
      (r #\- 20 title)
      (map (lambda (x i)
             (p (~a (~a " ["
                        (~a i #:width 3 #:align 'right #:pad-string "0")
                        "]")
                    " "
                    (str/wire x)))
             (and (= 0 (remainder (add1 i) 3))
                  (newline)))
           h
           (range (length h)))
      (newline))

    (define (print/structs body/struct)
      (if (empty? body/struct)
          (d "---")
          (let ((next (first body/struct)))
            (def-from-pair (function params) next)
            (def-from-pair (i/ports o/ports) params)
            (r #\~ 20 function)

            (define w
              (lambda (l i)
                (if (< i (length l))
                    (~a (list-ref l i) #:width 20)
                    (~a  #:width 20))))

            (map (lambda (idx)
                   (d (~a (~a idx #:width 4 #:align 'right)
                          " |   " (w i/ports idx))
                      (~a " |   " (w o/ports idx))))
                 (range (max (length i/ports)
                             (length o/ports))))

            (print/structs (rest body/struct)))))

    (print/header "INPUT" input)
    (print/header "OUTPUT" output)
    (print/header "INTERNAL" internal)
    (print/structs boxes)))

;;; Stage 2 ---

(define s2/bytecode-generator
  (lambda (body c co)
    ;; (define input (first body))
    ;; (define output (second body))
    ;; (define instruments (third body))
    ;; (define intern (fourth body))
    ;; (define boxes (fifth body))

    (define-values (input output instruments intern boxes)
      (vector->values (vector-map
                       (lambda (sel) (sel body))
                       (vector first second third fourth fifth))))
    
    (define io/header (append input output))

    (define idx
      (lambda (l e)
        (define (iter l n)
          (cond ((null? l) false)
                ((equal? e (car l)) n)
                (else (iter (cdr l) (add1 n)))))
        (iter l 0)))

    (define idx/io/header
      (lambda (e)
        (idx io/header e)))

    (define idx/intern
      (lambda (e)
        (- -1 (idx intern e))))

    (define get-pointer
      (lambda (id)
        (cond ((eq? id 'GND) 'GND)
              ((eq? id 'VDD) 'VDD)
              ((eq? id 'CLK) 'CLK)
              ((idx/io/header id))
              ((idx/intern id))
              (else (error "invalid ID")))))

    (define update/structure
      (lambda (params co)
        ((lambda (s) (s s params
                        (lambda (pointers)
                          (co pointers))))
         (lambda (s params co)
           (if (empty? params)
               (co '())
               (s s (rest params)
                  (lambda (pointers)
                    (co (cons (get-pointer (first params))
                              pointers)))))))))
    ((lambda (s i)
       (s s boxes
          (lambda (bc/s)
            (i i instruments bc/s
               (lambda (bc/i)
                 (co (cons (length intern)
                           bc/i)))))))
     (lambda (s structures co)
       (if (empty? structures)
           (co '())
           (s s
              (rest structures)
              (lambda (struct0)
                (def-from-pair (function params) (first structures))
                (def-from-pair (input output) params)
                (update/structure (append input output)
                                  (lambda (struct)
                                    (co (cons (cons function (list->vector struct))
                                              struct0))))))))
     (lambda (s instruments bc/structs co)
       (if (empty? instruments)
           (co bc/structs)
           (s s
              (rest instruments)
              bc/structs
              (lambda (instr0)
                (define instrument (first instruments))
                (def-from-pair (function args) instrument)
                (update/structure args
                                  (lambda (instr)
                                    (co (cons (cons function (list->vector instr))
                                              instr0)))))))))))

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
                  (rest els)
                  (lambda (e)
                    (if (member (first els) out)
                        (co (first els))
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
                  (rest els)
                  (lambda (sigma)
                    (if (is/0/1? (first els))
                        (co sigma)
                        (co (cons (cons (first els) generator)
                                  sigma))))))))))

    (define update/signature/structure
      (lambda (o/ports sigma)
        ((lambda (s) (s s o/ports (lambda (x) x)))
         (lambda (s groups co)
           (if (empty? groups)
               (co sigma)
               (s s
                  (rest groups)
                  (lambda (sigma)
                    (co
                     (update/signature/group (first groups)
                                             (output/group/generator (first groups))
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
              (rest structures)
              (lambda (sig)
                (define params (cdr (first structures)))
                (define o/ports (cadr params))
                (co
                 (update/signature/structure o/ports sig)))))))))

(define s2/apply/signature
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
              (rest structures)
              (lambda (up/struct0 iw0)
                (define next (first structures))
                (def-from-pair (function params) next)
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
              (rest instruments)
              (lambda (up/instrum0)
                (define instrument (first instruments))
                (def-from-pair (function args) instrument)
                (co (cons (cons function (map S args))
                          up/instrum0)))))))))

;;; Stage 1 ---

(define is/O?
  (lambda (w)
    (eq? 'O w)))
(define is/I?
  (lambda (w)
    (eq? 'I w)))
(define is/0/1?
  (lambda (w)
    (pair? (memq w '(GND VDD CLK)))))

(define s1/expand/subbus/pins
  (lambda (sym i j success failure)
    (if (> i j)
        ;; this error will never produce, as this check was moved in
        ;; the lexer. However, we keep its collector for a possible
        ;; future use.
        (failure "Subbus index error")
        (success
         (map (lambda (x) (cons sym x))
              (range i (add1 j)))))))

(define s1/preprocess/io/header
  (lambda (pins next)
    (if (empty? pins)
        (next '())
        (s1/preprocess/io/header
         (rest pins)
         (lambda (v0)
           (let* ((bus (first pins)))
             (s1/expand/subbus/pins
              (first bus)
              0
              (sub1 (second bus))
              (lambda (v) (next (append v v0)))
              (lambda (e) (error 'io "never can occur" e)))))))))

(define s1/load/chip
  (lambda (file col)
    (define s1/check/filename/chipname
      (lambda (file name)
        (if (equal? (path->string file) (~a name ".hdl"))
            '()
            (W! "file/chip mismatch" file "vs." name "."))))
    (define s1/split/body/instrument/gates
      (lambda (body co)
        (cond ((empty? body)
               (co '() '()))
              ((eq? 'i (caar (first body)))
               (s1/split/body/instrument/gates
                (rest body)
                (lambda (s i)
                  (co s (cons (first body) i)))))
              (else
               (s1/split/body/instrument/gates
                (rest body)
                (lambda (s i)
                  (co (cons (first body) s) i)))))))

    (let* ((ast           (get/ast (~a "./hdl/" file)))
           (name          (car (first ast)))
           (ast/input     (second ast))
           (ast/output    (third ast))
           (ast/body      (drop ast 3)))
      ;; (d ast/clocked)
      ;; (d ast)
      ;; (newline)
      ;; (ast/print/with/title name (lambda (_) 'void) 'void)
      ;; (ast/print/io ast/input)
      ;; (ast/print/body ast/body)
      (s1/preprocess/io/header          ; >> INPUT HEADER
       ast/input
       (lambda (input)                       ; << INPUT
         (s1/preprocess/io/header       ; >> OUTPUT HEADER
          ast/output
          (lambda (output)                   ; <<OUTPUT
            (let ((w/filename/chipname
                   (s1/check/filename/chipname file name))
                  (m/loading
                   (debug/msg/load-chip name)))
              (s1/split/body/instrument/gates
               ast/body
               (lambda (body/structures body/instruments)
                 (col name
                      input
                      output
                      body/structures
                      body/instruments
                      (list w/filename/chipname)
                      '()
                      (list m/loading))))))))))))

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
                               (fail (~a "Unknown left value "
                                         ID))
                               (co 'O o))))
                      (co 'I i))))))
         (lambda (s in/pin index co)
           "SYM is a SYMBOL"
           (cond
            ;; NOT FOUND
            ((empty? in/pin)
             (co '()))
            ;; FOUND
            ((eq? ID (car (first in/pin)))
             (s s
                (rest in/pin)
                (add1 index)
                (lambda (idx)
                  (co (cons index idx)))))
            ;; LOOP AND COLLECT
            (else
             (s s
                (rest in/pin)
                (add1 index)
                (lambda (idx)
                  (co idx))))))
         (lambda (s in/pin index co)
           "SYM IS A PAIR"
           (cond
            ;; NOT FOUND
            ((empty? in/pin)
             (co '()))
            ;; FOUND
            ((equal? ID (first in/pin))
             (co (list index)))
            ;; TRY NEXT
            (else
             (s s
                (rest in/pin)
                (add1 index)
                (lambda (idx)
                  (co idx)))))))))

    (case (ast/sym/type sym)
      ;; Collect all the pins named `name` from Inputs or Output of
      ;; the structure being analysed.  If none is found in inputs or
      ;; outputs, then returns 'Undefined. If sym is a symbol, collect
      ;; a set of pins whose name is SYM-NAME. Otherwise look only for
      ;; 1 pin in in/out that has the same name and index as SYM.
      ('A (get/addr (ast/sym/name sym)
                    (lambda (p/t a)
                      (succ p/t a '() '() '()))))
      ('S (s1/expand/subbus/pins
           (ast/sym/name sym)
           (ast/sym/p1 sym)
           (ast/sym/p2 sym)
           (lambda (x)
             ((lambda (s) (s s x
                        (lambda (addr m e w)
                          (let ((type (list->set (map car addr)))
                                (addr (map cadr addr)))
                            (if (= 1 (set-count type))
                                (succ (set-first type)
                                      addr m e w)
                                (error "never here"))))))
              (lambda (s l co) (if (empty? l)
                                   (co '()'()'()'())
                                   (s s
                                      (rest l)
                                      (lambda (address/list m e w)
                                        (get/addr
                                         (first l)
                                         (lambda (p/t a)
                                           (co
                                            (cons (cons p/t a)
                                                  address/list)
                                            m e w)))))))))
           fail)))))

(define s1/resolve/right/value
  ;; If we have a symbol of type Any/GND/VDD/CLK in the right side of an
  ;; assignment, it can be either a single wire, or it can be a
  ;; bus. The bus width is decided by the width of the left side, that
  ;; at this point is already known.
  (lambda (right/value port/type length/left/value succ fail)
    (define stype (ast/sym/type right/value))
    (define sname (ast/sym/name right/value))
    (case stype
      ((GND VDD CLK) (succ (make-list length/left/value stype)
                           '() '() '()))
      ('A (s1/expand/subbus/pins
           (ast/sym/name right/value)
           0
           (sub1 length/left/value)
           (lambda (v) (succ v '() '() '()))
           fail))
      ('S (if (= length/left/value
                 (add1 (- (ast/sym/p2 right/value)
                          (ast/sym/p1 right/value))))
              (s1/expand/subbus/pins
               (ast/sym/name right/value)
               (ast/sym/p1 right/value)
               (ast/sym/p2 right/value)
               (lambda (v) (succ v '() '() '()))
               fail)
              (fail (~a
                     "Bus width does not match "
                     " -- " port/type
                     " -- " length/left/value
                     " -- " right/value
                     ".")))))))

(define s1/preprocess/assignment/list
  (lambda (header/in header/out environment undefined assignment/list IN OUT co)
    "Expands the assignments from <FUNCTION ASSIGNMENT-PARAMETERS ...>
in the form:

   FUN/HEADER/INPUT X1 X2 ... Xn, Xi being a symbol defined as
right/value of the assignments from the parameter list of FUN.

   FUN/HEADER/OUTPUT [Y1.1 Y1.2], [Y2.1 Y2.2...], ... [Yn...], Yi
being a symbol defined as right/value of the assignments from the
parameter list of FUN.

           The internals of a structure (box).
          +--------------------------------------------------+
          |    BOX (STRUCTURE) Internals                     |
          |                                                  |
    IN0--->                                                  ---> OUT0
          |    <---------------------+                       |
          |                          |                       |
    IN1--->        +----------  ENVIRONMENT  ----------+     ---> OUT1
          |        |                                   |     |
          |        V  +-----++---------------++-----+  V     |
    IN2--->           |     ||               ||     |        ---> OUT2
          |       LV1 ~ RV1 ||               || RVx ~ LVx    |
          |       LV2 ~ RV2 ||               || RVy ~ LVy    |
    ...   |       LV3 ~ RV3 ||               || RVz ~ LVz    |    ...
          |           | ... ||               || ... |        |
          |           |     ||               ||     |        |
          |           |     ||   SOME        || O   |        |
          |           | I   ||   INTERNAL    || U   |        |
          |           | N P ||   STRUCTURE   || T P |        |
          |           | P O ||   (BLACK BOX) || P O |        |
          |           | U R ||               || U R |        |
          |           | T T ||               || T T |        |
          |           |   S ||               ||   S |        |
          |           |     ||               ||     |        |
          |           +-----++---------------++-----+        |
          |                                                  |
          +--------------------------------------------------+

"
    (define input/vector (make-vector (length header/in) false))
    (define output/vector (make-vector (length header/out) '()))
    ((lambda (s) (s s
               assignment/list
               (lambda (updated/env updated/undef m e w)
                 (co updated/env
                     updated/undef
                     (vector->list input/vector)
                     (vector->list output/vector)
                     m e w))))
     (lambda (s params co)
       (if (empty? params)
           (co environment undefined '() '() '())
           (s s
              (rest params)
              (lambda (env undef m0 e0 w0)
                (define assignment (first params))
                (define left/value (ast/leftvalue assignment))
                (define right/value (ast/rightvalue assignment))
                ;; ---------------------------------------- >> LEFT VALUE
                (s1/resolve/left/value
                 left/value
                 header/in
                 header/out
                 ;; ---------------------------------------- << ADDRESS
                 (lambda (port/type address ml el wl)
                   (define LEFT/BUS/WIDTH (length address))
                   ;; -------------------------------------- >> RIGHT VALUE
                   (s1/resolve/right/value
                    right/value
                    port/type
                    LEFT/BUS/WIDTH
                    ;; ------------------------------------- << VALUE
                    (lambda (value mr er wr)
                      ((lambda (s) (s s address value
                                 (lambda (up/env up/undef assign/errors)
                                   ;; we collect only the errors,
                                   ;; there are no messages/warnings
                                   ;; here as yet, but we keep the
                                   ;; collectors for a future use.
                                   (co up/env
                                       up/undef
                                       (list ml mr m0)
                                       (list assign/errors el mr e0)
                                       (list wl mr w0)))))
                       (lambda (s addr/list val/list co)
                         "loop over addr/value pairs and populate the
                          vectors that represent the input and output
                          ports of FUNCTION.

                          Each input port can be connected to a single
                          wire, while output ports can have multiple
                          wires.  For output ports, only 1 such wire
                          will be considered to generate the signal
                          for the group of wires connected to that
                          output port.  The other wires are considered
                          aliases for that output port.  The unification
                          will replace all the aliases with the generator
                          of each group."
                         (cond
                          ((empty? addr/list)
                           (co env undef '()))
                          ((and (is/I? port/type)
                                (vector-ref input/vector (first addr/list)))
                           (s s
                              (rest addr/list)
                              (rest val/list)
                              (lambda (env undef err)
                                (co env
                                    undef
                                    (cons
                                     (~a "Cannot input multiple wires in the same pin"
                                         " -- " (first addr/list)
                                         " -- " (list-ref header/in (first addr/list))
                                         ".")
                                     err)))))
                          ((is/I? port/type)
                           (s s
                              (rest addr/list)
                              (rest val/list)
                              (lambda (env undef err)
                                (define a (first addr/list))
                                (define v (first val/list))
                                ;; SET UP THE `A` ENTRY IN INPUT PORTS
                                (vector-set! input/vector a v)
                                ;; add V to unknownks if v is not in environment.
                                (define unknowns
                                  (if (member v env)
                                      undef
                                      (cons v undef)))
                                (co env unknowns err))))
                          ((and (is/O? port/type)
                                (not (is/0/1? (first val/list)))
                                (pair? (member (first val/list) env)))
                           (s s
                              (rest addr/list)
                              (rest val/list)
                              (lambda (env undef err)
                                (co env
                                    undef
                                    (cons (~a "The wire " (first val/list)
                                              " was defined multiple times.")
                                          err)))))
                          ((is/O? port/type)
                           (s s
                              (rest addr/list)
                              (rest val/list)
                              (lambda (env undef err)
                                (define a (first addr/list))
                                (define v (first val/list))
                                (define prev/val (vector-ref output/vector a))
                                ;; SET UP THE `A` ENTRY in OUTPUT PORTS
                                (vector-set! output/vector a (cons v prev/val))
                                ;; add V to environment and remove V from unknowns
                                (co (cons v env)
                                    (remove* (list v) undef)
                                    err))))))))
                    (lambda (err)
                      ;; in case left value is not valid, we go on
                      ;; collecting more errors, but we are no more
                      ;; interested about the value of i/o.
                      (co env
                          undef
                          (list ml m0)
                          (list err el e0)
                          (list wl w0)))))
                 (lambda (err)
                   (co env
                       undef
                       m0
                       (cons err e0)
                       w0))))))))))

(define s1/preprocess/lisp/instruments
  (lambda (instruments env succ fail)
    (define p (q 'ok (or (empty? instruments) 'ok)))
    ;; (map (lambda (x) (p ".." x)) instruments)

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
              (define instrument (first instr))
              (def-from-pair (lisp-function arguments) instrument)
              (define lisp-fun-id (second lisp-function))
              (s s
                 (rest instr)
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
                             (rest args)
                             (lambda (addr0 failed/pins)
                               (define arg (first args))
                               (s1/resolve/left/value
                                arg
                                env
                                '()
                                (lambda (_ address ml el wl)
                                  (define subbus (map (lambda (i) (list-ref env i))
                                                      address))
                                  (co (cons subbus addr0)
                                      failed/pins))
                                (lambda (_)
                                  (co addr0
                                      (cons arg failed/pins)))))))))))))))))

(define s1/preprocess/body/unification
  (lambda (c IN OUT BOXES INSTRUMENTS in/headers out/headers co)
    (define initial-environment (append IN '(VDD GND CLK)))

    ;;(ast/print/with/title c (lambda (_) 'void) 'void)

    ((lambda (s) (s s BOXES
               (lambda (env undefined/input/wires i/gen-o m e w)
                 (if (empty? undefined/input/wires)
                     ;; We can preprocess the instruments only after
                     ;; we know the whole environment local to a gate.
                     (s1/preprocess/lisp/instruments
                      INSTRUMENTS
                      (set->list
                       (list->set
                        (append
                         ;;* avoid CLK GND VDD *
                         (filter pair? env)
                         ;;* for lisp instruments, allow access to
                         ;;  non-initialized output wires. we take
                         ;;  care not to duplicate the outputs *
                         OUT)))
                      (lambda (instruments)
                        ;; The i/o ports of each structure are
                        ;; processed after we finished to pre-process
                        ;; all the internal structures
                        (s2/unify i/gen-o                       ; >> UNIFICATION
                                  OUT
                                  (lambda (sigma)                    ; << SIGNATURE
                                    (s2/apply/signature         ; >> APPLY SIGNATURE
                                     sigma
                                     i/gen-o
                                     instruments
                                     OUT
                                     (lambda (body/structs            ; << BODY + INSTR
                                         instrum
                                         out
                                         internal/wires)
                                       (co (list IN
                                                 out
                                                 instrum
                                                 (set->list internal/wires)
                                                 body/structs)
                                           m e w))))))
                      (lambda (undefined/instrument/argument)
                        (co '()
                            m
                            (cons (map (lambda (x) (~a "" x))
                                       undefined/instrument/argument)
                                  e)
                            w)))
                     (co '()
                         m
                         (cons (map (lambda (x)
                                      (~a "Undefined input wires " x))
                                    undefined/input/wires)
                               e)
                         w)))))
     (lambda (s structure co)
       (if (empty? structure)
           (co initial-environment
               '() '() '() '() '())
           (s s
              (rest structure)
              (lambda (env undef i/o0 m0 e0 w0)
                (define next (first structure))
                (def-from-pair (function params) next)
                (define fun/name (car function))
                (if (pair? (assoc fun/name in/headers))
                    (s1/preprocess/assignment/list
                     (cdr (assoc fun/name in/headers))
                     (cdr (assoc fun/name out/headers))
                     env
                     undef
                     params
                     IN OUT
                     (lambda (up/env undef i o m e w)
                       (co up/env
                           undef
                           (cons (list fun/name i o) i/o0)
                           (list m m0)
                           (list e e0)
                           (list w w0))))
                    (co env
                        undef
                        m0
                        '()
                        (cons (~a "Cannot find the gate definition for "
                                  fun/name)
                              e0)
                        w0)))))))))

(define s1/initialize/gates
  (lambda (success fatal)
    (define hdl/file? (lambda (f) (and (equal? #"hdl" (filename-extension f)) f)))
    (define hdl/files (filter hdl/file? (directory-list "./hdl/")))
    ((lambda (s)
       (s s
          hdl/files '()'()'()'()'()'()'()'()
          (lambda (IDs in out body instrument m e w)
            ;; after all the inputs and outputs were preprocessed, we
            ;; start preprocessing the body. During body preprocessing
            ;; we need to know the i/o for all the gates.
            ((lambda (s) (s s
                       IDs
                       '()'()
                       m e w
                       (lambda (structures bytecode m e w)
                         (define fatal/errors (flatten e))
                         (define warn (flatten w))
                         (define logs (flatten m))
                         (if (empty? fatal/errors)
                             (success structures bytecode logs warn)
                             (fatal logs fatal/errors warn)))))
             (lambda (s chip/list structures bytecode m e w co)
               (if (empty? chip/list)
                   (co structures bytecode m e w)
                   (let* ((chip (first chip/list)))
                     (s1/preprocess/body/unification ; >> Symbolic representation
                      chip
                      (cdr (assoc chip in))
                      (cdr (assoc chip out))
                      (cdr (assoc chip body))
                      (cdr (assoc chip instrument))
                      in out
                      (lambda (body/structures m~ e~ w~)
                        (if (empty? (flatten e~))
                            (s2/bytecode-generator   ; >> Bytecode representation
                             body/structures
                             chip
                             (lambda (bc)
                               (s s
                                  (rest chip/list)
                                  (cons (cons chip body/structures) structures)
                                  (cons (cons chip bc) bytecode)
                                  (list m~ m)
                                  (list e~ e)
                                  (list w~ w)
                                  co)))
                            (s s
                               (rest chip/list)
                               '()
                               '()
                               m~
                               e~
                               w~
                               co)))))))))))
     (lambda (s files IDs in0 out0 body0 instrument0 m e w co)
       (if (empty? files)
           (co IDs in0 out0 body0 instrument0 m e w)
           (s1/load/chip (first files)
                         (lambda (id in out structs instruments w/io e/io m/io)
                           (s s
                              (rest files)
                              (cons id IDs)
                              (cons (cons id in) in0)
                              (cons (cons id out) out0)
                              (cons (cons id structs) body0)
                              (cons (cons id instruments) instrument0)
                              (list m/io m)
                              (list e/io e)
                              (list w/io w)
                              co))))))))

(define bytecode
  (s1/initialize/gates
   (lambda (structures bytecode m w)
     (map (lambda (x) (d "MM" x)) (flatten m))
     (map (lambda (x) (d "WW" x)) (flatten w))
     (p "Loaded ")
     (for-each (lambda (x) (p x ", ")) (map car structures))
     (p "total " (length structures) " gates defined.\n")
     (cons structures bytecode))
   (lambda (m e w)
     (map (lambda (x) (d "MM" x)) (flatten m))
     (map (lambda (x) (d "WW" x)) (flatten w))
     (map (lambda (x) (d ">>" x)) (flatten e))
     (d (length e) "errors found.")
     'fail)))

(module+ test
  (define (test/ )
    (map (lambda (x) (s2/sem/print/body
                 x
                 (cdr (assoc x (car bytecode)))))
         (map car (car bytecode)))
    (void))
  (define (test/bc)
    (map (lambda (x)
           (d "++++++++++++++++" x)
           (s2/print/bytecode
            (cdr (assoc x (cdr bytecode)))))
         (map car (car bytecode)))
    (void))
  "--- TEST"
  '(test/)
  (test/bc)
  'done)

(provide (rename-out
          (bytecode sem/bytecode)))
