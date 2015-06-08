#lang racket

(require "tools.rkt")
(require data/queue)

(define environment
  (let ((syms (make-hash)))

    (define load/module
      (lambda (name)
        (define data (file->lines (~a "./hw/" name)
                                  #:mode 'text))
        (define gate/name (string->symbol (list-ref data 0)))
        (define i/w (string->number (list-ref data 1)))
        (define code
          (map (lambda (instr/str)
                 (define instr (string-split instr/str))
                 (define function (car instr))
                 (define args (list->vector (cdr instr)))
                 (cons (string->symbol function)
                       (vector-map (lambda (idx)
                                     (case (string->symbol idx)
                                       ('GND 'GND)
                                       ('VDD 'VDD)
                                       ('CLK 'CLK)
                                       (else (string->number idx))))
                                   args)))
               (drop data 2)))
        (list gate/name i/w code)))

    (define hw/file? (lambda (f) (and (equal? #"hw" (filename-extension f)) f)))
    (define hw/files (filter hw/file? (directory-list "./hw/")))

    (set! syms (make-hash (map load/module hw/files)))
    (lambda (id)
      (define def (hash-ref syms id))
      (values (car def) (cadr def)))))

;;; WIRES

(define new/wire
  (lambda ()
    (define signal false)
    (define todo-list '())
    ;; count all wires
    (1+ 'wires)
    ;; permissions definition
    (define get
      (lambda () signal))
    (define set
      (lambda (new/val)
        (or (boolean=? new/val signal)
            (begin
              (set! signal new/val)
              (wire/alert/propagators todo-list)))))
    (define add
      (lambda (new/propagator/action)
        (set! todo-list (cons new/propagator/action todo-list))
        (new/propagator/action)))
    ;; wire permissions
    (lambda (p)
      (p get
         set
         add))))
(define wire/alert/propagators
  (lambda (todo)
    "when a wire changes its signal, it will call all the actions from
     todo-list. Some of these actions, installed by propagators, will
     alert more wires about a change in their signals."
    (cond ((empty? todo) 'ok)
          (else ((first todo))
                (wire/alert/propagators (rest todo))))))
(define wire/get/signal
  (lambda (wire)
    (wire (lambda (g _ __) (g)))))
(define wire/set/signal!
  (lambda (wire new/val)
    (or (memq new/val '(#t #f))
        (error "!! signal can be only 1 or 0." new/val))
    (wire (lambda (_ s __) (s new/val)))))
(define wire/add/todo/action
  (lambda (wire propagator/action)
    "
A propagator installs an action on a wire.

The action will be executed when the signal on the given wire changes
its value.

When the action is executed, it will add to the scheduler a function
that will be executed after a given delay by the scheduler.

When the scheduler executes an action, this action will set the value
of the signal on some wire."
    (wire (lambda (_ __ a) (a propagator/action)))))
(define wire/new/scheduled/action
  (lambda (output/wire new-value)
    "creates an action that will be added in the scheduler. This
action, when executed by the scheduler, will set the value of a signal
on some output wire of some propagator."
    (lambda ()
      (wire/set/signal! output/wire new-value))))

(define VDD
  (lambda (p)
    (p (lambda () true)
       (lambda (_) "!!! cannot set the source wire signal")
       (lambda (p) (p)))))
(define GND
  (lambda (p)
    (p (lambda () false)
       (lambda (_) "!!! cannot set the ground wire signal")
       (lambda (p) (p)))))

(define CLK
  (let ((todo-list '())
        (signal false))
    ;; permissions
    (define get
      (lambda () signal))
    (define set
      (lambda (nv)
        (case nv
          ('high (set! signal true)
                 (wire/alert/propagators todo-list))
          ('low (set! signal false)
                (wire/alert/propagators todo-list))
          (else
           (error "cannot set system clock. use tick and tock instead.")))))
    (define str
      (lambda ()
        (~a "W:'System-Clock =" (if signal 1 0) ";" (length todo-list))))
    (define add
      (lambda (new/propagator/action)
        (set! todo-list (cons new/propagator/action todo-list))
        (new/propagator/action)))
    ;; wire
    (define this
      (lambda (p)
        (p get
           set
           add)))
    this))
(define tick
  (lambda () (CLK (lambda (_ s __) (s  'high)))))
(define tock
  (lambda () (CLK (lambda (_ s __) (s  'low)))))

;;; SCHEDULER

(define propagation/combinatorial/max/time
;;; must be greater than the maximum of the propagation times of all
;;; the combinatorial circuits.
  
;;; we use more segments, each one having a queue, such that we may
;;; detect whether the circuit has infinite loops. It is nicer
;;; detecting the infinite loops using propagation time instead of
;;; checking for a maximum size of a queue of scheduled events.
  (* 10 20))
(define scheduler
;;;
;;;  1. PROPAGATOR installs an action on wire A and B.
;;;
;;;  2. when wire A or B changes its value, it executes its
;;;     actions. The actions installed by some propagator will send
;;;     tasks to the scheduler.
;;;
;;;  3. the scheduler executes its tasks inserted by wires in
;;;     queue-order in each time segment.  Each task will set the
;;;     signal of the output wire of the corresponding propagator.  A
;;;     time segment is executed in parallel in a real circuit.
;;;
;;;
;;;
;;;                                    o-----------o
;;;  +--------------+                 /             \
;;; -+              |     A          / action        o
;;;  |  propagator  +--o-------+    / on wire A      |                     ---
;;; -+              |  |       |   /                 |                   -/
;;;  +--------------+  |       |  o                  |                  /
;;;                    |       |            +--------+-----+           /
;;;                    |       |            |              |          /     C
;;;                    |       +------------>  THE NAND    |  out     |     L
;;;                    |                    |  PROPAGATOR  +--------->|     O
;;;                    |       +------------>              |   ^      |     U
;;;   +--------------+ |       |            |              |   |      \     D
;;;  -+              | |   B   |            +--------+-----+   |       \
;;;  -+  propagator  +-|---o---+                     |         |        \
;;;  -+              | |   |     o                   |         |         -\
;;;   +--------------+ |   |      \                  |         |           ---
;;;                    |   |       \ action          |         |
;;;                    |   |        \ on wire B      o         |
;;;                    |   |         \              /          |
;;;                    |   |          o------------o           |
;;;                    |   |                                   |
;;;                    |   |    +------------------+           |
;;;                    +-> +--> |    SCHEDULER     |-->--------+
;;;                             +------------------+
;;;

  (let ((time-segments (build-vector propagation/combinatorial/max/time
                                     (lambda (_) (make-queue))))
        (current-time 0)
        (pointer 0)
        (count 0))
    (define Q (lambda (time)
                (if (>= time propagation/combinatorial/max/time)
                    (error "The hardware circuit has some loop not ")
                    (vector-ref time-segments time))))
    (define R (lambda (time)   (dequeue! (Q time))))
    (define A (lambda (time e) (enqueue! (Q time) e)))
    (define look-for-next-time-segment
      (lambda ()
        (cond ((non-empty-queue? (Q pointer)) 'ok)
              (else
               (set! current-time (add1 current-time))
               (set! pointer (add1 pointer))
               (look-for-next-time-segment)))))
    (define add
      (lambda (delay action)
        (set! count (add1 count))
        (A (+ delay pointer) action)))
    (define get-current-time
      (lambda ()
        current-time))
    (define get-pointer
      (lambda ()
        pointer))
    (define get-number-of-tasks
      (lambda ()
        count))
    (define get-first-item
      (lambda ()
        (set! count (sub1 count))
        (cond ((negative? count)
               (error "!! there is no propagator waiting in scheduler."))
              (else
               (look-for-next-time-segment)
               (R pointer)))))
    (define reset
      (lambda ()
        (set! pointer 0)))
    (define empty?
      (lambda ()
        (= count 0)))
    (lambda (p)
      (p add
         get-current-time
         get-pointer
         get-first-item
         empty?
         get-number-of-tasks
         reset))))
(define scheduler/add
  (lambda (time propagator)
    (scheduler (lambda (add $2 $3 $4 $5 $6 $7) (add time propagator)))))
(define scheduler/current/time
  (lambda ()
    (scheduler (lambda ($1 ct $3 $4 $5 $6 $7) (ct)))))
(define scheduler/pointer
  (lambda ()
    (scheduler (lambda ($1 $2 p $4 $5 $6 $7) (p)))))
(define scheduler/first/item
  (lambda ()
    (scheduler (lambda ($1 $2 $3 fi $5 $6 $7) (fi)))))
(define scheduler/empty?
  (lambda ()
    (scheduler (lambda ($1 $2 $3 $4 e? $6 $7) (e?)))))
(define scheduler/tasks/count
  (lambda ()
    (scheduler (lambda ($1 $2 $3 $4 $5 tc $7) (tc)))))
(define scheduler/reset
  (lambda ()
    (scheduler (lambda ($1 $2 $3 $4 $5 $6 r) (r)))))
(define scheduler/do
  (lambda ()
    (if (scheduler/empty?)
        (scheduler/reset)
        (let ((item (scheduler/first/item)))
          (item)
          (scheduler/do)))))

;;; PROPAGATORS FOR LOGIC GATES
(define propagation/time/nand 1)
(define logic/nand
  (lambda (a b)
    (nand (wire/get/signal a)
          (wire/get/signal b))))

(define Nand
  (lambda (io)
    "A propagator that unidirectionally computes the logical NAND from
the inputs A and B toward the output OUT.

     The NAND propagator will install the nand/action on the wires a
and b, which will schedule a thunk after propagation/time/nand, when
the signal a or b changes.

    The scheduler will execute an action that will set the value of
the output port, which is equal to the logical nand of the signal
value of a and b."

    (or (= 3 (vector-length io))
        (error "!!! wrong NAND call."))

    (define a   (vector-ref io 0))
    (define b   (vector-ref io 1))
    (define out (vector-ref io 2))

    (define nand/propagator/action
      (lambda ()
        (let ((new/val (logic/nand a b)))
          (scheduler/add propagation/time/nand
                         (wire/new/scheduled/action out new/val)))))
    (wire/add/todo/action a nand/propagator/action)
    (wire/add/todo/action b nand/propagator/action)
    'ok))

;;; PROPAGATOR FOR READ-ONLY MEMORY
(define propagation/time/rom 1)
(define rom
  (lambda (args)
    (define data binary/file)
    (d "ROM installed. The loaded program has a length of "
       (vector-length data)
       "instructions.")
    (define-values (bus-addr bus-out) (vector-split-at args 15))
    
    (define rom/propagator
      (lambda ()
        (let ((addr (bus->dec bus-addr)))
          (and (< addr (vector-length data))
               (scheduler/add propagation/time/rom
                              (lambda ()
                                (bus/set! bus-out (vector-ref data addr))))))))
    (vector-map (lambda (w) (wire/add/todo/action w rom/propagator))
                bus-addr)
    'done))

;;; PROPAGATORS FOR SEQUELTIAL PARTS
(define propagation/time/ram 1)

(counter/reset 'ram.8 0)

(define ram8
  (lambda (env)
    "Any sequential structure must simulate the behavior of a Bit, and
in the same time it must behave as a combinatorial component."
    (define segment (counter/get 'ram.8))
    (1+ 'ram.8)

    (define in   (vector-copy env 0 16))
    (define lo@d (vector-ref env 16))
    (define addr (vector-copy env 17 20))
    (define out  (vector-copy env 20 36))
    
    (define memory (make-vector 8 0))
    
    (define get/load (lambda ()    (wire/get/signal lo@d)))
    (define get/clk  (lambda ()    (wire/get/signal CLK)))
    (define get/val  (lambda (bus) (bus->dec bus)))
    
    (define output/data
      (lambda ()
        (define address (get/val addr))
        (scheduler/add
         propagation/time/ram
         (lambda ()
           (bus/set! out
                     (rev-vector
                      (dec->boolvec (vector-ref memory address) 16)))))))

    (define ram8/addr
      (lambda ()
        (output/data)))
    (define ram8/clock
      (lambda ()
        (define l (get/load))
        (define clk (get/clk))
        (define i (get/val in))
        (define a (get/val addr))

        (if clk
            (begin
              (and l
                   (vector-set! memory a i)))
            (begin
              (output/data)))))
    
    (wire/add/todo/action CLK ram8/clock)
    (vector-map (lambda (x) (wire/add/todo/action x ram8/addr)) addr)))

;;; INSTRUMENTS

(define set-high
  (lambda args
    (vector-map
     (lambda (w)
       (wire/set/signal! w true))
     (car args))))

(define Keyboard-send-code (lambda () 'not-installed))
(define screen
  (lambda (args)
    (d "Screen activated.")
    ;; (define bus-s (make/bus args))
    ;; (bus/dec/probe "S: " bus-s)
    ;; (bus/bin/probe "S: " bus-s)
    'done))
(define keyboard
  (lambda (args)
    ;; (define bus-key (make/bus args))
    (d "Keyboard activated. Use Keyboard-send-code(ascii-code) to send \
a key with a given ASCII code to the little computer.")
    (set! Keyboard-send-code
          (lambda (ascii-code)
            (bus/set! args (dec->bin 16 ascii-code))))))

;;; The Hack program from this file will be executed
(define binary/file
  (let ((in/file (vector-ref (current-command-line-arguments) 0)))
    (list->vector
     (map
      (lambda (line)
        (list->vector
         (map
          (lambda (x) (if (zero? x) false true))
          (take (drop (map string->number
                           (string-split line "")) 1) 16))))
      (file->lines in/file)))))

(define @PC 0)
(define @A 0)
(define @D 0)
(define @Mo 0)
(define @Ma 0)
(define @Mw 0)

(define reg/pc
  (lambda (bus-pc)
    (d "REG PC ok.")
    (define set
      (lambda ()
        (set! @PC (bus->dec bus-pc))))
    (vector-map (lambda (w) (wire/add/todo/action w set))
                bus-pc)))
(define reg/d
  (lambda (bus-d)
    (d "REG D ok")
    (define set
      (lambda ()
        (set! @D (bus->bin bus-d))))
    (vector-map (lambda (w) (wire/add/todo/action w set))
                bus-d)))
(define reg/a
  (lambda (bus-a)
    (d "REG A ok.")
    (define set
      (lambda ()
        (set! @A (bus->bin bus-a))))
    (vector-map (lambda (w) (wire/add/todo/action w set))
                bus-a)))
(define reg/m
  (lambda (bus-m)
    (d "REG M ok.")
    (define bus-writeM (vector-copy bus-m 0 1))
    (define bus-outM   (vector-copy bus-m 1 17))
    (define bus-addrM  (vector-copy bus-m 17 32))

    (define set
      (lambda ()
        (set! @Mo (bus->dec bus-outM))
        (set! @Ma (bus->dec bus-addrM))
        (set! @Mw (bus->dec bus-writeM))))
    (vector-map (lambda (w) (wire/add/todo/action w set))
                bus-m)))

(define lisp/instrument?
  (lambda (sym)
    (char-lower-case? (string-ref (symbol->string sym) 0))))

;;; HARDWARE BUILDER

(define (eval id env)
  (1+ id)

  (case id
    ('Nand      (Nand env))
    ('RAM8      (ram8 env))
    (else
     (define-values (intern/len bc) (environment id))
     (define intern (build-vector intern/len (lambda (_) (new/wire))))
     (apply bc (vector-append env intern)))))

(define (apply bc args)
  (define (get/w idx)
    (case idx
      ('GND GND)
      ('VDD VDD)
      ('CLK CLK)
      (else (vector-ref args idx))))
  ((lambda (s) (s s bc))
   (lambda (s bc)
     (and (pair? bc)
          (let* ((call (first bc))
                 (mod (car call))
                 (args (cdr call))
                 (wires (vector-map get/w args)))
            (cond ((eq? mod 'set-high)            (set-high wires))
                  ((eq? mod 'instrument-screen)   (screen wires))
                  ((eq? mod 'instrument-keyboard) (keyboard wires))
                  ((eq? mod 'instrument-rom)      (rom wires))
                  ((eq? mod 'instrument-reg-pc)   (reg/pc wires))
                  ((eq? mod 'instrument-reg-D)    (reg/d wires))
                  ((eq? mod 'instrument-reg-A)    (reg/a wires))
                  ((eq? mod 'instrument-reg-M)    (reg/m wires))
                  ((lisp/instrument? mod)
                   (error "lisp instrument not defined" mod))
                  (else
                   (eval mod wires)))
            (s s (rest bc)))))))

;;; DISASSEMBLER

(define disassemble
  (lambda (i)
    "Disassemble binary hack programs."
    (define dest
      (let ((destA (vector-ref i 10))
            (destD (vector-ref i 11))
            (destM (vector-ref i 12)))
        (if (> (+ destA destD destM) 0)
            (~a (if (zero? destA) "" "A")
                (if (zero? destD) "" "D")
                (if (zero? destM) "" "M"))
            false)))
    (define comp
      (let ((a  (vector-ref i 3))
            (c (o binvec->dec rev-vector (vector-copy i 4 10))))
        (define k
          (if (zero? a)
              (case c
                ((48) "A")
                ((42) "0")
                ((12) "D")
                (else (~a "a=0;..." c)))
              (case c
                ((55) "M+1")
                ((50) "M-1")
                ((49) "!M")
                ((48) "M")
                ((19) "D-M")
                ((2)  "D+M")
                (else "a=1;..." c))))
        k))
    (define jmp
      (let ((j< (vector-ref i 13))
            (j= (vector-ref i 14))
            (j> (vector-ref i 15)))
        (if (> (+ j< j= j>) 0)
            (~a (if (zero? j<) "" "<")
                (if (zero? j>) "" ">")
                (if (zero? j=) "" "="))
            false)))
    (if (zero? (vector-ref i 0))
        (~a "A = " (o binvec->dec rev-vector i))
        (if dest
            (~a dest "=" comp)
            (if (= 3 (string-length jmp))
                "goto A"
                (~a "if " comp " " jmp " 0 then goto A"))))))

;;; DEBUGGING

(define (sim.eval/dbg id env)
  (~a "EVAL: " id " @@ " (vector-length env)))
(define (sim.apply/dbg bc args)
    (~a "APPLY: @@ " (vector-map wire/get/signal args) "\n"
        (foldr string-append "--" (map (lambda (x) (~a "\t * " x "\n")) bc))))

(define dec->boolvec
  (lambda (n w)
    (vector-map positive? (dec->binvec n w))))
(define dec->binvec
  (lambda (n w)
    (list->vector (dec->bin w n))))
(define boolvec->dec
  (lambda (vec)
    (foldr +
           0
           (map (lambda (v i)
                  (if v (expt 2 i) 0))
                (vector->list vec)
                (range (vector-length vec))))))
(define binvec->dec
  (lambda (vec)
    (foldr +
           0
           (map (lambda (v i)
                  (if (zero? v) 0 (expt 2 i)))
                (vector->list vec)
                (range (vector-length vec))))))
(define bus->dec
  (lambda (bus)
    (foldr +
           0
           (map (lambda (w i)
                  (if (wire/get/signal w)
                      (expt 2 i)
                      0))
                (vector->list bus)
                (range (vector-length bus))))))
(define bus->bin
  (lambda (bus)
    (vector-map (lambda (w) (if (wire/get/signal w) 1 0))
                bus)))
(define bus/set!
  (lambda (bus vals)
    ;; (d vals)
    (vector-map wire/set/signal!
                bus
                (list->vector (reverse (vector->list vals))))))

(define status
  (lambda ()
    (d "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      (d "PC => " @PC)
      (d "A => " @A (binvec->dec @A))
      (d "D => " @D (binvec->dec @D))
      (p " M => " 'O= @Mo " " 'A= @Ma " " 'w= @Mw "\n")
      (newline)
      (newline)))

(define tick-tock
    (lambda ()
      (define i (vector-map (lambda (x) (if x 1 0)) (vector-ref binary/file @PC)))
      (d "\n\nPC=" @PC "EXECUTE:" (disassemble i))
      (tick) (scheduler/do)
      (tock) (scheduler/do)
      (status)))

(define reset (new/wire))
(eval 'Computer (vector reset))

(define run
  (lambda ()
    (tick-tock)
    (run)))

(run)

(define bus/bin/probe
  (lambda (name bus)
    "An action that only provides the state of a wire it is connected
to. It installs nothing in the scheduler."
    (define print-action
      (lambda ()
        (newline)
        (o name (strcat (bus->bin bus)) "\n")))

    (bus (lambda (wire) (wire/add/todo/action wire print-action)))))
(define bus/dec/probe
  (lambda (msg bus)
    "An action that only provides the state of a wire it is connected
to. It installs nothing in the scheduler."
    (define print-action
      (lambda ()
        (o msg (bus->dec bus) "\n")))
    (bus (lambda (wire) (wire/add/todo/action wire print-action)))))

(module+ test


  (define probe
    (lambda wires
      "An action that only provides the state of a wire it is connected
  to. It installs nothing in the scheduler."
      (define f (lambda (x) (~a x  #:width 20 #:align 'right)))
      (define bus
        '(make/bus wires))
      (define bin
        (lambda ()
          (bus->bin bus)))
      (define dec
        (lambda ()
          (bus->dec bus)))
      (define str/vals
        (lambda ()
          (map (lambda (x) (f x))
               (bus wire/get/signal))))
      (define str/diffs
        (lambda ()
          (map (lambda (x) (f (if x "" "*")))
               (map boolean=? (bus wire/get/signal) prev/vals))))
      (define prev/vals
        (bus wire/get/signal))
      (define print-action
        (lambda ()
          (newline)
          (o "TIMER: " (scheduler/current/time) "; "
             "TASKS: " (scheduler/tasks/count)  "; "
             "POINTER: " (scheduler/pointer)  ";\n")
          (o "CURRENT-VALUES: " (strcat (str/vals)) "\n")
          (o "DIFFERENCES:    " (strcat (str/diffs)) "\n")
          (o "bin:" (strcat (bin)) "---" "dec:" (dec) "\n")
          (set! prev/vals (bus wire/get/signal))))
      (map (lambda (wire) (wire/add/todo/action wire print-action)) wires)))

  (define-values (a b c
                    sel
                    sum carry
                    s r nq q
                    clk
                    data en
                    in in1 in2
                    load
                    inc
                    out out1 out2 out3 out4)
    (values (new/wire) (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire)))

  (define-values (zx nx zy ny f no zr ng)
    (values (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire)))

  (define-values (o0 o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15)
    (values (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire)))
  (define-values (i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15)
    (values (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire)))
  (define-values (x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)
    (values (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire)))
  (define-values (y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15)
    (values (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire)
            (new/wire) (new/wire) (new/wire) (new/wire)))

  (define mk/io
    (lambda names
      (list->vector (flatten names))))

  (define s!
    (lambda (w sig-val)
      (wire/set/signal! w sig-val)
      (scheduler/do)))

  (define S!
    (lambda (w s)
      (wire/set/signal! w s)
      (d "~~~> TICK")
      (tick) (scheduler/do)
      (d "~~~> TOCK")
      (tock) (scheduler/do)))

  (define (test/and)
    (d "--- AND")
    (eval 'And (vector a b out) 0)
    (probe a b out)
    (scheduler/do)
    (d "~~~>")
    (s! a true)
    (s! b true)
    (s! b false)
    (s! b true)
    (s! a false)
    (s! a true)
    'done)

  (define (test/mux)
    (d "--- MUX")
    (eval 'Mux (vector a b sel out) 0)

    (probe a b sel out)
    (scheduler/do)
    (d "~~~>")

    (s! a true)
    (s! b true)
    (s! sel true)

    (s! b false)
    (s! sel false)

    (s! a false)
    (s! sel true)
    (s! a true)
    (s! sel false)
    (s! sel true)
    (s! a false)
    (s! b true)
    'done)

  (define (test/not)
    (eval 'Not (vector in out) 0)
    (probe in out)
    (scheduler/do)
    (d "~~~>")

    (s! in false)
    (s! in false)
    (s! in true)
    (s! in true)
    (s! in false)
    'done)

  (define (test/ha)
    (eval 'HalfAdder (vector a b sum carry) 0)
    (probe a b sum carry)
    (scheduler/do)
    (d "~~~>")

    ;; 1+0
    (s! a true)
    (scheduler/do)

    ;; 1+1
    (s! b true)
    (scheduler/do)

    ;; 0+1
    (s! a false)
    (scheduler/do)

    ;; 0+0
    (s! b false)
    (scheduler/do)
    'done)

  (define (test/fa)
    (d "--- FULL ADDER")
    (eval 'FullAdder (vector a b c sum carry) 0)
    (scheduler/do)
    (probe a b c sum carry)
    (scheduler/do)

    (d "~~~>")

    ;; 1+0+0
    (s! a true)
    (scheduler/do)

    ;; 1+1+0
    (s! b true)
    (scheduler/do)

    ;; 0+1+0
    (s! a false)
    (scheduler/do)

    ;; 0+0+0
    (s! b false)
    (scheduler/do)

    (s! c true)

    ;; 1+0+1
    (s! a true)
    (scheduler/do)

    ;; 1+1+1
    (s! b true)
    (scheduler/do)

    ;; 0+1+1
    (s! a false)
    (scheduler/do)

    ;; 0+0+1
    (s! b false)
    (scheduler/do)

    'done)

  (define (test/rslatch)
    (d "RSLatch")
    (eval 'RSLatch (vector s r q nq) 0)
    (probe s r q nq)
    (scheduler/do)

    (d "~~~>")

    (s! r false)
    (s! r true)

    (s! s false)
    (s! s true)

    'done)

  (define (test/dlatch)
    (eval 'DLatch (vector data clk q nq) 0)
    (probe data clk q nq)
    (scheduler/do)
    (d "~~~>")

    (s! clk true)
    (s! data true)
    (s! clk false)
    (s! data false)
    (s! clk true)
    (s! clk false)
    (s! data true)
    (s! clk true)
    (s! clk false)
    (s! data false)
    'done)

  (define (test/dff)
    (d "DFF")
    (eval 'DFF (vector in out) 0)
    (probe in out CLK)
    (scheduler/do)

    (d "~~~>")

    (S! in true)
    (S! in true)
    (S! in false)
    (tick)(scheduler/do)
    (tock)(scheduler/do)
    (s! in true)
    (tick)(scheduler/do)
    (tock)(scheduler/do)

    (S! in false)
    (S! in false)
    (S! in true)

    'done)

  (define (test/bit)
    (d "Bit")
    (eval 'Bit (vector in load out) 0)
    (probe in load out)
    (scheduler/do)

    (d "~~~>")

    (S! load true)
    (S! in true)
    (S! load false)
    (S! in false)
    (S! load true)
    (S! load false)
    (S! in true)
    (S! load true)
    (S! in false)
    (S! in true)
    (S! load false)
    (S! in false)

    'done)

  (define (test/inc16)
    (define out-wires (list o15 o14 o13 o12 o11 o10 o9 o8 o7 o6 o5 o4 o3 o2 o1 o0))
    (define in-wires  (list i15 i14 i13 i12 i11 i10 i9 i8 i7 i6 i5 i4 i3 i2 i1 i0))
    '(define bus-out   (make/bus out-wires))
    '(define bus-in    (make/bus in-wires))

    (eval 'Inc16 (mk/io in-wires out-wires) 0)
    ;; (bus/dec/probe "IN:" bus-in)
    ;; (bus/dec/probe "OUT:" bus-out)
    (scheduler/do)

    ;(bus/set! bus-in (dec->bin 16 15))
    (scheduler/do)

    ;(bus/set! bus-in (dec->bin 16 2222))
    (scheduler/do)

    ;(bus/set! bus-in (dec->bin 16 8764))
    (scheduler/do)

    'done)

  (define (test/add16)
    (d "ADD~16 TEST")
    (define x-wires   (list x15 x14 x13 x12 x11 x10 x9 x8 x7 x6 x5 x4 x3 x2 x1 x0))
    (define y-wires   (list y15 y14 y13 y12 y11 y10 y9 y8 y7 y6 y5 y4 y3 y2 y1 y0))
    (define out-wires (list o15 o14 o13 o12 o11 o10 o9 o8 o7 o6 o5 o4 o3 o2 o1 o0))
    (define bus-x   '(make/bus x-wires))
    (define bus-y   '(make/bus y-wires))
    (define bus-out '(make/bus out-wires))
    (eval 'Add16 (mk/io x-wires y-wires out-wires) 0)
    (scheduler/do)

    (bus/dec/probe "OP:" bus-out)
    ;; (bus/bin/probe "OP:" bus-out)

    (bus/set! bus-x (dec->bin 16 2))
    (bus/set! bus-y (dec->bin 16 2))
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 20))
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 222))
    (bus/set! bus-y (dec->bin 16 999))
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 1001))
    (bus/set! bus-y (dec->bin 16 999))
    (scheduler/do)

    'done)

  (define (test/program/counter)
    (d "PC")
    (define out-wires (list o15 o14 o13 o12 o11 o10 o9 o8 o7 o6 o5 o4 o3 o2 o1 o0))
    (define in-wires  (list i15 i14 i13 i12 i11 i10 i9 i8 i7 i6 i5 i4 i3 i2 i1 i0))
    (define bus-out   '(make/bus out-wires))
    (define bus-in    '(make/bus in-wires))
    (eval 'PC (mk/io in-wires load inc reset out-wires) 0)
    (bus/dec/probe "IN:" bus-in)
    (bus/dec/probe "OUT:" bus-out)
    (scheduler/do)

    (define next
      (lambda (_)
        (tick-tock)
        (d "                          ===>" (bus->dec bus-out))))

    (wire/set/signal! inc true)
    (for-each next (range 100))
    (wire/set/signal! load true)
    (bus/set! bus-in (dec->bin 16 777))
    (next '_)
    (wire/set/signal! load false)
    (for-each next (range 100))
    (wire/set/signal! reset true)
    (next '_)
    (wire/set/signal! reset false)
    (for-each next (range 100))

    'done)

  (define (test/alu)
    (d "ALU")
    (define x-wires   (list x15 x14 x13 x12 x11 x10 x9 x8 x7 x6 x5 x4 x3 x2 x1 x0))
    (define y-wires   (list y15 y14 y13 y12 y11 y10 y9 y8 y7 y6 y5 y4 y3 y2 y1 y0))
    (define out-wires (list o15 o14 o13 o12 o11 o10 o9 o8 o7 o6 o5 o4 o3 o2 o1 o0))
    (define op-wires  (list zx nx zy ny f no))
    (define bus-x   '(make/bus x-wires))
    (define bus-y   '(make/bus y-wires))
    (define bus-out '(make/bus out-wires))
    (define bus-op  '(make/bus op-wires))

    (eval 'ALU (mk/io x-wires y-wires op-wires out-wires zr ng) 0)

    (bus/dec/probe "DEC:" bus-out)
    ;; (bus/bin/probe "BIN:" bus-out)
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 10))
    (bus/set! bus-y (dec->bin 16 15))
    (bus/set! bus-op '(0 0 0 0 1 0))    ; X+Y
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 15))
    (bus/set! bus-op '(0 1 0 0 1 1))    ; X-Y
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(0 0 0 1 1 1))    ; Y-X
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(0 0 0 0 0 0))    ; X&Y
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(0 1 0 1 0 1))    ; X|Y
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(0 0 1 1 1 0))    ; X-1
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(1 1 0 0 1 0))    ; Y-1
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(0 1 1 1 1 1))    ; X+1
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(1 1 0 1 1 1))    ; Y+1
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(0 0 1 1 1 1))    ; -X
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(1 1 0 0 1 1))    ; -Y
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(0 0 1 1 0 1))    ; !X
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(1 1 0 0 0 1))    ; !Y
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(0 0 1 1 0 0))    ; X
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(1 1 0 0 0 0))    ; Y
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(1 1 1 0 1 0))    ; -1
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(1 1 1 1 1 1))    ; 1
    (scheduler/do)

    (bus/set! bus-x (dec->bin 16 20))
    (bus/set! bus-y (dec->bin 16 25))
    (bus/set! bus-op '(1 0 1 0 1 0))    ; 0
    (scheduler/do)

    'done)

  (define (test/ram512)
    (define in-wires   (list x15 x14 x13 x12 x11 x10 x9 x8 x7 x6 x5 x4 x3 x2 x1 x0))
    (define addr-wires (list y8 y7 y6 y5 y4 y3 y2 y1 y0))
    (define out-wires (list o15 o14 o13 o12 o11 o10 o9 o8 o7 o6 o5 o4 o3 o2 o1 o0))
    (define bus-in   '(make/bus in-wires))
    (define bus-addr '(make/bus addr-wires))
    (define bus-out  '(make/bus out-wires))

    (eval 'RAM512 (mk/io in-wires load addr-wires out-wires) 0)
    (tick-tock)

    (bus/dec/probe "OUT:" bus-out)

    (bus/set! bus-in (dec->bin 16 1000))
    (bus/set! bus-addr (dec->bin 16 200))
    (wire/set/signal! load true)
    (tick) (scheduler/do) (tock) (scheduler/do)

    (bus/set! bus-in (dec->bin 16 2222))
    (bus/set! bus-addr (dec->bin 16 300))
    (wire/set/signal! load true)
    (tick) (scheduler/do) (tock) (scheduler/do)

    (bus/set! bus-in (dec->bin 16 3000))
    (bus/set! bus-addr (dec->bin 16 100))
    (wire/set/signal! load true)
    (tick) (scheduler/do) (tock) (scheduler/do)

    (bus/set! bus-in (dec->bin 16 0))
    (bus/set! bus-addr (dec->bin 16 200))
    (wire/set/signal! load false)
    (tick) (scheduler/do) (tock) (scheduler/do)

    (bus/set! bus-in (dec->bin 16 0))
    (bus/set! bus-addr (dec->bin 16 777))
    (wire/set/signal! load false)
    (tick) (scheduler/do) (tock) (scheduler/do)

    (bus/set! bus-in (dec->bin 16 0))
    (bus/set! bus-addr (dec->bin 16 100))
    (wire/set/signal! load false)
    (tick) (scheduler/do) (tock) (scheduler/do)

    (bus/set! bus-in (dec->bin 16 0))
    (bus/set! bus-addr (dec->bin 16 300))
    (wire/set/signal! load false)
    (tick) (scheduler/do) (tock) (scheduler/do)
    'done)

  (define (test/ram16k)
    (d "RAM 16K")
    (define in-wires   (list x15 x14 x13 x12 x11 x10 x9 x8 x7 x6 x5 x4 x3 x2 x1 x0))
    (define addr-wires (list         y13 y12 y11 y10 y9 y8 y7 y6 y5 y4 y3 y2 y1 y0))
    (define out-wires  (list o15 o14 o13 o12 o11 o10 o9 o8 o7 o6 o5 o4 o3 o2 o1 o0))
    (define bus-in   '(make/bus in-wires))
    (define bus-addr '(make/bus addr-wires))
    (define bus-out  '(make/bus out-wires))
    (define print (lambda () (d "O===" (bus->dec bus-out)) (newline)))

    (eval 'RAM16K (mk/io in-wires load addr-wires out-wires) 0)
    (tick-tock)

    (d "~~~>")

    (wire/set/signal! load true)

    (bus/set! bus-in (dec->bin 16 1000))
    (bus/set! bus-addr (dec->bin 16 12200)) ; 12200 => 1000
    (tick-tock) (print)

    (bus/set! bus-in (dec->bin 16 2222))
    (bus/set! bus-addr (dec->bin 16 13300)) ; 13300 => 2222
    (tick-tock) (print)

    (bus/set! bus-in (dec->bin 16 3000))
    (bus/set! bus-addr (dec->bin 16 11100)) ; 11100 => 3000
    (tick-tock) (print)

    (bus/set! bus-in (dec->bin 16 0))
    (wire/set/signal! load false)

    (bus/set! bus-addr (dec->bin 16 12200)) ; < 12200
    (tick-tock) (print)

    (bus/set! bus-addr (dec->bin 16 11100)) ; < 11100
    (tick-tock) (print)

    (bus/set! bus-addr (dec->bin 16 13300)) ; < 13300
    (tick-tock) (print)

    (bus/set! bus-addr (dec->bin 16 13301)) ; < 13301
    (tick-tock) (print)

    (wire/set/signal! load true)

    (bus/set! bus-in (dec->bin 16 1234))
    (bus/set! bus-addr (dec->bin 16 12200)) ; 12200 => 1234
    (tick-tock) (print)

    (bus/set! bus-in (dec->bin 16 9876))
    (bus/set! bus-addr (dec->bin 16 13300)) ; 13300 => 9876
    (tick-tock) (print)

    (bus/set! bus-in (dec->bin 16 0))
    (wire/set/signal! load false)

    (bus/set! bus-addr (dec->bin 16 12200)) ; < 12200
    (tick-tock) (print)

    (bus/set! bus-addr (dec->bin 16 13300)) ; < 13300
    (tick-tock) (print)

    (bus/set! bus-addr (dec->bin 16 11100)) ; < 11100
    (tick-tock) (print)

    'done)

  (define (test/ram4k)
    (define in-wires   (list x15 x14 x13 x12 x11 x10 x9 x8 x7 x6 x5 x4 x3 x2 x1 x0))
    (define addr-wires (list y11 y10 y9 y8 y7 y6 y5 y4 y3 y2 y1 y0))
    (define out-wires (list o15 o14 o13 o12 o11 o10 o9 o8 o7 o6 o5 o4 o3 o2 o1 o0))
    (define bus-in   '(make/bus in-wires))
    (define bus-addr '(make/bus addr-wires))
    (define bus-out  '(make/bus out-wires))
    (define print (lambda ()
                    ;;(d "O===" (bus->bin bus-out))
                    (d "O===" (bus->dec bus-out))
                    (newline)))
    (eval 'RAM4K (mk/io in-wires load addr-wires out-wires) 0)
    (scheduler/do)

    (wire/set/signal! load true)

    (bus/set! bus-in (dec->bin 16 1000))
    (bus/set! bus-addr (dec->bin 16 2200)) ; 2200 ==> 1000
    (tick-tock) (print)

    (bus/set! bus-in (dec->bin 16 2222))
    (bus/set! bus-addr (dec->bin 16 3300)) ; 3300 ==> 2222
    (tick-tock) (print)

    (bus/set! bus-in (dec->bin 16 3000))
    (bus/set! bus-addr (dec->bin 16 1100)) ; 1100 ==> 3000
    (tick-tock) (print)

    (wire/set/signal! load false)
    (bus/set! bus-in (dec->bin 16 0))

    (bus/set! bus-addr (dec->bin 16 2200)) ; < 2200
    (tick-tock) (print)

    (bus/set! bus-addr (dec->bin 16 1100)) ; < 1100
    (tick-tock) (print)

    (bus/set! bus-addr (dec->bin 16 2300)) ; < 2300
    (tick-tock) (print)

    (wire/set/signal! load true)

    (bus/set! bus-in (dec->bin 16 8888))
    (bus/set! bus-addr (dec->bin 16 2200)) ; 2200 ==> 8888
    (tick-tock) (print)

    (bus/set! bus-in (dec->bin 16 7777))
    (bus/set! bus-addr (dec->bin 16 1100)) ; 1100 ==> 7777
    (tick-tock) (print)

    (wire/set/signal! load false)
    (bus/set! bus-in (dec->bin 16 0))

    (bus/set! bus-addr (dec->bin 16 2200)) ; < 2200
    (tick-tock) (print)

    (bus/set! bus-addr (dec->bin 16 1100)) ; < 1100
    (tick-tock) (print)
    'done)

  (define (test/cpu)
    (d "CPU")

    (define-values (iM15 iM14 iM13 iM12 iM11 iM10 iM9 iM8 iM7 iM6 iM5 iM4 iM3 iM2 iM1 iM0)
      (values (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire) (new/wire)))
    (define-values (oM15 oM14 oM13 oM12 oM11 oM10 oM9 oM8 oM7 oM6 oM5 oM4 oM3 oM2 oM1 oM0)
      (values (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire) (new/wire)))
    (define-values (aM14 aM13 aM12 aM11 aM10 aM9 aM8 aM7 aM6 aM5 aM4 aM3 aM2 aM1 aM0)
      (values (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire)))
    (define-values (pcM14 pcM13 pcM12 pcM11 pcM10 pcM9 pcM8
                    pcM7 pcM6 pcM5 pcM4 pcM3 pcM2 pcM1 pcM0)
      (values (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire)))
    (define-values (instr15 instr14 instr13 instr12 instr11 instr10 instr9 instr8
                    instr7 instr6 instr5 instr4 instr3 instr2 instr1 instr0)
      (values (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire) (new/wire)
              (new/wire) (new/wire) (new/wire) (new/wire)))

    (define writeM (new/wire))

    (define inM         (list iM15 iM14 iM13 iM12 iM11 iM10 iM9 iM8
                              iM7 iM6 iM5 iM4 iM3 iM2 iM1 iM0))
    (define instr       (list instr15 instr14 instr13 instr12 instr11 instr10 instr9 instr8
                              instr7 instr6 instr5 instr4 instr3 instr2 instr1 instr0))
    (define outM        (list oM15 oM14 oM13 oM12 oM11 oM10 oM9 oM8
                              oM7 oM6 oM5 oM4 oM3 oM2 oM1 oM0))
    (define addrM       (list aM14 aM13 aM12 aM11 aM10 aM9 aM8
                              aM7 aM6 aM5 aM4 aM3 aM2 aM1 aM0))
    (define pc          (list pcM14 pcM13 pcM12 pcM11 pcM10 pcM9 pcM8
                              pcM7 pcM6 pcM5 pcM4 pcM3 pcM2 pcM1 pcM0))
    (define bus-inM     '(make/bus inM))
    (define bus-outM    '(make/bus outM))
    (define bus-instr   '(make/bus instr))
    (define bus-addrM   '(make/bus addrM))
    (define bus-pc      '(make/bus pc))

    (eval 'CPU (mk/io inM instr reset outM writeM addrM pc))

    (bus/dec/probe "OUT-M:" bus-outM)
    (bus/dec/probe "addrM:" bus-addrM)
    (bus/bin/probe "addrM:" bus-addrM)
    (tick-tock)

    (d "~~~>")

    (bus/set! bus-instr '(0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0))  ; A = 10
    (tick-tock)

    (d "~~~>")

    (define cpu/c/test
      (lambda (computation destination jump)
        (let ((instruction (append '(1 1 1) computation destination jump)))
          (bus/set! bus-instr (reverse instruction)))
        (tick-tock)))

    (cpu/c/test '(0 1 1 0 1 1 1)        ; A+1
                '(1 1 0)                ; AD
                '(0 0 1))               ; J > 0

    (cpu/c/test '(0 0 0 0 1 1 1)        ; A-D
                '(1 0 0)                ; A
                '(0 1 0))               ; J = 0

    '(bus/set! bus-inM '(1 1 0 1 1 0 0 0 0 0 0 0 0 0 0 0))

    (cpu/c/test '(1 0 0 0 1 1 1)        ; M-D
                '(1 0 1)                ; AM
                '(0 0 0))


    'done)

  (define (counters)
    (values
     (counter/get 'wires)
     (counter/get 'dff)
     (counter/get 'and)
     (counter/get 'or)
     (counter/get 'not)
     (counter/get 'nand)
     (counter/get 'key)
     (counter/get 'scr)
     (counter/get 'rom)
     (counter/get 'ram8)
     (counter/get 'ram64)
     (scheduler/current/time)))
  (define (test/computer)
    (eval 'Computer (vector reset))

    (tick-tock)
    (tick-tock)
    (tick-tock)
    (tick-tock)

    (call-with-values counters (lambda x (map d x)))
    (void))



  '(test/and)
  '(test/mux)
  '(test/not)
  '(test/ha)
  '(test/fa)
  '(test/rslatch)
  '(test/dlatch)
  '(test/dff)
  '(test/bit)
  '(test/inc16)
  '(test/add16)
  '(test/program/counter)
  '(test/alu)
  '(test/ram512)
  '(test/ram16k)
  '(test/ram4k)
  '(test/cpu)
  (test/computer)
  'test/done



  (define verbose-mode (make-parameter #f))
  (define profiling-on (make-parameter #f))
  (define optimize-level (make-parameter 0))
  (define link-flags (make-parameter null))

  (define file-to-compile
    (command-line
     #:program "compiler"
     #:once-each
     [("-v" "--verbose") "Compile with verbose messages"
      (verbose-mode #t)]
     [("-p" "--profile") "Compile with profiling"
      (profiling-on #t)]
     #:once-any
     [("-o" "--optimize-1") "Compile with optimization level 1"
      (optimize-level 1)]
     ;; show help on separate lines
     ["--optimize-2"        (
                             "Compile with optimization level 2,"
                             "which includes all of level 1")
      (optimize-level 2)]
     #:multi
     ;; flag takes one argument
     [("-l" "--link-flags") lf
      "Add a flag <lf> for the linker"
      (link-flags (cons lf (link-flags)))]
     ;; expect one command-line argument: <filename>
     #:args (filename)
     ;; return the argument as a filename to compile
     filename))
  )


