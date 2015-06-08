#lang racket ; -*- mode:racket ; buffer-read-only:t -*-

;;; Given hardware structure files, build the datapath and control
;;; automata and simulate the electrical signal flow by using
;;; propagators.

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
(require "sched.rkt")
(require "hack-asm-phrase-struct.rkt")

;;; GLOBAL VARIABLES
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

(define load/abstraction
  (lambda (name)
    "In principio erat Verbum"
    (define data (cdr (file->lines (~a "./hw/" name) #:mode 'text)))
    (define gate/name (string->symbol (list-ref data 0)))
    (define i/w (string->number (list-ref data 1)))
    (define code
      (map (lambda (instr/str)
             (define instr (string-split instr/str))
             (define function (string->symbol (car instr)))
             (define args (vector-map string->symbol (list->vector (cdr instr))))
             (cons function
                   (vector-map (lambda (idx)
                                 (case idx
                                   ('GND 'GND)
                                   ('VDD 'VDD)
                                   ('CLK 'CLK)
                                   (else (string->number (symbol->string idx)))))
                               args)))
           (cddr data)))
    (p ".")
    (list gate/name i/w code)))
(define hs-abstractions
  (let ((syms (make-hash)))
    (define hw/file? (lambda (f) (and (equal? #"hw" (filename-extension f)) f)))
    (define hw/files (filter hw/file? (directory-list "./hw/")))

    (p " Loading HWS abstractions ")
    (set! syms (make-hash (map load/abstraction hw/files)))
    (d "done.")
    (lambda (id)
      (define def (hash-ref syms id))
      (values (car def) (cadr def)))))

;;; CELLS
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

(define new/cell
  (lambda ()
    (define signal false)
    (define todo-list '())
    (define reader-list '())
    (define delay-list '())
    ;; count all cells
    (1+ 'cells)
    ;; permissions definition
    (define get
      (lambda () signal))
    (define set
      (lambda (new/val)
        (or (boolean=? new/val signal)
            (begin
              (set! signal new/val)
              (SCHED+ '__ 'WIRE reader-list todo-list delay-list)))))
    (define add
      (lambda (reader prop/delay prop/action)
        (1+ 'cell/action)
        (set! todo-list (cons prop/action todo-list))
        (set! reader-list (cons reader reader-list))
        (set! delay-list (cons prop/delay delay-list))
        (reader)
        (SCHED+ prop/delay prop/action '.. '.. '..)))
    (define permissions
      (lambda (p)
        (p get
           set
           add)))
    permissions))
(define cell/get/signal
  (lambda (cell)
    (cell (lambda (g _ __) (g)))))
(define cell/get/signal/bin
  (lambda (cell)
    (cell (lambda (g _ __) (if (g) 1 0)))))
(define cell/set/signal!
  (lambda (cell new/val)
    (or (memq new/val '(#t #f))
        (error "!! signal's value must be boolean" new/val))
    (cell (lambda (_ s __) (s new/val)))))
(define cell/add/action
  (lambda (cell reader propagation/delay propagator/action)
    (cell (lambda (_ __ a) (a reader propagation/delay propagator/action)))))

(define VDD
  (lambda (p)
    "Nomen est omen"
    (p (lambda .. true)
       (lambda .. 'nil)
       (lambda .. 'nil))))
(define GND
  (lambda (p)
    "Nomen est omen"
    (p (lambda .. false)
       (lambda .. 'nil)
       (lambda .. 'nil))))

(define CLK
  (let ((signal false))
    "Nomen est omen"
    (define todo-list '())
    (define reader-list '())
    (define delay-list '())
    ;; permissions
    (define get
      (lambda () signal))
    (define set
      (lambda (nv)
        (and DBG/STATUS (dbg))
        (case nv
          ('high (set! signal true))
          ('low (set! signal false))
          (else
           (error "use TICK and TOCK to set the clock.")))
        (SCHED+ '.. 'WIRE reader-list todo-list delay-list)
        (SCHED>>>)))
    (define add
      (lambda (reader prop/delay prop/action)
        (1+ 'clock)
        (set! todo-list (cons prop/action todo-list))
        (set! reader-list (cons reader reader-list))
        (set! delay-list (cons prop/delay delay-list))
        (reader)
        (SCHED+ prop/delay prop/action '.. '.. '..)))
    (define permissions
      (lambda (p)
        (p get
           set
           add)))
    permissions))
(define tick
  (lambda () (CLK (lambda (_ s __) (s 'high)))))
(define tock
  (lambda () (CLK (lambda (_ s __) (s 'low)))))

;;; PROPAGATORS
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

(define propagation/delay/nand           1)
(define propagation/delay/add16          1)
(define propagation/delay/inc16          1)
(define propagation/delay/rom            1)
(define propagation/delay/ram            1)
(define propagation/delay/output/console 1)
(define propagation/delay/input/console  1)

;;; PROPAGATOR FOR ATOMIC STRUCTURES (COMBINATIONAL LOGIC)
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

(define Nand
  (lambda (io)
    "A propagator that unidirectionally computes the logical NAND from
the inputs A and B toward the output OUT.

     The NAND propagator will install the nand/action on the cells a
and b, which will schedule a thunk after propagation/delay/nand, when
the signal a or b changes.

    The scheduler will execute an action that will set the value of
the output port, which is equal to the logical nand of the signal
value of a and b."
    (define a   (vector-ref io 0))
    (define b   (vector-ref io 1))
    (define out (vector-ref io 2))

    (1+ 'nand)
    
    (define VAL (queue:make))
    
    (define reader
      (lambda ()
        (VAL '+ (nand (cell/get/signal a)
                      (cell/get/signal b)))))
    
    (define nand/action
      (lambda ()
        (cell/set/signal! out (VAL '-))))
    
    (cell/add/action a reader propagation/delay/nand nand/action)
    (cell/add/action b reader propagation/delay/nand nand/action)
    'ok))

(define add16
  (let ((table
         (let ((table
                (vector
                 '(#f . 0)
                 '(#t . 0)
                 '(#t . 0)
                 '(#f . 1)
                 '(#t . 0)
                 '(#f . 1)
                 '(#f . 1)
                 '(#t . 1))))
           (lambda (carry u v)
             (vector-ref table (+ carry (* 2 v) (* 4 u)))))))
    (lambda (io)
      (define buses (vector-split io '(16 16 16)))
      (define a (car buses))
      (define b (cadr buses))
      (define out (caddr buses))
      (define VAL (queue:make))
      
      (define reader
        (lambda ()
          (VAL '+ (compute/+ (bus->binvec a) (bus->binvec b)))))
      
      (define compute/+
        (lambda (va vb)
          (let ((carry 0))
            (vector-map (lambda (u v)
                          (let ((add/carry (table carry u v)))
                            (set! carry (cdr add/carry))
                            (car add/carry)))
                        va
                        vb))))
      
      (define +/action
        (lambda ()
          (bus-set! out (VAL '-))))

      (bus-add-action! a +/action reader propagation/delay/add16)
      (bus-add-action! b +/action reader propagation/delay/add16)
      'ok)))
(define inc16
  (lambda (io)
    (define buses (vector-split io '(16 16)))
    (define in (car buses))
    (define out (cadr buses))
    
    (define VAL (queue:make))
    
    (define reader
      (lambda ()
        (VAL '+ (compute/1+ (bus->boolvec in)))))
    
    (define compute/1+
      (lambda (W)
        ((lambda (s)
           (s s 0))
         (lambda (s k)
           (cond ((> k 15)
                  W)
                 ((vector-ref W k)
                  (vector-set! W k false)
                  (s s (add1 k)))
                 (else
                  (vector-set! W k true)
                  W))))))
    
    (define 1+/action
      (lambda ()
        (bus-set! out (VAL '-))))
    
    (bus-add-action! in 1+/action reader propagation/delay/inc16)
    'ok))

;;; PROPAGATOR FOR READ-ONLY MEMORY (COMBINATIONAL LOGIC)
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

(define rom
  (lambda (args)
    (d "ROM installed.")
    (d "The microcode has a length of" program/length "instructions.")
    (define bus (vector-split args '(15 16)))
    (define bus-addr (car bus))
    (define bus-out (cadr bus))
    
    (define rom/propagator
      (lambda ()
        (and (< (bus->dec bus-addr) program/length)
             (bus-set! bus-out (control (bus->dec bus-addr))))))

    (bus-add-action! bus-addr rom/propagator (lambda _ _) propagation/delay/rom)
    'semper-veritas))

;;; PROPAGATORS FOR RANDOM-ACCESS MEMORY (SEQUENTIAL LOGIC)
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
(define ram
  (lambda (BITS)
    (lambda (cells)
      "Any sequential module must simulate the behavior of a Bit and
in the same time it must behave as a combinational component."

      (and (positive? BITS)
           (d "RAM activated on" BITS "bits."))

      (define buses (vector-split cells `(16 1 ,BITS 16)))
      
      (define in   (car buses))
      (define lo@d (vector-ref (cadr buses) 0))
      (define addr (caddr buses))
      (define out  (cadddr buses))
      
      (define ADDR (lambda () (bus->dec addr)))
      (define IN (lambda () (bus->dec in)))
      (define LOAD (lambda () (cell/get/signal lo@d)))
      
      (define MEMORY (make-vector (expt 2 BITS) 0))
      (define Mget  (lambda () (dec->boolvec (vector-ref MEMORY (ADDR)) 16)))
      (define Mset! (lambda () (vector-set! MEMORY (ADDR) (IN))))
      
      (define CLOCK (lambda () (cell/get/signal CLK)))

      (define ram/output
        (lambda ()
          (bus-set! out (Mget))))
      
      (define ram/clock
        (lambda ()
          (if (CLOCK)
              (and (LOAD) (Mset!))
              (ram/output))))

      (cell/add/action CLK (lambda _ _) propagation/delay/ram
                       ram/clock)
      (bus-add-action! addr ram/clock (lambda _ _) propagation/delay/ram))))
(define ram16k (ram 14))
(define ram8 (ram 3))
(define ram1 (ram 0))

;;; LISP INSTRUMENTS
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

;;; PREDICATES
(define lisp/instrument?
  (lambda (sym)
    (char-lower-case? (string-ref (symbol->string sym) 0))))

(define set-high
  (lambda (args)
    "the latches must be reset at startup."
    
    ;; *CAUTION*
    ;; 
    ;; When we do a reset we need to take care that there may be
    ;; scheduled actions on the wires that preserve the value back to
    ;; reset.  These actions may propagate the previous value and the
    ;; reset may not have the desired effect.
    ;; 
    ;; The Hardware Compiler calls the lisp instruments (in particular
    ;; `set-high`) after the wires and functions were installed, so it
    ;; is possible some previous actions, scheduled by the functions
    ;; called before the lisp instruments, to enclose the previous
    ;; value.  When the lisp instrument is executed, these actions are
    ;; still present in todo-list and may corrupt the desired effect.
    ;; The ideal solution would be to add a new class of lisp
    ;; instruments that are executed after the wires are installed and
    ;; before the functions are executed (the execution of some
    ;; functions may schedule some actions), but in this current form
    ;; it's enough to do a successful reset for latches.  We keep the
    ;; things as simple, as they work so.  If we need to build more
    ;; complicated computers using The Little Computer, the addition
    ;; of this class of lisp instruments may be taken in
    ;; consideration.  Nil mortalibus ardui est.
    
    (1+ 'latches)
    (vector-map
     (lambda (w)
       ;; do not schedule the reset! do it directly during startup!
       ;; otherwise the latches will behave as oscillators. check what
       ;; actions were scheduled before the reset is done. these
       ;; actions may corrupt the reset.
       (cell/set/signal! w true))
     args)
    'semper-veritas))

;;; REGISTERS
(define register
  (let ((data (make-hash)))
    (define set
      (lambda (name bus)
        (d 'REG name 'ok.)
        (case name
          ((A D PC)
           (hash-set! data name bus))
          ((M)
           (define t (vector-split bus '(1 16)))
           (hash-set! data 'Mw (vector-ref (first t) 0))
           (hash-set! data 'Mo (second t))))
        'semper-veritas))
    (define get
      (lambda (id)
        (define v (hash-ref data id))
        (case id
          ((A D) (bus->binvec v))
          ((PC)  (bus->dec v))
          ((Mo)  (bus->dec v))
          ((Mw)  (cell/get/signal v)))))
    (lambda (m)
      (case m
        ('set! set)
        ('get  get)))))
(define @
  (lambda (id)
    ((register 'get) id)))
(define register-set!
  (lambda (n b)
    ((register 'set!) n b)))

;;; REGISTERS STATUS
(define DBG/STATUS false)
(define dbg
  (lambda ()
    (define i (boolvec->binvec (control (@ 'PC))))
    (d "CLOCK is" (if (cell/get/signal CLK) "ON" "OFF"))
    (d "PC = " (~a4 (@ 'PC)) " --" (strvec i) "-- " (disassembler i))
    (d "A  => " (strvec (@ 'A)) "" (binvec->dec (@ 'A)))
    (d "D  => " (strvec (@ 'D))    (binvec->integer (@ 'D)))
    (d "M  => " (~a 'ALU '=> (~a4 (@ 'Mo))) (~a 'Write: (if (@ 'Mw) 'OFF 'ON)))
    (p (make-string 50 #\~))
    (newline)
    'done))

;;; THE CONSOLE
(define terminal
  (let ((settings false)
        (stty (find-executable-path "stty")))
    (if (false? stty)
        (error "cannot find `stty`.")
        (lambda (m)
          (case m
            ('SAVE! (set! settings
                          (string-trim
                           (with-output-to-string
                               (lambda () (system* stty "--save")))))
                    (d "Input console settings saved."))
            ('RESTORE! (and settings
                            (begin
                              (system* stty settings)
                              (d "Console restored."))))
            ('RAW! (system* stty
                            "-echo" "-brkint" "-ignpar" "-inpck" "-istrip" "-inlcr"
                            "-igncr" ;; "-icrnl"
                            "-ixon" "-ixoff" "-ixany" "-imaxbel"
                            "-icanon" "-xcase" "min" "1" "time" "100")
                   (d "Input console commuted in raw mode."))
            (else (error "console")))))))

(define output-console
  (lambda (cells)
    "We use screen memory bus as a read-only output console."

    (d "Screen activated.")

    ;; The output bus is ignored, as we do not need to allow a Hack
    ;; program to read the screen memory.
    (define buses (vector-split cells '(16 1 13 16)))
    
    (define in    (car buses))
    (define lo@d  (vector-ref (cadr buses) 0))
    (define addr  (caddr buses))

    (define get/color (lambda () (bus->dec in)))
    (define get/load  (lambda () (cell/get/signal lo@d)))
    (define get/clk   (lambda () (cell/get/signal CLK)))
    (define get/addr  (lambda () (bus->dec addr)))

    (define check/color/use
      (lambda ()
        "A Hack program to print the character having ASCIICODE to the
output console may look like this:

    D=SOME/ASCII/CODE
    ...
    @SCREEN
    A=A+D
    M=1

As we do not consider colors as yet, we do not need more codes than 0
and 1 to write to the input bus of the output console.  In future we
could interpret IN=COLOR for any color that the terminal supports.  At
the present, we allow the Hack program to sent only color `1` on the
input bus.
"
        (or (= 1 (get/color))
            (begin
              '(d "bad use of output console" (get/color))
              false))))

    (define output!
      (lambda ()
        (let ((ASCII (get/addr)))
          (cond ((> ASCII 0)
                 (display (integer->char ASCII))
                 (flush-output))))))

    (define clock
      (lambda ()
        (and (get/clk)
             (get/load)
             (check/color/use)
             (output!))))

    ;; because the output console is connected to RAM we attach its
    ;; action on the CLOCK wire.
    (cell/add/action CLK (lambda _ _) propagation/delay/output/console
                     clock)
    'semper-veritas))
(define input-console
  (lambda (keybus)
    "Ex nihilo nihil fit.  This is the only function from all the
system whose associated HDL definition has no input but it has some
output.  The outputs are ascii characters of the pressed keys, not
coins.  Sine labore non erit panis in ore."
    (d "Keyboard activated.")

    ;; save current terminal settings
    (terminal 'SAVE!)
    
    ;; put the terminal input in RAW mode:
    (terminal 'RAW!)

    (define sendcode!
      (lambda (ASCII)
        (bus-set! keybus (dec->boolvec ASCII 16))))

    (define key-reader!
      (let ((resend-count 0)
            (CHASCII 0))
        (lambda ()
          "We send the same ASCII code a few times during consecutive
CLOCK TICKS (CLOCK ON), as we cannot surely know after how many
microcode instructions the keyboard's memory will be read by a Hack
program.  The smallest possible Hack program that can read the
keyboard's input has 3 instructions, like that:

  (LOOP)
    @KBD
    D=M     ## This instruction captures the key. 
    @LOOP

This is a canonical form for a GETCHAR function in Hack.

Taking into account that -- due to the structure of the datapath --
each instruction is executed in a single CLOCK TICK, in order to be
absolutely sure that this smallest code catches any pressed key, we
are going to resend the keycode during 3 consecutive CLOCK TICKS, such
that at any time when we press a key the `D=M` captures that key.

In order for the system to be logically sound, we suppose that the
GETCHAR function is provided inside the C library and the keyboard's
input will not be programmed by the user, as this could cause trouble
to the user who does not know the internals of the computer.

Note that a change in the datapath may affect the number of times we
need to resend the ascii code and this also involves reconsidering the
way we need to write the canonical Hack program when we want to write
a GETCHAR-like function -- but in the case of any change in HDL
equations, Hack will not be Hack any longer, but another computer,
with a possible different semantics for microcode as well.

TODO: if you set the RESEND-COUNT to 2 then the expected value of the
keys that are captured by the canonical Hack GETCHAR procedure should
be .666666 -- this is an _a priori_ statement.  TODO: Test this using
the ECHO program.
"
          (cond ((char-ready?)
                 (set! CHASCII (char->integer (read-char)))
                 ;; (d CHASCII)
                 (set! resend-count 3)
                 (sendcode! CHASCII))
                ((> resend-count 0)
                 (set! resend-count (sub1 resend-count))
                 (sendcode! CHASCII))
                ((zero? resend-count)
                 (set! resend-count (sub1 resend-count))
                 (sendcode! 0))
                (else
                 'nihil)))))

    (define clock
      (lambda ()
        (and (cell/get/signal CLK)
             (key-reader!))))

    ;; The best way to check for a pressed key is during the CLOCK
    ;; TICKS.
    (cell/add/action CLK (lambda _ _) propagation/delay/input/console
                     clock)
    'semper-veritas))

;;; The Hack microcode
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

(define program/length
  "Nihil ex nihilo")
(define control
  (lambda (microcode)
    (set! program/length (vector-length microcode))
    (set! control
          (lambda (program-counter)
            "another part of control comes from ALU's output"
            (and (< program-counter program/length)
                 (>= program-counter 0)
                 (vector-ref microcode program-counter))))))

;;; DATAPATH BUILDER
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

(define (eval id env)
  "add a computation element to the datapath"
  (cond
   ;; Special forms evaluation
   ((eq? 'Nand id) (Nand env))

   ;; *** CAUTION ***
   ;; 
   ;; If you expand all RAM as NANDs you need to have a lot of free
   ;; memory or swap space and very much pacience for execution.
   ;; 
   ;; ((eq? 'RAM8 id)   (ram8 env))
   ((eq? 'RAM16K id) (ram16k env))
   ;; ((eq? 'Register id) (ram1 env))

   ((eq? 'Add16 id)  (add16 env))
   ((eq? 'Inc16 id)  (inc16 env))
   
   ((eval-lisp-instrument id env))
   
   (else
    ;; Function evaluation
    (define-values (intern/len bc) (hs-abstractions id))
    (define intern (build-vector intern/len (lambda _ (new/cell))))
    (apply bc (vector-append env intern)))))

(define (apply bc args)
  "expand the datapath of some module"
  (define (get/w idx)
    (case idx
      ('GND GND)
      ('VDD VDD)
      ('CLK CLK)
      (else (vector-ref args idx))))
  ((lambda (s) (s s bc))
   (lambda (s bc)
     (and (pair? bc)
          (let* ((mod (caar bc))
                 (cells (cdar bc)))
            (eval mod (vector-map get/w cells))
            (s s (cdr bc)))))))

(define eval-lisp-instrument
  (lambda (id env)
    "execute actions associated with lisp instruments.  each
successful action must _always_ return a true value."
    (case id
      ('set-high            (set-high env))
      ('instrument-screen   (output-console env))
      ('instrument-keyboard (input-console env))
      ('instrument-rom      (rom env))
      ('instrument-reg-pc   (register-set! 'PC env))
      ('instrument-reg-D    (register-set! 'D  env))
      ('instrument-reg-A    (register-set! 'A  env))
      ('instrument-reg-M    (register-set! 'M  env))
      ;; ((eq? id 'debug-jmp) (test/jmp env))
      ;; ((eq? id 'dbg-reg)   (test/reg env))
      ;; ((eq? id 'dbg-alu)   (test/alu env))
      (else
       (and (lisp/instrument? id)
            (error "lisp instrument not defined" id))))))

;;; BUS HELPERS
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

(define bus->dec
  (lambda (bus)
    (o boolvec->dec bus->boolvec bus)))
(define bus->boolvec
  (lambda (bus)
    (vector-map cell/get/signal bus)))
(define bus->binvec
  (lambda (bus)
    (vector-map cell/get/signal/bin bus)))
(define bus-set!
  (lambda (bus v)
    (vector-map cell/set/signal! bus v)))
(define bus-add-action!
  (lambda (bus action reader propagation/delay)
    (vector-map (lambda (w) (cell/add/action
                        w
                        reader
                        propagation/delay
                        action))
                bus)))

;;; The FETCH-EVAL-WRITE processor's cycle
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

(define quit?
  (lambda ()
    "check for the HALT condition:
        (END)
 [PC-1] @END
 [PC]   0;JMP"
    (define jmp?
      (lambda (i)
        (and (vector-ref i 0)
             (vector-ref i 1)
             (vector-ref i 2))))
    (define a?
      (lambda (i)
        (false? (vector-ref i 15))))
    (define pc (@ 'PC))
    (define pc-1 (sub1 pc))
    (define prev/instr (control pc-1))
    (and prev/instr
         (a? prev/instr)
         (jmp? (control pc))
         (= pc-1 (boolvec->dec prev/instr)))))

(define tick-tock
  (lambda (loop trap)
    (cond ((quit?)
           (trap 'HALT))
          ((false? (control (@ 'PC)))
           (trap 'FINISHED))
          (else
           (tick)
           (tock)
           (loop)))))

;;; RESET AND GO
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

(define binary-microcode-file
  (let* ((args (current-command-line-arguments))
         (bin/file (and (= 1 (vector-length args))
                        (vector-ref args 0))))
    (cond ((and bin/file (file-exists? bin/file))
           (d "Load the microcode...")
           (list->vector
            (map
             (lambda (line)
               (list->vector
                (map
                 positive?
                 (cdr
                  (reverse
                   (cdr
                    (map
                     string->number
                     (string-split line ""))))))))
             (file->lines bin/file))))
          (else
           (d "racket sim.rkt FILE.HACK")
           (exit 1)))))

(define reset/computer
  (lambda (reset go fail)
    "Load the microcode, build the datapath, propagate the initial signal and go."
    (control binary-microcode-file)
    (d "Build the datapath...")
    (eval 'Computer (vector reset))
    (p " Reset...")
    (cell/set/signal! reset true)
    (tick)
    (tock)
    (cell/set/signal! reset false)
    (cond ((false? (SCHED?))
           (d "reset complete.")
           (d "The CLOCK is connected to" (counter/get 'clock) "wires.")
           (d "total" (counter/get 'nand) "nands.")
           (d "total" (counter/get 'cells) "wires.")
           (d "total" (/ (counter/get 'latches) 16 3) "registers expanded as nands.")
           (d "total" (counter/get 'cell/action) "actions installed.")
           (d "Starting the HACK microcode execution.")
           (p "--")
           (newline)
           (go))
          (else
           (fail)))))

(define tempus-fugit
  (lambda (s)
    "Tempus rerum imperator.  The heavenly machine is not a kind of
divine, live being, but a kind of clockwork -- Johannes Kepler."
    (tick-tock
     (lambda ()
       "tempora non numero nisi serena."
       (1+ 'tempus-serenus)
       (s s))
     terminus)))

(define terminus
  (lambda (trap . m)
    (newline)
    (p "--")
    (newline)
    (d trap)
    (for-each (lambda (a) (p " " a) (newline)) m)
    (d "fugit irreparabile tempus" (counter/get 'tempus-serenus))
    (terminal 'RESTORE!)
    (d "Noli turbare circulos meos.")))

(with-handlers
    ([exn:break? (lambda _ (terminus 'INTERRUPTED))])
  (reset/computer
   (new/cell)
   (lambda ()
     (tempus-fugit tempus-fugit))
   (lambda ()
     (terminus 'FAILED "loop in some combinational circuit."))))

