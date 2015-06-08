#lang racket

(require parser-tools/lex)
(require parser-tools/yacc)
(require "lexer.rkt")
(require "tools.rkt")

(define DEBUG/PARSER false)

(define debug/parsing
  (lambda (x)
    (and DEBUG/PARSER
         (begin
           (display (~a "Parsing " x "."))
           (newline)))))

(define mk/io/any
  (lambda (data)
    (list (second data) 1 (third data))))
(define mk/io/bus
  (lambda (data)
    (rest data)))

(define mk/left/right/value
  (lambda (data)
    (case (ast/sym/type data)
      ('B (list 'S
                (ast/sym/name data)
                (ast/sym/p1 data)
                (ast/sym/p1 data)
                (ast/sym/dbg data)))
      (else
       ;; VDD  GND ANY SUBBUS
       data))))

(define mk/def
  (lambda (header body)
    ;; (display (~a ">>" header))
    ;; (newline)
    ;; (display (~a ">>" body))
    ;; (newline)
    (append header (map car body))
    ;;(cons header body)
    ))

(define mk/ast
  (lambda (gate def)
    (cons gate def)))
(define get/ast
  (lambda (file)
    (debug/parsing file)
    (let ((i (open-input-file file)))
      (port-count-lines! i)
      (bison (lambda () (hdl/lex i))))))
(define ast/name   first)
(define ast/input  second)
(define ast/output third)
(define ast/body   fourth)

(define parse-error-handler
  (lambda (tok-ok? tok-name tok-value start-pos end-pos)
    (newline)
    (display (~a "\nPARSING ERROR: " tok-ok?   " "
                 "\nTOKEN NAME\t"  tok-name    " "
                 "\nTOKEN VALUE\t" tok-value   " "
                 "\nBEGIN { "
                 "POINT "  (position-offset start-pos) ";"
                 "\tLINE " (position-line start-pos) ";"
                 "\tCOL "  (position-col start-pos)    ";\t}"
                 "\nEND   { "
                 "POINT "  (position-offset end-pos) ";"
                 "\tLINE " (position-line end-pos) ";"
                 "\tCOL "  (position-col end-pos) ";\t}"))
    (newline)
    (exit 1)))

(define bison
  (parser
   (start NEWGATE)
   (end eof)
   (src-pos)
   (error parse-error-handler)
   (tokens hdl/0tok hdl/1tok)
   (grammar
    ;; GATE ID
    [NEWGATE      ({def/gate GATE begin/def DEF end/def}  (mk/ast $2 $4))]
    [DEF          ({HEADER BODY}                          (mk/def $1 $2))]
    ;; I/O HEADER
    [HEADER       ({INPUT OUTPUT}                         (list $1 $2))]
    [INPUT        ({input/wires   BUS/PIN/IO semicolon}   $2)
                  ({} '())]
    [OUTPUT       ({output/wires  BUS/PIN/IO semicolon}   $2)
                  ({} '())]
    [BUS/PIN/IO   ({I/O}                                  (list $1))
                  ({I/O comma BUS/PIN/IO}                 (cons $1 $3))]
    ;; BODY STRUCTURES
    [BODY         ({define/structs colon STRUCTURES}      (apply list $3))]
    [STRUCTURES   ({STRUCT semicolon}                     (list $1))
                  ({STRUCT semicolon STRUCTURES}          (cons $1 $3))]
    [STRUCT       ({gateID openparen A/LIST closeparen}   (list (cons $1 $3)))
                  ({INSTRUMENT LISP/ARGUMENTS}            (list (cons $1 $2)))]
    [A/LIST       ({ASSIGNMENT}                           (list $1))
                  ({ASSIGNMENT comma A/LIST}              (cons $1 $3))]
    [ASSIGNMENT   ({LEFTVAL equal RIGHTVAL}               (ast/mk-assign $1 $3))]
    ;; LISP INSTRUMENTATION
    [LISP/ARGUMENTS ({PIN} (list $1))
                    ({PIN comma LISP/ARGUMENTS}           (cons $1 $3))]
    [PIN          ({BUSPIN}      (mk/left/right/value $1))
                  ({ANY}         (mk/left/right/value $1))]
    ;; PINS and BUSSES IDs
    [I/O          ({BUSPIN}      (mk/io/bus $1))
                  ({ANY}         (mk/io/any $1))]
    ;; ASSIGNMENTS ADDRESSES AND VALUES
    [LEFTVAL      ({ANY}         (mk/left/right/value $1))
                  ({BUSPIN}      (mk/left/right/value $1))
                  ({SUBBUS}      (mk/left/right/value $1))]
    [RIGHTVAL     ({LEFTVAL}     (mk/left/right/value $1))
                  ({VDD}         (mk/left/right/value $1))
                  ({GND}         (mk/left/right/value $1))
                  ({CLK}         (mk/left/right/value $1))]
    ;; ATOMIC
    [ANY          ({wireID}      (mk/sym 'A   $1))] ; wire OR bus OR bus/pin
    [BUSPIN       ({bus/pinID}   (mk/sym 'B   $1))] ; bus/pin
    [SUBBUS       ({subbusID}    (mk/sym 'S   $1))] ; subbus
    [VDD          ({vdd}         (mk/sym 'VDD $1))] ; constant 1
    [GND          ({gnd}         (mk/sym 'GND $1))] ; constant 0
    [CLK          ({clk}         (mk/sym 'CLK $1))] ; system clock
    [GATE         ({gateID}      $1)]
    [INSTRUMENT   ({instrument}  (mk/sym 'i   $1))]))) ; gate symbol

(define mk/sym       cons)
(define ast/sym/type first)
(define ast/sym/name second)
(define ast/sym/p1   third)
(define ast/sym/p2   fourth)
(define ast/sym/dbg  last)

(define ast/mk-assign  (lambda (a b) (list a b)))
(define ast/leftvalue  first)
(define ast/rightvalue second)

(module+ test
  (define (test/ file)
    (ast/print (get/ast file))
    'done)
  ;; (test/ "Add16.hdl")
  ;; (test/ "ALU.hdl")
  ;; (test/ "And16.hdl")
  ;; (test/ "And.hdl")
  ;; (test/ "Bit.hdl")
  ;; (test/ "CLA16.hdl")
  ;; (test/ "Computer.hdl")
  ;; (test/ "CondNeg16.hdl")
  (test/ "CPU.hdl")
  ;; (test/ "DMux4Way.hdl")
  ;; (test/ "DMux8Way.hdl")
  ;; (test/ "DMux.hdl")
  ;; (test/ "FullAdder.hdl")
  ;; (test/ "HalfAdder.hdl")
  ;; (test/ "Inc16.hdl")
  ;; (test/ "Memory.hdl")
  ;; (test/ "Mux16.hdl")
  ;; (test/ "Mux4Way16.hdl")
  ;; (test/ "Mux8Way16.hdl")
  ;; (test/ "Mux.hdl")
  ;; (test/ "Not16.hdl")
  ;; (test/ "Not.hdl")
  ;; (test/ "Or16.hdl")
  ;; (test/ "Or8Way.hdl")
  ;; (test/ "Or.hdl")
  ;; (test/ "PC.hdl")
  ;; (test/ "RAM16K.hdl")
  ;; (test/ "RAM4K.hdl")
  ;; (test/ "RAM512.hdl")
  ;; (test/ "RAM64.hdl")
  ;; (test/ "RAM8.hdl")
  ;; (test/ "Register.hdl")
  ;; (test/ "Xor16.hdl")
  ;; (test/ "Xor.hdl")
  ;; (test/ "Zero16.hdl")
  )

(define (str/w w)
  (if (pair? w)
      (take w (sub1 (length w)))
      w))

(define (ast/print-assign l)
  (define (str/wire pos w)
    (~a (str/w w)
        #:pad-string " "
        #:width 20
        #:align pos))
  (cond ((empty? l) 'done)
        (else (display (~a "        "
                           (str/wire 'left (caar l))
                           " <- "
                           (str/w (cadar l))
                           "\n"))
              (ast/print-assign (cdr l)))))

(define (ast/print/io l)
  (cond ((empty? l) 'done)
        (else (display (~a "----" (str/w (car l))))
              (newline)
              (ast/print/io (cdr l)))))

(define (ast/print/body l)
  (cond ((empty? l) 'done)
        (else (display (~a ":: " (str/w (caar l)) "\n"))
              (ast/print-assign (cdar l))
              (ast/print/body (cdr l)))))

(define (ast/print/with/title t f data)
  (define (str/title w)
    (let ((width 50))
      (~a (~a w #:pad-string " " #:width width #:align 'right)
          "\n"
          (make-string width #\*)
          "\n")))
  (display (str/title t))
  (f data))

(define (ast/print name input output body)
  (ast/print/with/title name       (lambda (_) 'void) 'void)
  (ast/print/with/title "**INPUT"  ast/print/io input)
  (ast/print/with/title "**OUTPUT" ast/print/io output)
  (ast/print/with/title "**BODY"   ast/print/body body)
  )

(provide get/ast
         ast/name
         ast/input
         ast/output
         ast/body
         ast/leftvalue
         ast/rightvalue
         ast/sym/type
         ast/sym/name
         ast/sym/p1
         ast/sym/p2
         ast/print
         ast/print/io
         ast/print/body
         ast/print/with/title)
