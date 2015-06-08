#lang racket

;;; Phrase structure grammar for the Hack assembler files.

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


;;; PHRASE STRUCTURE GRAMMAR OF THE HACK PROGRAMMAING LANGUAGE
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

(define ::tok/op
  (lambda (ID op)
    (lambda (TOK s f)
      ((if (eq? ID (op TOK))
           s
           f)))))

(define ::type/check
  (lambda (type)
    (::tok/op type tok-type)))

(define ::type/id/check
  (lambda (type id)
    (define type? (::tok/op type tok-type))
    (define id? (::tok/op id tok-val))
    (lambda (TOK s f)
      (type? TOK
             (lambda () (id? TOK s f))
             f))))

(define ::id?
  (::type/check 'id))

(define ::key?
  (::type/check 'key))

(define ::zero?
  (::type/id/check 'key '|0|))

(define ::one?
  (::type/id/check 'key '|1|))

(define ::number?
  (lambda (TOK s f)
    (define num? (::type/check 'const))
    (num? TOK
          s
          (lambda ()
            (::zero? TOK
                     s
                     (lambda ()
                       (::one? TOK
                               s
                               f)))))))

(define ::at?
  (::type/id/check 'punct '|@|))

(define ::openparen?
  (::type/id/check 'punct '|(|))

(define ::closeparen?
  (::type/id/check 'punct '|)|))

(define ::equal?
  (::type/id/check 'punct '|=|))

(define ::semicolon?
  (::type/id/check 'punct '|;|))

(define a
  (lambda (stream)
    "A-instruction"
    (let ((address (tok-val (stream 'GET 1))))
      (1+ 'instruction/address)
      (stream 'PUSH 2)
      (cond ((number? address)  (cons 0 (reverse (dec->bin 15 address))))
            ((eq? address '|0|) (cons 0 (reverse (dec->bin 15 0))))
            ((eq? address '|1|) (cons 0 (reverse (dec->bin 15 1))))
            (else address)))))

(define l
  (lambda (stream)
    "L-instruction"
    (let ((sym (tok-val (stream 'GET 1))))
      (and (hash-has-key? s-table sym)
           (error "Symbol already defined." sym))
      (hash-set! s-table sym (counter/get 'instruction/address))
      (stream 'PUSH 3))))

(define c
  (lambda (comp dest jump C-INSTR/LEN stream)
    "C-instruction"
    (1+ 'instruction/address)
    (stream 'PUSH C-INSTR/LEN)
    (append '(1 1 1) comp dest jump)))

(define ~~dcj?
  (lambda (L)
    (lambda (TOK s f)
      (::key? TOK
              (lambda () ((if (memq (tok-val TOK) L) s f)))
              f))))

(define DandA (string->symbol "D&A"))
(define DandM (string->symbol "D&M"))
(define DorA  (string->symbol "D|A"))
(define DorM  (string->symbol "D|M"))
(define Xop?
  (lambda (op TOK)
    (eq? op (tok-val TOK))))

(define ~~computation?
  (~~dcj?
   `(D M A
     |0| |1| |-1|
     !D !A !M
     -D -A -M
     D+1 A+1 M+1 D-1 A-1 M-1
     A+D D+A M+D D+M D-A D-M A-D M-D
     ,DandA ,DandM ,DorA ,DorM)))

(define computation
  (lambda (TOK s f)
    (~~computation? TOK
                    (lambda ()
                      (cond
                       ((Xop? DandA TOK) (s '(0   0 0 0 0 0 0)))
                       ((Xop? DandM TOK) (s '(1   0 0 0 0 0 0)))
                       ((Xop? DorA  TOK) (s '(0   0 1 0 1 0 1)))
                       ((Xop? DorM  TOK) (s '(1   0 1 0 1 0 1)))
                       (else
                        [case (tok-val TOK)
                          ({D}     (s '(0   0 0 1 1 0 0)))
                          ({M}     (s '(1   1 1 0 0 0 0)))
                          ({A}     (s '(0   1 1 0 0 0 0)))
                          ('|0|    (s '(0   1 0 1 0 1 0)))
                          ('|1|    (s '(0   1 1 1 1 1 1)))
                          ('|-1|   (s '(0   1 1 1 0 1 0)))
                          ({!D}    (s '(0   0 0 1 1 0 1)))
                          ({!A}    (s '(0   1 1 0 0 0 1)))
                          ({!M}    (s '(1   1 1 0 0 0 1)))
                          ({-D}    (s '(0   0 0 1 1 1 1)))
                          ({-A}    (s '(0   1 1 0 0 1 1)))
                          ({-M}    (s '(1   1 1 0 0 1 1)))
                          ({D+1}   (s '(0   0 1 1 1 1 1)))
                          ({A+1}   (s '(0   1 1 0 1 1 1)))
                          ({M+1}   (s '(1   1 1 0 1 1 1)))
                          ({D-1}   (s '(0   0 0 1 1 1 0)))
                          ({A-1}   (s '(0   1 1 0 0 1 0)))
                          ({M-1}   (s '(1   1 1 0 0 1 0)))
                          ({D+A}   (s '(0   0 0 0 0 1 0)))
                          ({A+D}   (s '(0   0 0 0 0 1 0)))
                          ({D+M}   (s '(1   0 0 0 0 1 0)))
                          ({M+D}   (s '(1   0 0 0 0 1 0)))
                          ({D-A}   (s '(0   0 1 0 0 1 1)))
                          ({D-M}   (s '(1   0 1 0 0 1 1)))
                          ({A-D}   (s '(0   0 0 0 1 1 1)))
                          ({M-D}   (s '(1   0 0 0 1 1 1)))])))
                    f)))

(define ~~jump?
  (~~dcj?
   '(JGT JEQ JGE JLT JNE JLE JMP)))

(define jump
  (lambda (TOK s f)
    (~~jump? TOK
             (lambda ()
               [case (tok-val TOK)
                 ({JGT}   (s '(0 0 1)))
                 ({JEQ}   (s '(0 1 0)))
                 ({JGE}   (s '(0 1 1)))
                 ({JLT}   (s '(1 0 0)))
                 ({JNE}   (s '(1 0 1)))
                 ({JLE}   (s '(1 1 0)))
                 ({JMP}   (s '(1 1 1)))])
             f)))

(define ~~destination?
  (~~dcj?
   '(M D MD A AM AD AMD)))

(define destination
  (lambda (stream s f)
    (~~destination? (stream 'GET 0)
                    (lambda ()
                      [case (tok-val (stream 'GET 0))
                        ({M}     (s '(0 0 1)))
                        ({D}     (s '(0 1 0)))
                        ({MD}    (s '(0 1 1)))
                        ({A}     (s '(1 0 0)))
                        ({AM}    (s '(1 0 1)))
                        ({AD}    (s '(1 1 0)))
                        ({AMD}   (s '(1 1 1)))])
                    f)))

(define ~~l-instruction?
  (lambda (stream s f)
    (::openparen? (stream 'GET 0)
                  (lambda ()
                    (::id? (stream 'GET 1)
                           (lambda ()
                             (::closeparen? (stream 'GET 2) s f))
                           f))
                  f)))

(define l-instruction
  (lambda (stream s f)
    (~~l-instruction? stream
                      (lambda ()
                        (s (l stream)))
                      f)))

(define ~~a-instruction?
  (lambda (stream s f)
    (::at? (stream 'GET 0)
           (lambda ()
             (::id? (stream 'GET 1)
                    s
                    (lambda ()
                      (::number? (stream 'GET 1)
                                 s
                                 f))))
           f)))

(define a-instruction
  (lambda (stream s f)
    (~~a-instruction? stream
                      (lambda ()
                        (s (a stream)))
                      f)))

(define c-instruction
  (lambda (stream s f)

    (define JMP
      (lambda (N COMP DEST)
        (lambda ()
          (jump (stream 'GET N)
                (lambda (JMP)
                  (s (c COMP DEST JMP (add1 N) stream)))
                f))))
    
    (define COMP-JMP
      (lambda ()
        (computation (stream 'GET 0)
                     (lambda (COMP)
                       (::semicolon? (stream 'GET 1)
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ COMP ; JMP
                                     (JMP 2 COMP '(0 0 0))
                                     f))
                     f)))
    
    (destination
     stream
     (lambda (DEST)
       (::equal? (stream 'GET 1)
                 (lambda ()
                   (computation
                    (stream 'GET 2)
                    (lambda (COMP)
                      (::semicolon? (stream 'GET 3)
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DST = COMP ; JMP
                                    (JMP 4 COMP DEST)
                                    (lambda ()
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DST = COMP
                                      (s (c COMP DEST '(0 0 0)
                                            3
                                            stream)))))
                    f))
                 COMP-JMP))
     COMP-JMP)))

(define s-table (make-hash
                 '((SP     . 0)
                   (LCL    . 1)
                   (ARG    . 2)
                   (THIS   . 3)
                   (THAT   . 4)
                   (R0     . #x0)
                   (R1     . #x1)
                   (R2     . #x2)
                   (R3     . #x3)
                   (R4     . #x4)
                   (R5     . #x5)
                   (R6     . #x6)
                   (R7     . #x7)
                   (R8     . #x8)
                   (R9     . #x9)
                   (R10    . #xa)
                   (R11    . #xb)
                   (R12    . #xc)
                   (R13    . #xd)
                   (R14    . #xe)
                   (R15    . #xf)
                   (SCREEN . #x4000)
                   (KBD    . #x6000))))

(define linker
  (lambda (micro0)
    (counter/reset 'variable-address #x10)
    (map
     strcat
     (map
      (lambda (i)
        (if (symbol? i)
            (cons 0 (reverse
                     (dec->bin
                      15
                      (hash-ref s-table i
                                (lambda ()
                                  ((lambda (new/var/addr)
                                     (hash-set! s-table i new/var/addr)
                                     new/var/addr)
                                   (sub1 (1+ 'variable-address))))))))
            i))
      micro0))))

;;; HACK DISASSEMBLER
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

(define disassembler
  (lambda (in)
    "Disassemble binary hack programs."
    (define i (rev-vector in))
    (define a/instr
      (lambda ()
        (~a "A = " (binvec->dec in))))
    (define c/instr
      (lambda ()
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
            (if (zero? a)
                (case c
                  ((63) "1")
                  ((58) "-1")
                  ((55) "A+1")
                  ((51) "-A")
                  ((50) "A-1")
                  ((49) "!A")
                  ((48) "A")
                  ((42) "0")
                  ((31) "D+1")
                  ((21) "A|D")
                  ((19) "D-A")
                  ((14) "D-1")
                  ((13) "!D")
                  ((12) "D")
                  ((7)  "A-D")
                  ((2)  "D+A")
                  ((0)  "D&A")
                  (else (~a "a=0;..." c)))
                (case c
                  ((55) "M+1")
                  ((51) "-M")
                  ((50) "M-1")
                  ((49) "!M")
                  ((48) "M")
                  ((21) "M|D")
                  ((19) "D-M")
                  ((7)  "M-D")
                  ((2)  "D+M")
                  ((0)  "M&D")
                  (else (~a "a=1;..." c))))))
        (define jmp
          (let* ((j< (vector-ref i 13))
                 (j= (vector-ref i 14))
                 (j> (vector-ref i 15))
                 (count (+ j< j= j>))
                 (str (if (positive? count)
                          (~a (if (zero? j<) "" "<")
                              (if (zero? j>) "" ">")
                              (if (zero? j=) "" "="))
                          false)))
            (cond
             ((= count 3) "jmp A")
             (str (~a "if " comp " " str " 0 then goto A"))
             (else false))))
        (cond ((and jmp (not dest))
               ;; COMPUTATION ; JUMP
               jmp)
              ((not jmp)
               ;; DESTINATION = COMPUTATION
               (~a dest "=" comp))
              (else
               ;; DESTINATION = COMPUTATION ; JUMP
               (~a dest "=" comp "; " jmp)))))
    (if (zero? (vector-ref i 0))
        (a/instr)
        (c/instr))))

;;; HACK ASSEMBLER
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=


(define assembler
  (lambda (tokens)
    (define unit
      (lambda (x)
        (lambda (k)
          (k x))))

    (define >>=
      (lambda (ma seq)
        (lambda (k)
          (ma (lambda (a)
                ((seq a) k))))))

    ((lambda (u) ((u u) linker))
     (lambda (u)
       (if (tokens 'NULL?)
           (unit '())
           (a-instruction
            tokens
            (lambda (x)
              (>>= (u u)
                   (lambda (r)
                     (unit (cons x r)))))
            (lambda ()
              (c-instruction
               tokens
               (lambda (x)
                 (>>= (u u)
                      (lambda (r)
                        (unit (cons x r)))))
               (lambda ()
                 (l-instruction
                  tokens
                  (lambda _
                    (>>= (u u) unit))
                  (lambda ()
                    (d "FAIL --" (tokens 'FAIL)))))))))))))


(provide assembler disassembler)

