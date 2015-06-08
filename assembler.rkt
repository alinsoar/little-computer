#lang racket

(require parser-tools/lex)
(require parser-tools/yacc)
(require "tools.rkt")

(define-lex-abbrevs
  [letter (union lower-case upper-case)]
  [number (repetition 1 +inf.0 numeric)]
  [graph (union "_" "$" "." ":")]
  [id (concatenation
       letter
       (repetition 0 +inf.0 (union number letter graph)))]
  [comment (concatenation
            (union "#" "//")
            (repetition 0 +inf.0 (char-complement #\newline)))])

(define-empty-tokens 0tok
  (
   ;; dest
   ~M
   ~D
   ~MD
   ~A
   ~AM
   ~AD
   ~AMD
   ;; comp
   ~0   ~1   ~-1
   ~!D  ~!A  ~!M
   ~-D  ~-A  ~-M
   ~D+1 ~A+1 ~M+1
   ~D-1 ~A-1 ~M-1
   ~D+A ~D+M
   ~D-A ~D-M
   ~A-D ~M-D
   ~DandA ~DandM
   ~DorA  ~DorM
   ;; jmp
   ~JGT
   ~JEQ
   ~JGE
   ~JLT
   ~JNE
   ~JLE
   ~JMP
   ;; const
   ~SP
   ~LCL
   ~ARG
   ~THIS
   ~THAT
   ;; RAM[0..15]
   ~R0  ~R1  ~R2  ~R3
   ~R4  ~R5  ~R6  ~R7
   ~R8  ~R9  ~R10 ~R11
   ~R12 ~R13 ~R14 ~R15

   ~SCREEN
   ~KBD
   ;; graph
   colon
   semicolon
   comma
   openparen
   closeparen
   begin/def
   end/def
   equal
   at
   ;; the end
   eof))

(define-tokens 1tok
  (number id))

(define lex
  (lexer
   [comment       (lex input-port)]
   [whitespace    (lex input-port)]
   ["="           (token-equal)]
   ["@"           (token-at)]
   [";"           (token-semicolon)]
   ["("           (token-openparen)]
   [")"           (token-closeparen)]
   ;; DESTINATION
   ["M"           (token-~M)]
   ["D"           (token-~D)]
   ["MD"          (token-~MD)]
   ["A"           (token-~A)]
   ["AM"          (token-~AM)]
   ["AD"          (token-~AD)]
   ["AMD"         (token-~AMD)]
   ;; COMPUTATION that is not destination
   ["0"           (token-~0)]
   ["1"           (token-~1)]
   ["-1"          (token-~-1)]
   ["!D"          (token-~!D)]
   ["!A"          (token-~!A)]
   ["!M"          (token-~!M)]
   ["-D"          (token-~-D)]
   ["-A"          (token-~-A)]
   ["-M"          (token-~-M)]
   ["D+1"         (token-~D+1)]
   ["A+1"         (token-~A+1)]
   ["M+1"         (token-~M+1)]
   ["D-1"         (token-~D-1)]
   ["A-1"         (token-~A-1)]
   ["M-1"         (token-~M-1)]
   ["D+A"         (token-~D+A)]
   ["A+D"         (token-~D+A)]
   ["D+M"         (token-~D+M)]
   ["M+D"         (token-~D+M)]
   ["D-A"         (token-~D-A)]
   ["D-M"         (token-~D-M)]
   ["A-D"         (token-~A-D)]
   ["M-D"         (token-~M-D)]
   ["D&A"         (token-~DandA)]
   ["A&D"         (token-~DandA)]
   ["D&M"         (token-~DandM)]
   ["M&D"         (token-~DandM)]
   ["D|A"         (token-~DorA)]
   ["A|D"         (token-~DorA)]
   ["D|M"         (token-~DorM)]
   ["M|D"         (token-~DorM)]
   ;; JUMP
   ["JGT"         (token-~JGT)]
   ["JEQ"         (token-~JEQ)]
   ["JGE"         (token-~JGE)]
   ["JLT"         (token-~JLT)]
   ["JNE"         (token-~JNE)]
   ["JLE"         (token-~JLE)]
   ["JMP"         (token-~JMP)]
   [id            (token-id     (string->symbol lexeme))]
   [number        (token-number (string->number lexeme))]
   [(eof)         (token-eof)]))

(define parse-error-handler
  (lambda (tok-ok? tok-name tok-value)
    (newline)
    (display (~a "\nPARSING ERROR: " tok-ok?
                 "\nTOKEN NAME\t"  tok-name
                 "\nTOKEN VALUE\t" tok-value))
    (newline)
    (exit 1)))

(define (a address)
  (1+ 'instruction/address)
  (if (number? address)
      (cons 0 (reverse (dec->bin 15 address)))
      address))
(define (c dest comp jump)
  (1+ 'instruction/address)
  (append '(1 1 1) dest comp jump))
(define (l sym)
  (and (hash-has-key? s-table sym)
       (error "Symbol already defined." sym))
  (hash-set! s-table sym (counter/get 'instruction/address))
  '())

(define bison
  (parser
   (start BIN)
   (end eof)
   (error parse-error-handler)
   (tokens 0tok 1tok)
   (grammar
    [BIN           ({ASM_CODE}  $1)]
    [ASM_CODE      ({INSTRUCTION} (list $1))
                   ({INSTRUCTION ASM_CODE} (cons $1 $2))]
    [INSTRUCTION   ({A-INSTRUCTION} $1)
                   ({C-INSTRUCTION} $1)
                   ({L-INSTRUCTION} $1)]
    [A-INSTRUCTION ({at id}                                        (a $2))
                   ({at number}                                    (a $2))
                   ({at ~0}                                        (a 0))
                   ({at ~1}                                        (a 1))]
    [C-INSTRUCTION ({DESTINATION equal COMPUTATION semicolon JUMP} (c $3 $1 $5))
                   ({DESTINATION equal COMPUTATION}                (c $3 $1 '(0 0 0)))
                   ({COMPUTATION semicolon JUMP}                   (c $1 '(0 0 0) $3))]
    [L-INSTRUCTION ({openparen id closeparen}                      (l $2))]
    [JUMP          ({~JGT}   '(0 0 1))
                   ({~JEQ}   '(0 1 0))
                   ({~JGE}   '(0 1 1))
                   ({~JLT}   '(1 0 0))
                   ({~JNE}   '(1 0 1))
                   ({~JLE}   '(1 1 0))
                   ({~JMP}   '(1 1 1))]
    [DESTINATION   ({~M}     '(0 0 1))
                   ({~D}     '(0 1 0))
                   ({~MD}    '(0 1 1))
                   ({~A}     '(1 0 0))
                   ({~AM}    '(1 0 1))
                   ({~AD}    '(1 1 0))
                   ({~AMD}   '(1 1 1))]
    [COMPUTATION   ({~D}     '(0   0 0 1 1 0 0))
                   ({~M}     '(1   1 1 0 0 0 0))
                   ({~A}     '(0   1 1 0 0 0 0))
                   ({~0}     '(0   1 0 1 0 1 0))
                   ({~1}     '(0   1 1 1 1 1 1))
                   ({~-1}    '(0   1 1 1 0 1 0))
                   ({~!D}    '(0   0 0 1 1 0 1))
                   ({~!A}    '(0   1 1 0 0 0 1))
                   ({~!M}    '(1   1 1 0 0 0 1))
                   ({~-D}    '(0   0 0 1 1 1 1))
                   ({~-A}    '(0   1 1 0 0 1 1))
                   ({~-M}    '(1   1 1 0 0 1 1))
                   ({~D+1}   '(0   0 1 1 1 1 1))
                   ({~A+1}   '(0   1 1 0 1 1 1))
                   ({~M+1}   '(1   1 1 0 1 1 1))
                   ({~D-1}   '(0   0 0 1 1 1 0))
                   ({~A-1}   '(0   1 1 0 0 1 0))
                   ({~M-1}   '(1   1 1 0 0 1 0))
                   ({~D+A}   '(0   0 0 0 0 1 0))
                   ({~D+M}   '(1   0 0 0 0 1 0))
                   ({~D-A}   '(0   0 1 0 0 1 1))
                   ({~D-M}   '(1   0 1 0 0 1 1))
                   ({~A-D}   '(0   0 0 0 1 1 1))
                   ({~M-D}   '(1   0 0 0 1 1 1))
                   ({~DandA} '(0   0 0 0 0 0 0))
                   ({~DandM} '(1   0 0 0 0 0 0))
                   ({~DorA}  '(0   0 1 0 1 0 1))
                   ({~DorM}  '(1   0 1 0 1 0 1))])))

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

(define filename
  (second (regexp-match #rx"(.*)([.]asm)"
                        (last (explode-path
                               (vector-ref (current-command-line-arguments) 0))))))

(define in/filename
  (~a "./asm/" filename ".asm"))
(define out/filename
  (~a "./bin/" filename ".hack"))

(counter/reset 'variable-address #x10)

(d in/filename "=>" out/filename)

(call-with-output-file out/filename
  #:mode 'text #:exists 'truncate
  (lambda (out)
    (for-each
     (lambda (line)
       (or (string=? "" line)
           (begin (display line out)
                  (newline out))))
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
                                      (1+ 'variable-address)
                                      new/var/addr)
                                    (counter/get 'variable-address)))))))
             i))
       (call-with-input-file in/filename
         (lambda (in)
           (bison (lambda ()
                    (define tok (lex in))
                    ;; (d tok)
                    tok)))))))))
