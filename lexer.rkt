#lang racket

(require parser-tools/lex)

(define-lex-abbrevs
  [letter (union lower-case upper-case number)]
  [number (repetition 1 +inf.0 numeric)]
  [id (repetition 0 +inf.0 letter)]
  [restofline (repetition 0 +inf.0 (char-complement #\newline))]
  [insideccomment (complement (concatenation any-string "*/" any-string))]
  [cpp/comment (concatenation "//" restofline)]
  [c/comment (concatenation "/*" insideccomment "*/")]
  [lisp-function (concatenation lower-case (repetition 1 +inf.0 (union alphabetic punctuation)))]
  [instrument (concatenation "@" lisp-function)]
  ;;
  [gateID (concatenation upper-case id)]
  [wireID (concatenation lower-case id)]
  [subbusID (concatenation wireID "[" number ".." number "]")]
  [bus/pinID (concatenation wireID "[" number "]")]
  [comment (union c/comment cpp/comment)])
(define-empty-tokens 0tok
  (def/gate
   input/wires
   output/wires
   define/structs
   colon
   semicolon
   comma
   openparen
   closeparen
   begin/def
   end/def
   equal
   eof))
(define-tokens 1tok
  (gateID
   wireID
   subbusID
   bus/pinID
   vdd
   gnd
   clk
   instrument))

(define get-bus/pinid
  (lambda (lexeme pos)
    (let ((w (regexp-split #rx"[][]" lexeme)))
      (list (string->symbol (first w))
            (string->number (second w))
            pos))))
(define get-subbusid
  (lambda (lexeme pos file)
    (let ((w (regexp-split #rx"[][.]" lexeme)))
      (when (> (string->number (second w))
               (string->number (fourth w)))
        (display file)
        (display
         (~a "Left index in a subbus must be less or equal to the right index.\n"
             pos))
        (exit 1))
      (list (string->symbol (first w))
            (string->number (second w))
            (string->number (fourth w))
            pos))))
(define get-id
  (lambda (id pos)
    (list (string->symbol id) pos)))
(define get-instrument-id
  (lambda (id pos)
    (list (string->symbol (substring id 1)) pos)))

(define lex
  (lexer-src-pos
   [comment       (return-without-pos (lex input-port))]
   [whitespace    (return-without-pos (lex input-port))]
   ["CHIP"        (token-def/gate)]
   ["IN"          (token-input/wires)]
   ["OUT"         (token-output/wires)]
   ["PARTS"       (token-define/structs)]
   [":"           (token-colon)]
   [";"           (token-semicolon)]
   [","           (token-comma)]
   ["{"           (token-begin/def)]
   ["}"           (token-end/def)]
   ["("           (token-openparen)]
   [")"           (token-closeparen)]
   ["="           (token-equal)]
   ["true"        (token-vdd        (list start-pos))]
   ["false"       (token-gnd        (list start-pos))]
   ["clock"       (token-clk        (list start-pos))]
   [instrument    (token-instrument (get-instrument-id lexeme start-pos))]
   [wireID        (token-wireID     (get-id            lexeme start-pos))]
   [gateID        (token-gateID     (get-id            lexeme start-pos))]
   [subbusID      (token-subbusID   (get-subbusid      lexeme start-pos input-port))]
   [bus/pinID     (token-bus/pinID  (get-bus/pinid     lexeme start-pos))]
   [(eof)         (token-eof)]))

(module+ test
  (define debug (lambda (input)
                  (let ((w (lex input)))
                    (let ((tok (position-token-token w)))
                     (or
                      (void? tok)
                      (begin
                        (display (~a "##: " tok))
                        (newline)
                        (debug input)))))))
  (void (debug (open-input-file "CPU.hdl"))))

(provide (rename-out (lex hdl/lex)
                     (0tok hdl/0tok)
                     (1tok hdl/1tok)))
