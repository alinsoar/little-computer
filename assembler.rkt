#lang racket ; -*- mode:racket ; buffer-read-only:t -*-

;;; Given some symbolic assembler generates microcode.

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

(require "hack-asm-phrase-struct.rkt")
(require "hack-asm-lexical.rkt")
(require "lexer.rkt")
(require "tools.rkt")

(void
 (or (= 2 (vector-length (current-command-line-arguments)))
     (error "racket assembler.rkt INFILE OUTDIR"
            (vector-length (current-command-line-arguments)))))

(define infile (vector-ref (current-command-line-arguments) 0))
(define outdir (vector-ref (current-command-line-arguments) 1))

(void
 (or (file-exists? infile)
     (begin
       (d "racket assembler.rkt INFILE OUTDIR")
       (error "in eror" infile)))

 (or (directory-exists? outdir)
     (begin
       (d "racket assembler.rkt INFILE OUTDIR")
       (error "out.error" outdir))))

(define in
  (regexp-match
   #rx"(.*)([.]asm)"
   (last (explode-path infile))))

(define filename
  (and in (second in)))

(define out/filename (~a outdir "./" filename ".hack"))

(define tokens
  (lex (string->list (file->string infile))
       :start-symbol
       hack-asm-lexical-environment))

(call-with-output-file out/filename
  #:mode 'text
  #:exists 'truncate
  (lambda (out)
    (p infile " => " out/filename "\n")
    (for-each
     (lambda (line)
       (or (string=? "" line)
           (begin (display line out)
                  (newline out))))
     (assembler tokens))))
