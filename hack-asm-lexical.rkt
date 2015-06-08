#lang racket ; -*- mode:racket ; buffer-read-only:t -*-

;;; Lexical grammar of the Hack assembler files.

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

(define :destination
  "M|D|MD|A|AM|AD|AMD")

(define :digit
  '(UNION #\0 nonzero-digit))

(define :nonzero-digit
  "[1-9]")

(define :constant
  'integer-constant)

(define :integer-constant
  '(UNION
    (CONCAT nonzero-digit
            (REPEAT 0 INF digit))))

(define :whitespace
  '(UNION #\tab #\space #\newline #\return))

(define :punctuator
  "@|=|;|[(]|[)]")

(define :nondigit
  "[a-zA-Z_$.:]")

(define :id
  '(CONCAT
    nondigit
    (REPEAT 0 INF (UNION nondigit digit))))

(define :comment
  '(CONCAT  "//|##|;;|%%" (REPEAT 0 INF (COMPLEMENT #\newline))))

(define :computation
  '(UNION
    "0|1|-1"
    "!D|!A|!M"
    "-D|-A|-M"
    "D+1|A+1|M+1"
    "D-1|A-1|M-1"
    "D+A|A+D|D+M|M+D"
    "D-A|D-M|A-D|M-D"
    "D&A|A&D|D&M|M&D"
    "D[|]A|A[|]D|D[|]M|M[|]D"))

(define :jump
  "JGT|JEQ|JGE|JLT|JNE|JLE|JMP")

(define :start-symbol
  '(SELECT
    [destination    @keyword]
    [jump           @keyword]
    [computation    @keyword]
    [whitespace     @blank]
    [punctuator     @punct]
    [id             @id]
    [constant       @const]
    [comment        @comment]))

(define hack-asm-lexical-environment
  (list (cons 'punctuator           :punctuator)
        (cons 'id                   :id)

        ;; ;; CONSTANTS
        (cons 'integer-constant     :integer-constant)
        (cons 'constant             :constant)
        (cons 'nonzero-digit        :nonzero-digit)
        (cons 'digit                :digit)

        ;; LETTERS
        (cons 'nondigit             :nondigit)
        (cons 'destination          :destination)
        (cons 'computation          :computation)
        (cons 'jump                 :jump)

        ;; ;; WHITESPACES
        (cons 'whitespace           :whitespace)

        ;; Hack assembler has CPP-like comment
        (cons 'comment              :comment)))


(provide :start-symbol hack-asm-lexical-environment)
