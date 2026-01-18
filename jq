#!/usr/bin/scheme --script

(import (json-query)
        (srfi-180)
        (debug)
        (chezscheme))

(define jq json:query)

(define-record-type args (fields (mutable filter) (mutable input)))

(define (parse-command-line command-line args)
    (cond
        ((string=? (car command-line) "-f")
         (begin
             (args-filter-set! args (cadr command-line))
             (parse-command-line (cddr command-line) args)))
        (else (args-input-set! args (car command-line))))
    args)

(define args (parse-command-line (cdr (command-line))
                                 (make-args "" "")))

(define data 
    (with-input-from-file  (args-input args)
        json-read))

(define filter
    (with-input-from-file (args-filter args)
        read))

(define result 
    ((eval `(jq ,filter))
     data))

(if (string? result)
    (display result)
    (pretty-print result))
