#!/usr/bin/csi -script
; Command-line program as an alternative to jqlang.org
(cond-expand
  (compiling
    (declare (uses util))
    (declare (uses json-query)))
  (else
    (load "./json-query.scm")))
(import json-query
        srfi-180
        srfi-13
        util
        vector-lib
        matchable
        getopt-long
        getopt-utils
        (chicken base)
        (chicken port)
        (chicken pretty-print)
        (chicken eval)
        (only srfi-193 command-line))

(usage-message "[options] <input-file>")
(define grammar
  '((filter "Filter file" (single-char #\f) (value #t))
    (help "Show help" (single-char #\h))))

(define args (getopt-long (command-line) grammar))
(when (opt-ref 'help args)
    (opt-usage grammar)
    (exit))
(define filter-file (opt-ref 'filter args ))
(define input-file (cadr (opt-ref '@ args)))

(module (jq-macros) (= jq je accessor prefix contains)
    (import (rename scheme (= base:=))
            (only srfi-13 string-prefix? string-contains))

    ; Functions for use in queries
    (define prefix string-prefix?)
    (define contains string-contains)

    (define-syntax =
        (syntax-rules ()
            ((= first arg ... )
             (cond
                 ((string? first) (string=? first arg ...))
                 ((number? first) (base:= first arg ...))
                 ((symbol? first) (eq? first arg ...))
                 (else (equal? first arg ...))))))

    (define-syntax jq
        (syntax-rules ()
            ((jq arg)
             (if (list? arg)
                 (json:query arg)
                 (json:query (list arg))))
            ((jq arg ...)
             (json:query (list arg ...)))))

    (define-syntax je
        (syntax-rules ()
            ((je arg)
             (if (list? arg)
                 (json:edit arg)
                 (json:edit (list arg))))
            ((je arg ...)
             (json:edit (list arg ...)))))

    (define-syntax accessor
        (syntax-rules ()
            ((accessor arg ...)
             (lambda args (json:query `(arg ... ,@args)))))))

(eval '(import (except scheme =)
               jq-macros
               json-query
               srfi-1
               srfi-13
               srfi-34
               srfi-180
               vector-lib
               util)
   (interaction-environment))

(define data
    (with-input-from-file input-file
        json-read))

(define filter
    (with-input-from-file filter-file
        read))

(define result
    ((eval `(jq ,filter) (interaction-environment))
     data))

(if (string? result)
    (display result)
    (pretty-print result))
