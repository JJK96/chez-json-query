#!r6rs
(library (json-query) 
     (export json:query
             json:ref
             json:keys
             json:values
             json:flatten
             json:unique
             ->
             vector-filter
             debug)
     (import (rnrs)
             (srfi-180)
             (only (srfi :1) delete-duplicates)
             (only (chezscheme) format eval vector-append))

     (define-syntax ->
         (syntax-rules ()
            ((_ init func ...)
             (fold-left (lambda (val f) (f val))
                        init
                        (list func ...)))))
     (define (vector-filter func v)
         (-> v
             vector->list
             (lambda (x) (filter func x))
             list->vector))

     (define (displayln . x)
         (map
          (lambda (x)
             (display x)
             (display "\n"))
          x))

     (define (debug x)
         (displayln x)
         x)

     (define (json:ref key)
         (let ((key (if (string? key) 
                        (string->symbol key)
                        key)))
             (lambda (node)
                 (cdr (assq key node)))))
     
     (define (json:keys node)
         (list->vector (map car node)))
         
     (define (json:values node)
         (list->vector (map cdr node)))

     (define (json:flatten nodes)
         (apply vector-append (vector->list nodes)))
         
     (define (json:unique nodes)
         (-> nodes
             vector->list
             delete-duplicates
             list->vector))
         
     (define arithmetic-mapping
         `((= . ,equal?)
           (!= . ,(lambda args (not (apply equal? args))))))

     (define (arithmetic? func)
         (assq func arithmetic-mapping))

     (define (interpret-function-rule rule)
         (if (procedure? rule) rule
         (let ((func (car rule))
               (args (cdr rule)))
             (cond
                ((eq? func '*)
                 ;input is a vector of nodes instead of just a single node
                 (lambda (nodes) (vector-map (json:query args) nodes)))
                ((arithmetic? func)
                 (lambda (node) (apply (cdr (assq func arithmetic-mapping))
                                       (map
                                         (lambda (arg) (if (procedure? arg)
                                                           (arg node)
                                                           arg))
                                         args))))
                ((eq? func 'filter)
                 (lambda (nodes) (vector-filter (interpret-function-rule (car args))
                                                 nodes)))
                (else (error 'interpret-function-rule "Incorrect function rule" rule))))))

     (define (interpret-rule rule)
        (cond
            ((string? rule)
             (json:ref rule))
            ((procedure? rule)
             rule)
            ((symbol? rule)
             (-> rule
                 symbol->string
                 (lambda (x) (string-append "json:" x))
                 string->symbol
                 eval))
            ((list? rule)
             (interpret-function-rule rule))
            (else (error 'inperpret-rule "Incorrect rule" rule))))
         
     (define (json:query rules)
         (let ((rules (map interpret-rule rules)))
             (lambda (node)
                 (fold-left
                    (lambda (node rule) (rule node))
                    node
                    rules))))
         
)
