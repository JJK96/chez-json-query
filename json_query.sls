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
             debug
             tree-map)
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

     (define (tree-map func)
         (lambda (tree)
             (cond
                ((null? tree)
                 '())
                ((vector? tree)
                 (vector-map (tree-map func) tree))
                ((list? tree)
                 (map (tree-map func) tree))
                (else (func tree)))))

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

     (define (execute-procedures tree node)
         ; Execute all procedures found anywhere within the node
        ((tree-map 
            (lambda (obj) 
                (if (procedure? obj)
                    (obj node)
                    obj)))
         tree))

     (define (interpret-function-rule rule)
         (if (procedure? rule) rule
         (let ((func (car rule))
               (args (cdr rule)))
             (cond
                ((eq? func '*)
                 ;input is a vector of nodes instead of just a single node
                 (lambda (nodes) (vector-map (json:query args) nodes)))
                ((eq? func 'filter)
                 (lambda (nodes) (vector-filter (interpret-function-rule (car args))
                                                 nodes)))
                (else
                 (lambda (node) (eval (execute-procedures rule node))))))))

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
