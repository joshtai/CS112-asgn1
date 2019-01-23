#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.8 2019-01-11 17:38:01-08 - - $

;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

;; ------------------------------------------------
;; --------------- Start of own code --------------
;; ------------------------------------------------

;; FUNCTION TABLE
(define *function-table* (make-hash))
(define (function-get key)
        (hash-ref *function-table* key))
(define (function-put! key value)
        (hash-set! *function-table* key value))

;; initalize *function table*
;; pulled from
;; https://ds26gte.github.io/tyscheme/index-Z-H-4.html#node_sec_2.1
(for-each
    (lambda (pair)
        (function-put! (car pair) (cadr pair)))
    `(

        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+ ,+)
        (^ ,expt)
        (ceil ,ceiling)
        (exp ,exp)
        (floor ,floor)
        (log ,log)
        (sqrt ,sqrt)
        ;;
        (<>,(lambda (x y) (not(= x y))))
        (print, print)
        (- ,-)
        (* ,*)
        ;; (/ 35 5) ; => 7
        (/ , (lambda (x y)
            (/ x (if (equal? y 0) (variable-get nan) y))))
        (abs ,abs)
        (< ,<)
        (<= ,<=)
        (> ,>)
        (>= ,>=)
        (= , =)
        (sin, sin)
        (cos, cos)
        (tan, tan)
        (acos, acos)
        (asin, asin)
        (atan, atan)
        (round, round)
        (asub, (lambda (x y)
            (vector-ref (array-get x) (exact-round (evaluate-expression y)))))
     ))

;; VARIABLE TABLE
(define *variable-table* (make-hash))
;; Variable-get => if key is in table return val, else return 0
(define (variable-get key)
        (if (hash-has-key? *variable-table* key)
            (hash-ref *variable-table* key) 0))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))
;; init *variable-table*
;; e & pi
(for-each
    (lambda (pair)
        (variable-put! (car pair) (cadr pair)))
    `(
        (nan 0.0)
        (eof 0)
        (e 2.718281828459045235360287471352662497757247093)
        (pi 3.141592653589793238462643383279502884197169399)
     ))
;; ARRAY TABLE
(define *array-table* (make-hash))
(define (array-get key)
        (hash-ref *array-table* key))
(define (array-put! key value)
        (hash-set! *array-table* key value))

;; function for making an array (vector)


;; function for updating an array (vector)
;;(define (vector-set!)

;;)

;; LABEL TABLE
(define *label-table* (make-hash))
(define (label-get key)
        (hash-ref *label-table* key))
(define (label-put! key value)
        (hash-set! *label-table* key value))

;; Gets the program and then puts the labels
;; into the label hashtable (does it recursively)
;; similar to listhash from Scheme Example
(define (fetch-labels list)
;; unless => (when (not ))
;; (unless #f) => true
    (unless (null? list)
        (unless (null? (cdar list))
            (let ((ele (cadar list)))
              ;;(printf "ele: ~a\n" ele)
                (when (symbol? ele)
                    (label-put! ele list))))
        (fetch-labels (cdr list))
    )
)

(define (interpret-program program)
  (unless (null? program)
    (let ((line (car program)))
      ;;(printf "line is : ~a\n" line)
      (let ((statement

        (cond
          [(null? (cdr line)) null]
          [(pair? (cadr line)) (cadr line)]
          [(null? (cddr line)) null]
          [(pair? (caddr line)) (caddr line)]
          [else (error "Error in syntax of file")]
        )))
        (if (null? statement)
          (interpret-program (cdr program))
          ;;if there is a statement, then interpret it
          (interpret-statement statement program)
        )
      )
    )
  )
)

(define (interpret-statement statement program)
  ;;(printf "statement is : ~a~n" statement)
  (let ((keyword (symbol->string(car statement))))
    ;;(printf "keyword is : ~a~n" keyword)
    ;;next_statement is void if no control transfer,
    ;;otherwise will be appropriate statement to jump to
    (let ((next_statement
        ;;this conditional finds out which kind of statement this is
        (cond
          [(equal? keyword "print") (interpret-print (cdr statement))]
          [(equal? keyword "let") (interpret-let (cdr statement))]
          [(equal? keyword "if") (interpret-if (cdr statement))]
          [(equal? keyword "dim") (interpret-dim (cadr statement))]
          [(equal? keyword "goto") (interpret-goto (cdr statement))]
          [(equal? keyword "input") (interpret-input (cdr statement))]
          [else (error "No such statement")]
        )))
        ;;(printf "next: ~a~n" next_statement)
        (if (void? next_statement)
          (interpret-program (cdr program))
          ;;if there is a statement, then interpret it
          (interpret-program next_statement)
        )
    )
  )
)

;; used hashexample.scm as an example (apply then map)
(define (evaluate-expression expr)
    (cond
        [(number? expr) (+ 0.0 expr)]
        [(and
            (hash-has-key? *variable-table* expr) (symbol? expr))
                (variable-get expr)]
        [(pair? expr)
            ;; (cons x (cons y z))
            ;;(printf "pair? ~a~n" expr)
            ;;(printf "~a~n" (cdr expr))
            (cond
                [(hash-has-key? *variable-table* (car expr))
                    (vector-ref (variable-get (car expr))
                        (- (inexact->exact
                            (evaluate-expression (cadr expr))) 1)
                    )
                ]
                ;; gets (car expr) -> applies the correct function
                [(hash-has-key? *function-table* (car expr))
                    (apply (function-get (car expr))
                        (map evaluate-expression (cdr expr)))]
                (else #f)
            )]
        (else expr)
    ))

;; go to a label
(define (interpret-goto program)
        ;;(printf "interpret goto ~a~n" program)
        (if (hash-has-key? *label-table* (car program))
            (label-get (car program))
            (error "Label does not exist")
        )

        ;;(interpret-program (label-get program))
)

(define (interpret-if program)
    (unless (null? program)
        (when (evaluate-expression (car program))
            (interpret-goto (cdr program))))
    ;; (if (expr) label)
    ;;(unless (null? program)
    ;;    (when (evaluate-expression (car program))
    ;;        (when (hash-has-key? *label-table* (cdr program))
    ;;            (label-get (car program)))))
)

(define (interpret-let program)
    ;;(printf "let ~a~n" program)
    (let ((symbol (car program)))
        ;;a = (cadr symbol))
        ;;i = (caddr symbol)
        ;;(printf "symbol ~a~n" (pair? symbol))
        ;;(printf "let ~a~n" (cadr program))
        (let ((val (evaluate-expression (cadr program))))
            (when (pair? symbol)

              (vector-set! (array-get (cadr symbol))
                (exact-round (evaluate-expression (caddr symbol))) val)
              ;;(exact-round (evaluate-expression (caddr symbol)))))
            )
            (unless (pair? symbol)
              (variable-put! symbol val)
            )
        )
    )
)

(define (interpret-dim program)
    (unless (null? program)
        (printf "dim with ~a ~n" program)
        (printf "dim ~a ~n " (evaluate-expression(caddr program)))
        ;;(printf"test: ~a~n" (exact-round 4.7))
        (when (> 0 (evaluate-expression(caddr program)))
          (error "cant have negative length array")
        )

        (array-put! (cadr program)
            (make-vector
                (exact-round (evaluate-expression(caddr program))) 0))
    )
)

(define (interpret-print statement)
  (unless (null? statement)
      (let ((expr (car statement)))
        ;;(printf "expr  ~a ~n" expr)
        (if (or (number? expr)(or (symbol? expr)(pair? expr)))
                    (printf " ~a" (evaluate-expression expr))
                    (printf "~a" expr)
        )
      )
      (interpret-print (cdr statement))
  )
  (when (null? statement) (printf "~n"))
  )

(define (interpret-input program)
    (let ((newnum (read)))
        (cond
            [(or (eof-object? newnum) (not (number? newnum)))
                (variable-get nan)]
            [(number? newnum)
                ;;(printf "--------~n")
                ;;(printf "~a~n" (car program))
                ;;(printf "~a~n" (variable-get (car program)))
                ;;(printf "--------~n")
                (variable-put! (car program) newnum)
                (unless (null? (cdr program))
                    (interpret-input (cdr program))
                )
            ]
        )
    )
)

;; --------- END ----------

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))


(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n")
    (dump-stdin))

(define (main arglist)

    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              ;;(write-program-by-line sbprogfile program)
              (fetch-labels program)
              (interpret-program program)
              )))

(main (vector->list (current-command-line-arguments)))
