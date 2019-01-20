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
        (- ,-)
        (* ,*)
        (/ ,/)
        (abs ,abs)
        (< ,<)
        (<= ,<=)
        (> ,>)
        (>= ,>=)
        (= , =)
     ))

;; VARIABLE TABLE
(define *variable-table* (make-hash))
;; Variable-get => if key is in table return val, else return 0
(define (variable-get key)
        (if (hash-has-key? *variable-table* key) (hash-ref *variable-table* key) 0))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))
;; init *variable-table*
;; e & pi
(for-each
    (lambda (pair)
        (variable-put! (car pair) (cadr pair)))
    `(
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
      (printf "line is : ~a\n" line)

      (let ( (statement
        (cond                                   ;;this conditional evaluates the appropriate statement for the specific line
          [(null? (cdr line)) null]
          [(pair? (cadr line)) (cadr line)]
          [(null? (cddr line)) null]
          [(pair? (caddr line)) (caddr line)]
          [else (error "Error in syntax of file")]
        )))
        (unless (null? statement)
          (interpret-statement statement)     ;;if there is a statement, then interpret it
        )
      )
    )

    (interpret-program (cdr program))
  )
)

(define (interpret-statement statement)
  (printf "statement is : ~a~n" statement)
  (let ((keyword (symbol->string(car statement))))
    (printf "keyword is : ~a~n" keyword)
    (cond                                   ;;this conditional finds out which kind of statement this is
      [(equal? keyword "print") (printf"print~n")]
      [(equal? keyword "let") (printf"let~n")]
      [(equal? keyword "if") (printf"if~n")]
      [(equal? keyword "dim") (printf"dim~n")]
      [(equal? keyword "goto") (printf"goto~n")]
      [(equal? keyword "input") (printf"input~n")]
      [else (error "No such statement")]
    )
  )
)


;; go to a label
(define (interpret-goto program)
        (interpret-program (label-get program))
)

;;(define (interpret-if program))

;;(define (interpret-let program))

;;(define (interpret-dim program))

;;(define (interpret-print program))

;;(define (interpret-input program))





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

;;(when (terminal-port? *stdin*)
;;    (main (vector->list (current-command-line-arguments))))
