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

;; standard error
(define *stderr* (current-error-port))

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
              (write-program-by-line sbprogfile program))))

;;(when (terminal-port? *stdin*)
;;      (main (vector->list (current-command-line-arguments))))

;; ------- Start of own code -------

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
(define (variable-get key)
        (hash-ref *variable-table* key))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))
;; init *variable-table*
;; e & pi
(for-each
    (lambda (pair) 
        (variable-put! (car pair) (cadr pair)))
    `( 
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



;; function for updating the symbol tables
;;(define (hash-set!)

;;)

;;(define (interpret-program))
