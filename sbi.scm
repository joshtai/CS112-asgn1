#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
<<<<<<< HEAD
;; $Id: sbi.scm,v 1.8 2019-01-11 17:38:01-08 - - $
=======
;; $Id: sbi.scm,v 1.5 2019-01-04 17:04:42-08 - - $
>>>>>>> c425dabf220cdc39b04b25698d3b9391477f5c83
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
<<<<<<< HEAD
=======
;; standard error
>>>>>>> c425dabf220cdc39b04b25698d3b9391477f5c83
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
<<<<<<< HEAD
    (printf ")~n"))
=======
    (printf ")~n")
    (dump-stdin))
>>>>>>> c425dabf220cdc39b04b25698d3b9391477f5c83

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))

<<<<<<< HEAD
(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments))))

=======
;;(when (terminal-port? *stdin*)
;;      (main (vector->list (current-command-line-arguments))))

;; ------- Start of own code -------
;; function table
(define *function-table* (make-hash))
;; variable table
(define *variable-table* (make-hash))
;; array table
(define *array-table* (make-hash))
;; label table
(define *label-table* (make-hash))
t
>>>>>>> c425dabf220cdc39b04b25698d3b9391477f5c83
