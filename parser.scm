;; Javascript pretty printer (to HTML)
;; Stephen Pedrosa Eilert, 2008
;; Construção de Compiladores 2008.1 - Riverson Rios

;; First things first: Parse the command line and open a file

;; Javascript identifiers
(define javascript-identifiers
  '(if then else return var))

;; Command line format:
;; javascript2html <-o output file> input_file

(define input-files
  '())

(define (process-output-command arguments)
  (print "Output command, arguments:" arguments))

(define command-line-parameters
  '(
    ("-o" process-output-command)
    ("-css" process-css-command)))

(define (get-command-line-token line)
  (if (null? line)
      #f
      (begin
        (print "get-command-line-token, line: " (car line))
        (let ((member-value (assoc (car line) command-line-parameters)))
          (if member-value
              (cons 'command (cdr member-value))
              (cons 'other (car line)))))))

(define (display-usage error)
  (when error
    (print "Error: " error)
    (newline))
  (print "Usage: javascript2html [-o output_file] input_file")
  (newline))

(define-macro (call-operation ass-list params)
    `(let ((command-c (cadr ,ass-list)))
      (command-c ,params)))

(define (add-input-file file) ; This is very ugly
  (set! (cons file input-files) input-files))

(define (parse-command-line parameters)
  (unless (null? parameters)
    (let ((argument (get-command-line-token parameters)))
      (print "Argument: " argument)
      (print "Car: " (car argument))
      (print "Parameters: " (cdr parameters))
      (case (car argument)
        ((command) (call-operation argument (cdr parameters)))
        ((other) (add-input-file (cdr argument)))))))

(define (open-js-file filename)(print "Load file here."))

(define (main args)
  (if (null? args)
      (display-usage #f)
      (print (parse-command-line args))))

(main (cdr (argv)))
