;; Javascript pretty printer (to HTML)
;; Stephen Pedrosa Eilert (9911081), 2008
;; Construção de Compiladores 2008.1 - Riverson Rios

;; Including the command-line file

;; Command line format:
;; javascript2html <-o output file> input_file

;; Redirects standard output to a file. Currently does nothing.
(define (process-output-command arguments)
  (print "Output command, arguments:" arguments)
  (cdr arguments)) ; Eat one argument

;; Enables CSS generation. Also, doing nothing.
(define (process-css-command arguments)
  (print "Process css command, arguments: " arguments)
  arguments) ; Eat no arguments

;; Command-line parameters and their function mappings.
(define command-line-parameters
  `(
    ("-o" ,process-output-command)
    ("-css" ,process-css-command)))

;; Gets the first identifier in the command line, identifies if it is a command
;; or a simple file name and returns a pair in the format (type . parameter)
(define (get-command-line-token line)
  (if (null? line)
      #f
      (let ((member-value (assoc (car line) command-line-parameters)))
        (if member-value
            (cons 'command (cdr member-value))
            (cons 'other (car line))))))

;; Display command-line options, in case the user screws up.
(define (display-usage error)
  (when error
    (print "Error: " error)
    (newline))
  (print "Usage: javascript2html [-o output_file] input_file")
  (newline))

;; Macro to make it easier to call an operation returned by 
;; get-command-line-token
(define-macro (call-operation ass-list params)
  `(let ((command-c (cadr ,ass-list)))
     (command-c ,params)))

;; Adds a new file to the input files list.
(define (add-input-file file) ; This is very ugly
  (set! input-files (cons file input-files)))

;; Actually parses the command-line
(define (parse-command-line parameters)
  (unless (null? parameters)
    (let ((argument (get-command-line-token parameters)))
      (let ((result
             (case (car argument)
               ((command) (call-operation argument (cdr parameters)))
               ((other) (begin
                          (add-input-file (cdr argument))
                          (cdr parameters))))))
        (parse-command-line result)))))


;; Javascript identifiers
(define javascript-identifiers
  '(if then else return var))

;; List of input files, initially empty
(define input-files
  '())

(define (process-js-file filename)
  (with-input-from-file filename
    (parse-javascript-file)))

;; Recognizes Javascript tokens (as required by read-token). Returns #f when
;; a token is recognized
(define (predicate-identify-js-token character)
  #f)

;; Actually parses the given javascript file. Gets input from the
;; current-input-port
;;(define (parse-javascript-file)
;;  (let ((current-token (read-token predicate-identify-js-token)))
;;    ()))

(define (main args)
  (if (null? args)
      (display-usage #f)
      (begin
        (parse-command-line args)
        (print "Input file list: " (reverse input-files))
        (map process-js-file input-files))))

(main (cdr (argv)))
