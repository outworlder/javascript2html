;; Javascript pretty printer (to HTML)
;; Stephen Pedrosa Eilert (9911081), 2008
;; Construção de Compiladores 2008.1 - Riverson Rios

;; Including the command-line file

;; Command line format:
;; javascript2html <-o output file> input_file

;; Loading the Regular Expressions library
(require 'regex)

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

(define javascript-reserved-words
  '("abstract" "boolean" "break" "byte" "case" "catch" "char" "class" "const"
"continue" "debugger" "default" "delete" "do" "double" "else" "enum" "export"
"extends" "false" "final" "finally" "float" "for" "function" "goto" "if"
"implements" "import" "in" "instanceof" "int" "interface" "long" "native"
"new" "null" "package" "private" "protected" "public" "return" "short" "static"
"super" "switch" "synchronized" "this" "throw" "throws" "transient" "true"
"try" "typeof" "var" "void" "volatile" "while" "with"))

(define syntax-highlight-table
  '(
    (identifier "#00FF00")
    (reserved-word "" ('bold))
    (string "#FF0000")
    (number "#0000FF")
    (comment "#AAAAAA" ('italics))))

(define (apply-formatting text type)
  (print "Formatting: " type)
  (print "Text: " text)
  (case (car type)
    ('bold (string-append "<b>" text "</b>"))
    ('italics (string-append "<i>" text "</i>"))
    (else text)))

(define (html-font text color . attributes)
  (let ((new-text (apply apply-formatting text attributes))) 
    (print "New text: " new-text)
    (if (< (string-length color) 0)
        (string-append "<font color=" color ">" new-text "</font>")
        new-text)))

(define (build-html-formatting type text)
      (let ((attributes (assq type syntax-highlight-table)))
        (if attributes
            (let ((color (cadr attributes))
                  (formatting (caddr attributes)))
              (print "Color: " color "Formatting: " formatting)
            (html-font text color formatting)))))


;; List of input files, initially empty
(define input-files
  '())

(define (process-js-file filename)
  (print "Parsing file: " filename)
  (let ((parsed-file (with-input-from-file filename parse-tokens)))
    (print parsed-file)))

(define (match-number? string)
  (string->number string))

(define (match-identifier? string)
  (string-match "[a-zA-Z_$]+[0-9a-zA-Z_]*" string))

(define (match-reserved-word? string)
  (member string javascript-reserved-words))

(define (match-comment? string)
  (or (string-match "^[/]{2}.*" string)
      (string-match "^/*(.*)*/" string)))

;; Recognizes Javascript tokens (as required by read-token). Returns #f when
;; a token is recognized
(define (predicate-identify-js-token character)
  (or (char-alphabetic? character)
      (char-numeric? character)))

(define (next-token)
  (read-token predicate-identify-js-token))

;; Actually parses the given javascript file. Gets input from the
;; current-input-port
(define (parse-tokens)
  (let ((current-token (next-token)))
    (if current-token
        (begin
          (print current-token)
          (let ((result (cond ((match-reserved-word? current-token) `(reserved-word ,current-token))
                              ((match-identifier? current-token) `(identifier ,current-token))
                              ((match-number? current-token) `(number ,current-token))
                              ((match-comment? current-token) `(comment ,current-token))
                              (else (cons (parse-tokens))))))
            (print "Match: " (car result))
            (build-html-formatting (car result) (cadr result)))))))
                  

(define (main args)
  (if (null? args)
      (display-usage #f)
      (begin
        (parse-command-line args)
        (print "Command-line parsing complete.")
        (if (null? input-files)
            (display-usage)
            (map process-js-file input-files)))))

(main (cdr (argv)))

;;(string-match "[a-z0-9A-Z]*|^[{}./+,;()%]" "teste.")
