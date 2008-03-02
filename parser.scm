;; Javascript pretty printer (to HTML)
;; Stephen Pedrosa Eilert (9911081), 2008
;; Construção de Compiladores 2008.1 - Riverson Rios

;; Including the command-line file

;; Command line format:
;; javascript2html <-o output file> input_file

;; Loading the Regular Expressions library
(require 'regex)

(define output-to-file
  #f)

(define output-filename
  "")

;; List of input files, initially empty
(define input-files
  '())

(define javascript-reserved-words
  '("abstract" "boolean" "break" "byte" "case" "catch" "char" "class" "const"
"continue" "debugger" "default" "delete" "do" "double" "else" "enum" "export"
"extends" "false" "final" "finally" "float" "for" "function" "goto" "if"
"implements" "import" "in" "instanceof" "int" "interface" "long" "native"
"new" "null" "package" "private" "protected" "public" "return" "short" "static"
"super" "switch" "synchronized" "this" "throw" "throws" "transient" "true"
"try" "typeof" "var" "void" "volatile" "while" "with"))

(define javascript-delimiters
  '(#\, #\. #\{ #\} #\( #\) #\; #\space #\newline))

(define syntax-highlight-table
  '(
    (identifier "#00FF00" (none))
    (reserved-word "" (bold))
    (string "#FF0000" (none))
    (number "#0000FF" (none))
    (comment "#AAAAAA" (italics))
    (other "" (none))))

(define html-header 
  "<!DOCTYPE
 html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
 \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:schemaLocation=\"http://www.w3.org/MarkUp/SCHEMA/xhtml11.xsd\"
     xml:lang=\"en\" >
<head>
 <title>Javascript pretty printer - CCO08 </title>
</head>
<body>")

(define html-footer
  "</body>
</html>")

;; Redirects standard output to a file. Currently does nothing.
(define (process-output-command arguments)
  (set! output-to-file #t)
  (if (null? arguments)
      (error "Insufficient arguments for \"-o\". Aborting."))
  (set! output-filename (car arguments))
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

(define (apply-formatting text type)
  (if (null? type)
      text
      (case (car type)
        ((bold) (string-append "<b>" text "</b>"))
        ((italics) (string-append "<i>" text "</i>"))
        (else text))))

(define (html-font text color . attributes)
  (let ((new-text (apply apply-formatting text attributes)))
    (if (> (string-length color) 0)
        (string-append "<font color=" color ">" new-text "</font>")
        new-text)))

(define (build-html-formatting type text)
  (let ((attributes (assq type syntax-highlight-table)))
    (if attributes
        (let ((color (cadr attributes))
              (formatting (caddr attributes)))
          (html-font text color formatting))
        text)))

(define (html-format-token token)
  (print (build-html-formatting (car token) (cadr token)) " "))

(define (tokens->html token-list)
  (with-output-to-string
   (lambda ()
     (map html-format-token token-list))))

(define (match-number? str)
  (string->number str))

(define (match-identifier? str)
  (string-match "[a-zA-Z_$]+[0-9a-zA-Z_]*" str))

(define (match-reserved-word? str)
  (member str javascript-reserved-words))

(define (match-comment? str)
  (or (string-match "^[/]{2}.*" str)
      (string-match "^/*(.*)*/" str)))

;; Recognizes Javascript tokens (as required by read-token). Returns #f when
;; a token is recognized
(define (predicate-identify-js-token character)
  (or (char-alphabetic? character)
      (char-numeric? character)))

(define (next-token)
  (my-read-token predicate-identify-js-token))

(define (read-until delimiter port)
  (let ((buffer (string)))
    (let loop()
      (if (eof-object? (peek-char port))
          buffer
          (begin
            (set! buffer (string-append buffer (string (read-char port))))
            (if (substring-index delimiter buffer)
                buffer
                (loop)))))))

(define (check-and-return-comments identifiers port)
  (if (string=? identifiers "//")
      (string-append identifiers (read-line port))
      (if (string=? identifiers "/*")
          (string-append identifiers (read-until "*/" port))
          #f)))

(define (my-read-token pred)
  (let ((buffer (string)))
    (let loop ()
      (let ((char (read-char (current-input-port))))
        (if (eof-object? char)
            char
            (let ((next-char (peek-char (current-input-port))))
              (unless (eof-object? next-char)
                (let ((comments (check-and-return-comments (string char next-char) (current-input-port))))
                  (if comments
                      comments)))
              (if (pred char)
                  (begin
                    (set! buffer (string-append buffer (string char)))
                    (if (pred next-char)
                        (loop)
                        buffer))
                  (string char))))))))

(define (parse-tokens)
  (let ((buffer (list)))
    (let loop()
      (let ((token (next-token)))
        (if (not (eof-object? token))
            (let ((result (parse-token token)))
              (if result
                  (begin
                    (set! buffer (append buffer result))
                    (loop))
                  buffer))
            buffer)))))

;; Actually parses the given javascript file. Gets input from the
;; current-input-port
(define (parse-token current-token)
  (if current-token
      (let ((result (cond ((match-reserved-word? current-token) `((reserved-word ,current-token)))
                          ((match-identifier? current-token) `((identifier ,current-token)))
                          ((match-number? current-token) `((number ,current-token)))
                          ((match-comment? current-token) `((comment ,current-token)))
;;                          ((match-newline? current-token) '((newline #\newline)))
                          (else `((other, current-token))))))
        result)))

(define (process-js-file filename)
  (print "Parsing file: " filename)
  (let ((parsed-file (with-input-from-file filename parse-tokens)))
                      (print html-header (tokens->html parsed-file) html-footer)))
  
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

