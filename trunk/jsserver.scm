;; Scheme Server (Start page)
;; 

(require 'web-scheme)

(define start-page
  (ws:page
   (head "Javascript Pretty Printer"
         (body (hr) "Entre com o código Javascript abaixo:"
               (input 'type "textbox" )
               (input 'type "submit" 'value "Enviar"))) charset:"utf-8"))

(define (main-cgi)
  (print "Content-type: text/html")
  (newline)
  (print start-page))
  
(main-cgi)