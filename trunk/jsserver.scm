;; Scheme Server (Start page)
;; 

(require 'web-scheme)

(define start-page
  (ws:page
   (head "Javascript Pretty Printer"
         (body (hr) "Entre com o código Javascript Abaixo:"
               (input 'type "textbox" )
               (input 'type "submit" 'value "Enviar")))))

(define (main-cgi)
  (print "Content-type: text/html")
  (print)
  (print start-page))
  
(main-cgi)