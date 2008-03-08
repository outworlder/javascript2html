;; Scheme Server (Start page)
;; 

(require 'web-scheme)

(define start-page
  (ws:page
   (p
    "Entre com o código Javascript abaixo:"
    (hr)
    (input 'type "textbox" )
    (input 'type "submit" 'value "Enviar")) page-title:"Javascript Pretty Printer" charset: "utf-8"))

(define (main-cgi)
  (print "Content-type: text/html")
  (newline)
  (print start-page))
  
(main-cgi)