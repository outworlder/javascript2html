;; Scheme Server (Start page)
;; 

(require 'web-scheme)

(define start-page
  (ws:page
   (p
    "Entre com o código Javascript abaixo:"
    (hr)
    (textarea 'name "jssource" 'rows 100 'cols 80)
    (br)
    (input 'type "submit" 'value "Enviar")) page-title:"Javascript Pretty Printer" charset: "utf-8"))

(define (main-cgi)
  (print "Content-type: text/html")
  (newline)
  (print start-page))
  
(main-cgi)