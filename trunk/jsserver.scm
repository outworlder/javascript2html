;; Scheme Server (Start page)
;; 

(require 'web-scheme)

(define start-page
  (ws:page
   (p
    (form 'action "results-page.cgi" 'method "get"
    "Entre com o código Javascript na caixa de texto abaixo:"
    (hr)
    (textarea 'name "jssource" 'rows 40 'cols 80)
    (br)
    (input 'type "submit" 'value "Enviar"))) page-title:"Javascript Pretty Printer" charset: "utf-8"))

(define (main-cgi)
  (print "Content-type: text/html")
  (newline)
  (print start-page))
  
(main-cgi)