;; Scheme Server (Start page)
;; 

(require 'web-scheme)
(require 'stream-cgi)

(define start-page
  (ws:page
   (head "Javascript Pretty Printer"
         (body (hr) "Entre com o código Javascript Abaixo:"
               (input 'type "textbox" )
               (input 'type "submit" 'value "Enviar")))))

(define (process-page query post cookies)
  start-page)

(cgi-main process-page)