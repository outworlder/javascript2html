;; Scheme Server
;; 

(require 'web-scheme)

(define start-page
  (ws:page
   (head "Javascript Pretty Printer"
         (body (hr) "Entre com o código Javascript Abaixo:"
               (input 'type "textbox" )
               (input 'type "submit" 'value "Enviar")))))

       