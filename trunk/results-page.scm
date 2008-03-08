#! 
;; Results page
;; 

(require 'web-scheme)

(define results-page
(ws:page
 (head "Javascript Pretty Printer - Resultados"
       (body 
        (link 'value "../start-page")))))