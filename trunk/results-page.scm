;; Results page
;; 

(require 'web-scheme)

(define results-page
(ws:page
 (head "Javascript Pretty Printer - Resultados"
       (body 
        (link 'value "../start-page")))))

(define (show-results-page formatted-js)
  (ws:page 
   (p "Código javascript com syntax-highlight:"
      (hr)
      formatted-js ) page-title: "Resultado" charset: "utf-8" ))

(define (main-cgi)
  (print "Content-type: text/html")
  (newline)
  (ws:with-get-vars (jssource)
                     (print (show-results-page jssource))))

(main-cgi)