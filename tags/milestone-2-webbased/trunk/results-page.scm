;; Results page
;; 

(require 'web-scheme)
(require 'cgi-util)

(include "js2html.scm")

(define (show-results-page formatted-js)
  (ws:page 
   (p "Código javascript com syntax-highlight:"
      (hr)
      formatted-js ) page-title: "Resultado" charset: "utf-8" ))

(define (main-cgi argv)
  (print "Content-type: text/html")
  (newline)
  (let ((jssource (CGI:lookup 'jssource 'string)))
    (print (show-results-page (process-js-from-web jssource)))))

(main-cgi argv)