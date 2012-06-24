(import (rnrs) (net oauth)
	(sagittarius control)
	(srfi :13 strings)
	(match))

(define (get-pin url)
  (print "Open the following url and type in the shown PIN.")
  (print url)
  (let loop ()
    (display "Input PIN: ") (flush-output-port (current-output-port))
    (let1 pin (get-line (current-input-port))
      (cond ((eof-object? pin) #f)
            ((string-null? pin) (loop))
            (else pin)))))

(define (report token)
  (print "(")
  (print " (consumer-key \"" (token-key (token-consumer token)) "\")")
  (print " (consumer-secret \""
	 (token-secret (token-consumer token))"\")")
  (print " (access-token \""(token-key token)"\")")
  (print " (access-token-secret \""(token-secret token)"\")")
  (print ")"))

(define (twitauth key secret)
  (let* ((token (obtain-request-token
		 "http://api.twitter.com/oauth/request_token"
		 (make-consumer-token :key key :secret secret)))
	 (pin (get-pin (make-authorization-uri
			"http://api.twitter.com/oauth/authorize" token))))
    ;; authorize the request token manually.
    (authorize-request-token token pin)
    ;; obtain the access token
    (let1 access-token (obtain-access-token 
			"http://api.twitter.com/oauth/access_token" token)
      (report access-token))))

(define (main args)
  (match (cdr args)
    (()
     (display "Enter consumer key: ") (flush-output-port (current-output-port))
     (let1 key (get-line (current-input-port))
       (when (eof-object? key)
	 (format (current-error-port) "aborted.") (exit 1))
       (display "Enter consumer secret: ")
       (flush-output-port (current-output-port))
       (let1 secret (get-line (current-input-port))
	 (when (eof-object? key)
	   (format (current-error-port) "aborted.") (exit 1))
	 (twitauth key secret))))
    ((key secret) (twitauth key secret)))
  0)
     