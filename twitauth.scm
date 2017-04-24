(import (rnrs)
	(rfc oauth)
	(rfc http-connections)
	(sagittarius control)
	(sagittarius stty)
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

(define (report key secret token)
  (print "(")
  (print " (consumer-key \"" key "\")")
  (print " (consumer-secret \"" secret"\")")
  (print " (access-token \""(oauth-access-token-token token)"\")")
  (print " (access-token-secret \""(oauth-access-token-token-secret token)"\")")
  (print ")"))

(define (twitauth key secret)
  (define conn (make-oauth-connection
		(make-http1-connection "api.twitter.com" #t)
		key
		(make-oauth-hmac-sha1-signer (string->utf8 secret))))
  (let* ((token (oauth-request-temporary-credential
		 conn "/oauth/request_token"))
	 (pin (get-pin (make-oauth-authorization-url
			"https://api.twitter.com/oauth/authorize" token))))
    ;; obtain the access token
    (let1 access-token (oauth-request-access-token
			conn "/oauth/access_token" token pin)
      (report key secret access-token))))

(define (main args)
  (match (cdr args)
    (()
     (display "Enter consumer key: ") (flush-output-port (current-output-port))
     (let1 key (get-line (current-input-port))
       (when (eof-object? key)
	 (format (current-error-port) "aborted.") (exit 1))
       (display "Enter consumer secret: ")
       (flush-output-port (current-output-port))
       (let1 secret (with-stty '(not echo)
		      (lambda () (get-line (current-input-port))))
	 (newline (current-output-port))
	 (when (eof-object? key)
	   (format (current-error-port) "aborted.") (exit 1))
	 (twitauth key secret))))
    ((key secret) (twitauth key secret)))
  0)
     
