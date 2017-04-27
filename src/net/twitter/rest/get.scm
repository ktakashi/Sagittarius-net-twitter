;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/twitter/rest/get.scm - Twitter GET REST API
;;;  
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

(library (net twitter rest get)
    (export twitter:account/settings
	    twitter:account/verify-credentials
	    twitter:application/rate-limit-status
	    twitter:blocks/ids
	    twitter:blocks/list
	    twitter:collections/entries
	    twitter:collections/list
	    twitter:collections/show
	    twitter:direct-messages
	    twitter:direct-messages/sent
	    twitter:direct-messages/show
	    twitter:direct-messages/events/list
	    twitter:direct-messages/events/show
	    twitter:direct-messages/welcome-messages/list
	    twitter:direct-messages/welcome-messages/show
	    twitter:direct-messages/welcome-messages/rules/list
	    twitter:direct-messages/welcome-messages/rules/show
	    twitter:favorites/list
	    twitter:followers/ids
	    twitter:followers/list
	    )
    (import (rnrs)
	    (rename (rfc oauth)
		    (oauth-request/header-authorization twitter-request))
	    (rfc uri)
	    (rfc http-connections)
	    (sagittarius)
	    (net twitter rest util))

  (define (compose-query-string uri parameter)
    (define (->string v)
      (cond ((string? v) v)
	    ((boolean? v) (if v "true" "false"))
	    ((number? v) (number->string v))
	    ((symbol? v) (symbol->string v))
	    ((keyword? v) (keyword->string v))
	    (else (assertion-violation 'compose-query-string
				       "unknown type of object" v))))
    (define (concat parameter)
      (let-values (((out extract) (open-string-output-port)))
	(let loop ((parameter parameter) (first? #t))
	  (if (null? parameter)
	      (extract)
	      (let ((k (car parameter))
		    (v (cadr parameter)))
		(unless first? (put-string out "&"))
		(put-string out (keyword->string k))
		(put-string out "=")
		(put-string out (uri-encode-string (->string v)))
		(loop (cddr parameter) #f))))))
    (string-append uri "?" (concat parameter)))

  (define-syntax define-twitter-get-api
    (lambda (x)
      (define (keyword&id id)
	(cons (symbol->keyword (syntax->datum id)) id))
      (syntax-case x (required)
	((_ name uri)
	 #'(define (name conn . opt)
	     (wrap-twitter-response
	      (apply twitter-request conn 'GET uri opt))))
	((_ name uri (required))
	 #'(define (name conn . opt)
	     (let-values (((parameter headers) (twitter-parameter&headers opt)))
	       (wrap-twitter-response
		(apply twitter-request conn 'GET
		       (compose-query-string uri parameter) opt)))))
	((k name uri (required req ...))
	 (with-syntax ((((key . req) ...)
			(datum->syntax #'k (map keyword&id #'(req ...)))))
	   #'(define (name conn req ... . opt)
	       (let-values (((parameter headers)
			     (twitter-parameter&headers opt)))
		 (wrap-twitter-response
		  (apply twitter-request conn 'GET
			 (compose-query-string uri (append `(key ,req) ...
							   parameter))
			 opt)))))))))
  (define-twitter-get-api twitter:account/settings "/1.1/account/settings.json")
  (define-twitter-get-api twitter:account/verify-credentials
    "/1.1/account/verify_credentials.json" (required))
  (define-twitter-get-api twitter:application/rate-limit-status
    "/1.1/application/rate_limit_status.json" (required))
  (define-twitter-get-api twitter:blocks/ids "/1.1/blocks/ids" (required))
  (define-twitter-get-api twitter:blocks/list "/1.1/blocks/list" (required))
  (define-twitter-get-api twitter:collections/entries
    "/1.1/collections/entries" (required id))
  (define-twitter-get-api twitter:collections/list
    "/1.1/collections/list" (required user_id screen_name))
  (define-twitter-get-api twitter:collections/show
    "/1.1/collections/list" (required id))
  (define-twitter-get-api twitter:direct-messages
    "/1.1/direct_messages.json" (required))
  (define-twitter-get-api twitter:direct-messages/sent
    "/1.1/direct_messages/sent.json" (required))
  (define-twitter-get-api twitter:direct-messages/show
    "/1.1/direct_messages/show.json" (required id))
  (define-twitter-get-api twitter:direct-messages/events/list
    "/1.1/direct_messages/events/list.json" (required))
  (define-twitter-get-api twitter:direct-messages/events/show
    "/1.1/direct_messages/events/show.json" (required id))
  (define-twitter-get-api twitter:direct-messages/welcome-messages/list
    "/1.1/direct_messages/welcome_messages/list.json" (required))
  (define-twitter-get-api twitter:direct-messages/welcome-messages/show
    "/1.1/direct_messages/welcome_messages/show.json" (required id))
  (define-twitter-get-api twitter:direct-messages/welcome-messages/rules/list
    "/1.1/direct_messages/welcome_messages/rules/list.json" (required))
  (define-twitter-get-api twitter:direct-messages/welcome-messages/rules/show
    "/1.1/direct_messages/welcome_messages/rules/show.json" (required id))
  (define-twitter-get-api twitter:favorites/list
    "/1.1/favorites/list.json" (required))
  (define-twitter-get-api twitter:followers/ids
    "/1.1/followers/ids.json" (required))
  (define-twitter-get-api twitter:followers/list
    "/1.1/followers/list.json" (required))
  
  )
