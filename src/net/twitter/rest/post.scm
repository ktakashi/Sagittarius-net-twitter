;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/twitter/rest/post.scm - Twitter POST REST API
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

(library (net twitter rest post)
    (export twitter-statuses-update)
    (import (rnrs)
	    (rfc oauth)
	    (rfc http-connections)
	    (rfc uri)
	    (srfi :13)
	    (sagittarius)
	    (net twitter rest util))

  (define (compose-form-parameters parameters)
    (define (->name&value parameters)
      (define (err)
	(assertion-violation 'compose-form-parameters "invalid parameter list"
			     parameters))
      (let loop ((parameters parameters) (r '()))
	(cond ((null? parameters) r)
	      ((null? (cdr parameters)) (err))
	      ((and (keyword? (car parameters)) (string? (cadr parameters)))
	       (loop (cddr parameters)
		     (cons (string-append (keyword->string (car parameters))
					  "="
					  (uri-encode-string (cadr parameters)))
			   r)))
	      (else (err)))))
    (string-join (->name&value parameters) "&"))
  
  (define (encode-parameters parameters)
    (define (encode-string p)
      (if (string? p)
	  (uri-encode-string p)
	  p))
    (map encode-string parameters))

  (define (send-post-request conn uri parameters headers)
    (apply oauth-request conn 'POST uri
	   :content-type "application/x-www-form-urlencoded"
	   :authorization (apply oauth-authorization-header
				 conn 'POST uri (encode-parameters parameters))
	   :sender (http-string-sender (oauth-connection-http-connection conn)
				       (compose-form-parameters parameters))
	   headers))

  (define (twitter-statuses-update conn message . options)
    (let-values (((parameters headers) (twitter-parameter&headers options)))
      (wrap-twitter-response
       (send-post-request conn "/1.1/statuses/update.json"
			  (cons* :status message parameters)
			  headers))))
    
  )
