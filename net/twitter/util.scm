;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/twitter/util.scm - Twitter API utility
;;;  
;;;   Copyright (c) 2012-2013  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=sagittarius/regex
(library (net twitter util)
    (export call/twitter-api
	    make-twitter-credential
	    make-query-params)
    (import (rnrs)
	    (rfc http)
	    (rfc uri)
	    (net oauth)
	    (text sxml ssax)
	    (util list)
	    (sagittarius)
	    (sagittarius io)
	    (sagittarius regex)
	    (json))
  (define (xml->sxml p) (ssax:xml->sxml p '()))
  (define (json->sexp p) (json-read p))

  ;; Do nothing
  (define (default-error-handler body header hint advice) body)
    
  ;; Low level API
  ;; Calls twitter API directory with params
  ;;
  ;; required arguments
  ;;  token:   oauth access token or #f if it's not required
  ;;  method:  http request method. must be 'GET or 'POST
  ;;  path:    twitter API path.
  ;;  params:  alist of parameters.
  ;;
  ;; keyword arguments
  ;;  secure?: use TLS if #t
  ;;  type:    currently only 'xml or 'json. determine calling API type
  ;;
  ;; rest arguments will be passed to http-get, http-post or
  ;; access-protected-resource as extra headers.
  (define (call/twitter-api token method path params
			    :key (secure? #f)
				 (type 'json) ;; 1.1 doesn't support xml anymore
				 (error-handler default-error-handler)
				 (body #f)
			    :allow-other-keys opts)
    (define composed-path (format "~a.~a" path type))
    (define (list->alist lis)
      (let loop ((lis lis) (r '()))
	(if (null? lis)
	    (reverse! r)
	    (loop (cddr lis) `((,(car lis) ,(cadr lis)) ,@r)))))
    (define (call)
      (if token
	  (access-protected-resource
	   (string-append (if secure?
			      "https://api.twitter.com"
			      "http://api.twitter.com") composed-path)
	   token
	   :user-parameters params
	   :request-method method
	   :body body
	   :additional-headers (list->alist opts))
	  (receive (status header body)
	      (case method
		((GET) (apply http-get "api.twitter.com"
			      (string-append composed-path "?"
					     (oauth-compose-query params))
			      :secure secure?
			      opts))
		((POST) (apply http-post "api.twitter.com"
			       (string-append composed-path "?"
					     (oauth-compose-query params))
			       body
			       :secure secure? opts)))
	    (values body header (not (string=? status "200")) status))))
    (define (retrieve body header hint advice)
      (if (not hint)
	  (call-with-input-string body 
	    (if (eq? type 'xml) xml->sxml json->sexp))
	  (error-handler body header hint advice)))
    (call-with-values call retrieve))

  ;; Convenient method
  ;; Thin wrapper of make-access-token and make-consumer-token
  (define (make-twitter-credential consumer-key consumer-secret
				   access-token access-token-secret)
    (make-access-token :key access-token :secret access-token-secret
		       :consumer (make-consumer-token
				  :key consumer-key
				  :secret consumer-secret)))

  ;; Internal macros
  ;; compose query parameter alist
  (define-syntax make-query-params
    (lambda (x)
      (syntax-case x ()
	((_ . vars)
	 #`(cond-list #,@(map (lambda (v)
				`(,v `(,',(regex-replace-all #/-/
							   (format "~a" v)
							   "_")
				       ,,v)))
			      (syntax->datum #'vars)))))))
)