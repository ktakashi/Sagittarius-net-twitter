;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/twitter/rest/util.scm - Twitter REST API utilities
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

#!read-macro=sagittarius/regex
(library (net twitter rest util)
    (export make-twitter-connection
	    wrap-twitter-response
	    twitter-parameter&headers
	    ->string
	    twitter-uri->api-name
	    twitter-connection-change-domain
	    twitter-connection-ensure-domain
	    +twitter-upload-server+
	    twitter-send-post-request
	    twitter-encode-parameters
	    twitter-compose-form-parameters
	    )
    (import (rnrs)
	    (text json)
	    (text json object-builder)
	    (srfi :13)
	    (rfc uri)
	    (rfc oauth)
	    (rfc :5322)
	    (rfc http-connections)
	    (sagittarius)
	    (sagittarius regex)
	    (util hashtables)
	    (net twitter conditions)
	    (only (net twitter connections) make-twitter-connection))

  (define-record-type twitter-errors
    (fields errors))
  (define twitter-error-builder
    (json-object-builder
     (make-twitter-errors
      ("errors" (@ condition
		   (make-twitter-error "code" "message"))))))

  (define-constant +twitter-upload-server+ "upload.twitter.com")
  (define (twitter-connection-change-domain conn domain
	    :optional (ctr make-http1-connection))
    (open-oauth-connection!
     (make-oauth-connection
      (ctr domain #t)
      (oauth-connection-consumer-key conn)
      (oauth-connection-access-token conn)
      (oauth-signer-clone (oauth-connection-signer conn)))))
  (define (twitter-connection-ensure-domain conn domain . opt)
    (define http-connection (oauth-connection-http-connection conn))
    (define server (http-connection-server http-connection))
    (if (string=? server domain)
	conn
	(apply twitter-connection-change-domain conn domain opt)))
  
  (define-syntax wrap-twitter-response
    (syntax-rules ()
      ((_ exprs ...)
       (let-values (((s h b) (let () exprs ...)))
	 (values s h (utf8->string b))))))

  (define (twitter-compose-form-parameters parameters)
    (define (->name&value parameters)
      (define (err)
	(assertion-violation 'twitter-compose-form-parameters
			     "invalid parameter list" parameters))
      (let loop ((parameters parameters) (r '()))
	(cond ((null? parameters) r)
	      ((null? (cdr parameters)) (err))
	      ((keyword? (car parameters))
	       (loop (cddr parameters)
		     (cons (string-append
			    (keyword->string (car parameters))
			    "="
			    (uri-encode-string (->string (cadr parameters))))
			   r)))
	      (else (err)))))
    (string-join (->name&value parameters) "&"))

  (define (twitter-encode-parameters parameters)
    (define (encode-string p)
      (if (keyword? p)
	  p
	  (uri-encode-string (->string p))))
    (map encode-string parameters))

  (define (twitter-send-post-request conn uri parameters headers)
    (apply oauth-request conn 'POST uri
	   :content-type "application/x-www-form-urlencoded"
	   :authorization (apply oauth-authorization-header
				 conn 'POST uri
				 (twitter-encode-parameters parameters))
	   :sender (http-string-sender (oauth-connection-http-connection conn)
		     (twitter-compose-form-parameters parameters))
	   headers))
  
  ;; bit ugly...
  (define-constant +twitter-parameter-keywords+
    '(:in_reply_to_status_id
      :possibly_sensitive
      :lat
      :long
      :place_id
      :query
      :ip
      :granularity
      :accuracy
      :max_results
      :contained_within
      :attribute:street_address
      :callback
      :desplay_coordinates
      :trim_user
      :media_ids
      :enable_dm_commands
      :fail_dm_commands
      :include_entities
      :skip_status
      :include_email
      :resources
      :stringify_ids
      :cursor
      :tweet_id
      :count
      :description
      :url
      :timeline_order
      :since_id
      :max_id
      :page
      :user_id
      :screen_name
      :slug
      :owner_screen_name
      :owner_id
      :include_rts
      :geocode
      :lang
      :locale
      :result_type
      :until
      :exclude_replies
      :map
      :include_user_entities
      :contributor_details
      :tweet_mode
      :attachment_url
      :auto_populate_reply_metadata
      :exclude_reply_user_ids
      ))
  (define (twitter-parameter&headers options)
    (define (err)
      (assertion-violation 'twitter-parameter&headers "invalid list of options"
			   options))
    (let loop ((options options) (params '()) (headers '()))
      (cond ((null? options) (values params headers))
	    ((null? (cdr options)) (err))
	    ((memq (car options) +twitter-parameter-keywords+)
	     (loop (cddr options)
		   (let ((name (car options))
			 (value (cadr options)))
		     (if value
			 (cons* (car options) (cadr options) params)
			 params))
		   headers))
	    (else
	     (loop (cddr options)
		   params
		   (let ((name (car options))
			 (value (cadr options)))
		     (if value
			 (cons* (car options) (cadr options) headers)
			 headers)))))))

  (define (->string v)
    (cond ((string? v) v)
	  ((boolean? v) (if v "true" "false"))
	  ((number? v) (number->string v))
	  ((symbol? v) (symbol->string v))
	  ((keyword? v) (keyword->string v))
	  (else (assertion-violation 'compose-query-string
				     "unknown type of object" v))))

  (define (twitter-uri->api-name uri)
    (cond ((#/\/1.1\/(.+?)\.json/ uri) =>
	   (lambda (m)
	     (let ((name (m 1)))
	       (string->symbol
		(string-append "twitter:" (regex-replace-all #/_/ name "-"))))))
	  (else (assertion-violation 'twitter-uri->api-name
				     "invalid Twitter API uri" uri))))

  )
