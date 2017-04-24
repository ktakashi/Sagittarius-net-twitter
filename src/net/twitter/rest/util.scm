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

(library (net twitter rest util)
    (export parse-twitter-response
	    make-twitter-connection
	    wrap-twitter-response
	    twitter-parameter&headers)
    (import (rnrs)
	    (text json)
	    (text json object-builder)
	    (srfi :13)
	    (rfc oauth)
	    (rfc :5322)
	    (sagittarius)
	    (net twitter conditions)
	    (only (net twitter connections) make-twitter-connection))

  (define-record-type twitter-errors
    (fields errors))
  (define twitter-error-builder
    (json-object-builder
     (make-twitter-errors
      ("errors" (@ condition
		   (make-twitter-error "code" "message"))))))
  
  (define (parse-twitter-response status header body)
    (unless (string-prefix? "application/json"
			    (rfc5322-header-ref header "content-type" ""))
      (raise (condition
	      (make-http-error status header (utf8->string body))
	      (make-who-condition 'parse-twitter-response)
	      (make-message-condition "content-type is not JSON"))))
    (unless (string=? status "200")
      (raise (condition
	      (twitter-errors-errors
	       (json-string->object (utf8->string body) twitter-error-builder))
	      (make-http-error status header (utf8->string body))
	      (make-who-condition 'parse-twitter-response)
	      (make-message-condition "got error status"))))
    (json-read (open-string-input-port (utf8->string body))))


  (define-syntax wrap-twitter-response
    (syntax-rules ()
      ((_ exprs ...)
       (let-values (((s h b) (let () exprs ...)))
	 (parse-twitter-response s h b)))))
  
  ;; bit ugly...
  (define-constant +twitter-parameter-keywords+
    '(:in_reply_to_status_id
      :possibly_sensitive
      :lat
      :long
      :place_id
      :desplay_coordinates
      :trim_user
      :media_ids
      :status))
  (define (twitter-parameter&headers options)
    (define (err)
      (assertion-violation 'twitter-parameter&headers "invalid list of options"
			   options))
    (let loop ((options options) (params '()) (headers '()))
      (cond ((null? options) (values params headers))
	    ((null? (cdr options)) (err))
	    ((memq (car options) +twitter-parameter-keywords+)
	     (loop (cddr options)
		   (cons* (car options) (cadr options) params)
		   headers))
	    (else
	     (loop (cddr options)
		   params
		   (cons* (car options) (cadr options) headers))))))
  )