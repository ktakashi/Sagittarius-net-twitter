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
    (export twitter-statuses-update
	    twitter-media-upload
	    twitter-media-chunk-upload
	    twitter-media-chunk-upload/init
	    twitter-media-chunk-upload/append
	    twitter-media-chunk-upload/finalize

	    )
    (import (rnrs)
	    (rfc oauth)
	    (rfc http-connections)
	    (rfc uri)
	    (rfc mime)
	    (rfc base64)
	    (srfi :13)
	    (sagittarius)
	    (sagittarius control)
	    (net twitter conditions)
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
  (define (send-multipart-request conn uri parts parameters headers)
    (apply oauth-request conn 'POST uri
	   :authorization (apply oauth-authorization-header
				 conn 'POST uri (encode-parameters parameters))
	   :sender (http-multipart-sender
		    (oauth-connection-http-connection conn) parts)
	   headers))

  (define (make-content-disposision name)
    `(("content-disposition" ("form-data" ("name" . ,name)))))
  (define (change-domain conn domain :optional (ctr make-http1-connection))
    (open-oauth-connection!
     (make-oauth-connection
      (ctr domain #t)
      (oauth-connection-consumer-key conn)
      (oauth-connection-access-token conn)
      (oauth-signer-clone (oauth-connection-signer conn)))))
  
  (define-constant +upload-server+ "upload.twitter.com")
  
  (define (twitter-media-upload conn media-type data . options)
    (let-values (((parameters headers) (twitter-parameter&headers options)))
      (let ((changed (change-domain conn +upload-server+))
	    (type&subtype (mime-parse-content-type media-type)))
	(wrap-twitter-response
	 (send-multipart-request changed "/1.1/media/upload.json"
	  (list (make-mime-part
		 :content data
		 :type (car type&subtype)
		 :subtype (cadr type&subtype)
		 :transfer-encoding "binary"
		 :headers (make-content-disposision "media")))
	  parameters
	  headers)))))
  
  (define (ensure-upload-domain conn . opt)
    (define http-connection (oauth-connection-http-connection conn))
    (define server (http-connection-server http-connection))
    (if (string=? server +upload-server+)
	conn
	(apply change-domain conn +upload-server+ opt)))
		  
  (define (twitter-media-chunk-upload/init conn total-bytes media-type . opt)
    (let-values (((parameters headers) (twitter-parameter&headers opt)))
      (let ((conn (ensure-upload-domain conn)))
	(wrap-twitter-response
	 (send-post-request conn "/1.1/media/upload.json"
	  (cons* :command "INIT"
		 :total_bytes (number->string total-bytes)
		 :media_type media-type
		 parameters)
	  headers)))))

  (define-constant +default-buffer-size+ (* 1024 10))
  (define (twitter-media-chunk-upload/append conn media-id data
	     :key (segment-index 0)
		  (buffer-size +default-buffer-size+)
	     :allow-other-keys opt)
    (define buffer (make-bytevector buffer-size))
    (define (send-part conn data index parameters headers)
      (send-multipart-request conn "/1.1/media/upload.json"
       (list (make-mime-part
	      :content data
	      :type "application"
	      :subtype "octet-stream"
	      :transfer-encoding "binary"
	      :headers (cons `("content-length" (,(number->string (bytevector-length data))))
			     (make-content-disposision "media"))))
       (cons* :command "APPEND"
	      :media_id media-id
	      :segment_index (number->string index)
	      parameters)
       headers))
    (define (send conn data index parameters header)
      (let-values (((s h b) (send-part conn data index parameters header)))
	(unless (string-ref s 0 #\2)
	  (raise (condition
		  (make-http-error s h "")
		  (make-who-condition
		   'twitter-media-chunk-upload/append)
		  (make-message-condition "Failed to upload chunk"))))))
    (let-values (((parameters headers) (twitter-parameter&headers opt)))
      ;; we use HTTP/2 connection to reuse socket if it's not
      ;; converted yet.
      (let ((conn (ensure-upload-domain conn make-http2-connection)))
	(let loop ((i segment-index)
		   (n (get-bytevector-n! data buffer 0 buffer-size)))
	  (cond ((eof-object? n) media-id)
		((< n buffer-size)
		 (send conn (bytevector-copy buffer 0 n)  i parameters headers)
		 media-id)
		(else
		 (send conn buffer i parameters headers)
		 (loop (+ i 1)
		       (get-bytevector-n! data buffer 0 buffer-size))))))))

  (define (twitter-media-chunk-upload/finalize conn media-id . opt)
    (let-values (((parameters headers) (twitter-parameter&headers opt)))
      (let ((conn (ensure-upload-domain conn)))
	(wrap-twitter-response
	 (send-post-request conn "/1.1/media/upload.json"
			    (cons* :command "FINALIZE"
				   :media_id media-id
				   parameters)
			    headers)))))

  (define (twitter-media-chunk-upload conn media-type data
				      :key (buffer-size +default-buffer-size+)
				      :allow-other-keys opt)
    (define (get-size data)
      (if (string? data)
	  (file-size-in-bytes data)
	  (let ((pos (port-position data)))
	    (set-port-position! data 0 'end)
	    (rlet1 n (- (port-position data) pos)
	     (set-port-position! data pos)))))
    (define (get-port data)
      (if (string? data)
	  (open-file-input-port data)
	  data))
    (define (close-port port)
      (when (string? data) (close-input-port port)))
    (define new-conn (ensure-upload-domain conn make-http2-connection))
    (define (find-media-id json)
      (cdr (find (lambda (v) (string=? (car v) "media_id_string"))
		 (vector->list json))))
    (let* ((json (apply twitter-media-chunk-upload/init new-conn
			(get-size data) media-type opt))
	   (media-id (find-media-id json))
	   (port (get-port data)))
      (guard (e (else (close-port port) (raise e)))
	(apply twitter-media-chunk-upload/append new-conn json port
	       :buffer-size buffer-size opt)
	(close-port port))
      (rlet1 r (apply twitter-media-chunk-upload/finalize new-conn media-id opt)
	(close-oauth-connection! new-conn))))
    
  )
