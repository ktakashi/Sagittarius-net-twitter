;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/twitter/v1.scm - Twitter 1.0 API library.
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

;; this is obsolated but for history :)
(library (net twitter v1)
    (export) ;; export syntax is defined in library
    (import (rnrs)
	    (net twitter util))

  ;; define and export high level APIs.
  ;; To support multiple type of Twitter API types, we need to define
  ;; almost the same procedures. To avoid this stupid repeation,
  ;; we define the base API and generate high level API from it. It is
  ;; actually just append suffix such as '/sxml'.
  (define-syntax define-api
    (lambda (x)
      (define (->name fmt k name)
	(string->symbol (format fmt (syntax->datum name))))
      (define (parse-formal formals)
	(let loop ((formals formals) (args '()))
	  (syntax-case formals ()
	    (() (values args '()))
	    ((a . d) (loop #'d (cons #'a args)))
	    (x (values (reverse! args) #'x)))))
      (syntax-case x ()
	((k (name . formals))
	 (let-values (((req opt) (parse-formal #'formals)))
	   (with-syntax (((args ...) (datum->syntax #'k req))
			 (opt  (datum->syntax #'k opt))
			 (sxml-name (->name "~a/sxml" #'k #'name))
			 (json-name (->name "~a/json" #'k #'name)))
	     #'(begin
		 (define (sxml-name args ... . opt)
		   (apply name args ... :type 'xml opt))
		 (define (json-name args ... . opt)
		   (apply name args ... :type 'json opt))
		 (export sxml-name json-name))))))))
		       
  ;; Twitter APIs
  ;; timeline related
  (define (twitter-public-timeline :key (trim-user #f) (include-entities #f)
				   :allow-other-keys opts)
    (apply call/twitter-api #f 'GET "/1/statuses/public_timeline"
	   (make-query-params trim-user include-entities) opts))

  (define-api (twitter-public-timeline . opts))

  (define (twitter-home-timeline token
				 :key (since-id #f) (max-id #f)
				      (count #f) (page #f) (trim-user #f)
				      (include-rts #f) (include-entities #f)
				      (exclude-replies #f)
				      (contributor-details #f)
				 :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1/statuses/home_timeline" 
	   (make-query-params since-id max-id count page 
			      trim-user include-rts include-entities
			      exclude-replies contributor-details) opts))
  (define-api (twitter-home-timeline token . opts))

  (define (twitter-friends-timeline token
				    :key (since-id #f) (max-id #f)
					 (count #f) (page #f) (trim-user #f)
					 (include-rts #f) (include-entities #f)
					 (exclude-replies #f)
					 (contributor-details #f)
				    :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1/statuses/friends_timeline"
	   (make-query-params since-id max-id count page
			      trim-user include-rts include-entities
			      exclude-replies contributor-details) opts))
  (define-api (twitter-friends-timeline token . opts))

  (define (twitter-mentions token
			    :key (since-id #f) (max-id #f)
				 (count #f) (page #f) (trim-user #f)
				 (include-rts #f) (include-entities #f)
				 (contributor-details #f)
			    :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/statuses/mentions"
	   (make-query-params since-id max-id count page
			      trim-user include-rts include-entities
			      contributor-details) opts))
  (define-api (twitter-mentions token . opts))

  (define (twitter-retweeted-by-me token
				   :key (screen-name #f) (id #f) (count #f)
				   (since-id #f) (max-id #f) (page #f)
				   (trim-user #f) (include-entities #f)
				   :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1/statuses/retweeted_by_me"
	   (make-query-params screen-name id count since-id max-id page
			      trim-user include-entities) opts))
  (define-api (twitter-retweeted-by-me token . opts))

  (define (twitter-retweeted-to-me token
				   :key (count #f) (since-id #f) (max-id #f)
				   (page #f) (trim-user #f)
				   (include-entities #f)
				   :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1/statuses/retweeted_to_me"
	   (make-query-params count since-id max-id page
			      trim-user include-entities) opts))
  (define-api (twitter-retweeted-to-me token . opts))

  (define (twitter-retweets-of-me token
				  :key (count #f) (since-id #f) (max-id #f)
				  (page #f) (trim-user #f) (include-entities #f)
				  :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1/statuses/retweets_of_me"
	   (make-query-params count since-id max-id page
			      trim-user include-entities) opts))
  (define-api (twitter-retweets-of-me token . opts))

  (define (twitter-retweeted-by-user token
				     :key (screen-name #f) (id #f) (count #f)
				     (since-id #f) (max-id #f) (page #f)
				     (trim-user #f) (include-entities #f)
				     :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1/statuses/retweeted_by_user"
	   (make-query-params screen-name id count since-id max-id page
			      trim-user include-entities) opts))
  (define-api (twitter-retweeted-by-user token . opts))

  (define (twitter-retweeted-to-user token
				     :key (screen-name #f) (id #f) (count #f)
				     (since-id #f) (max-id #f)
				     (page #f) (trim-user #f)
				     (include-entities #f)
				     :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1/statuses/retweeted_to_user"
	   (make-query-params screen-name id count since-id max-id page
			      trim-user include-entities) opts))
  (define-api (twitter-retweeted-to-user token . opts))

  ;; tweets
  (define (twitter-show token id 
			:key (trim-user #f) (include-entities #f)
			:allow-other-keys opts)
    (apply call/twitter-api token 'GET (format "/1/statuses/show/~a" id)
	   (make-query-params trim-user include-entities) opts))
  (define-api (twitter-show token id . opts))

  (define (twitter-update token message
			  :key (in-reply-to-status-id #f)
			       (lat #f) (long #f) (place-id #f)
			       (display-coordinates #f)
			       (include-entities #f) (trim-user #f)
			  :allow-other-keys opts)
    (apply call/twitter-api token 'POST "/1/statuses/update"
	   `(("status" ,message)
	     ,@(make-query-params in-reply-to-status-id
				  lat long place-id display-coordinates
				  in-reply-to-status-id trim-user))
	   opts))

  (define-api (twitter-update token message . opts))

  (define (twitter-destroy token id 
			   :key (include-entities #f) (trim-user #f)
			   :allow-other-keys opts)
    (apply call/twitter-api token 'POST (format "/1/statuses/destroy/~a" id)
	   (make-query-params include-entities trim-user) opts))
  (define-api (twitter-destroy token id . opts))

  (define (twitter-retweeted-by token id
				:key (count #f) (page #f) 
				:allow-other-keys opts)
    (apply call/twitter-api token 'GET (format "/1/statuses/~a/retweeted_by" id)
	   (make-query-params count page) opts))
  (define-api (twitter-retweeted-by token id . opts))

  (define (twitter-retweet token id . opts)
    (apply call/twitter-api token 'POST (format "/1/statuses/retweet/~a" id)
	   '() opts))
  (define-api (twitter-retweet token id . opts))

  (define (twitter-retweets token id
			    :key (count #f) (trim-user #f) (include-entities #f)
			    :allow-other-keys opts)
    (apply call/twitter-api token 'GET (format "/1/statuses/retweets/~a" id)
	   (make-query-params count trim-user include-entities) opts))
  (define-api (twitter-retweets token id . opts))

  (define (twitter-retweeted-by-ids token id
				    :key (count #f) (page #f) (stringify-ids #f)
				    :allow-other-keys opts)
    (apply call/twitter-api token 'GET
	   (format "/1/statuses/~a/retweeted_by/ids" id)
	   (make-query-params count page stringify_ids) opts))
  (define-api (twitter-retweeted-by-ids token id . opts))

  ;; user methods
  ;; token can be #f
  (define (twitter-user-show token user-id screen-name
			     :key (include-entities #f)
			     :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1/users/show"
	   (make-query-params user-id screen-name include-entities) opts))
  (define-api (twitter-user-show token . opts))

  ;; token can be #f
  (define (twitter-user-lookup token
			       :key (user-id #f) (screen-name #f)
				    (include-entities #f)
			       :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1/users/lookup"
	   (make-query-params user-id screen-name include-entities) opts))
  (define-api (twitter-user-lookup token . opts))

  (define (twitter-user-search token q
			       :key (page #f) (per-page #f)
				    (include-entities #f)
			       :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1/users/search"
	   (make-query-params q page per-page include-entities) opts))
  (define-api (twitter-user-search token . opts))

  ;; friends followers
  (define (twitter-followers-ids token
				 :key (user-id #f) (screen-name #f)
				 (cursor #f) (stringify-ids #f)
				 :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1/followers/ids"
	   (make-query-params user-id screen-name cursor stringify-ids) opts))
  (define-api (twitter-followers-ids token . opts))

  (define (twitter-friends-ids token
			       :key (user-id #f) (screen-name #f)
			       (cursor #f) (stringify-ids #f)
			       :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1/friends/ids"
	   (make-query-params user-id screen-name cursor stringify-ids) opts))
  (define-api (twitter-friends-ids token . opts))
)