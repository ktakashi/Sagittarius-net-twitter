;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/twitter/v1.1.scm - Twitter 1.1 API library.
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

(library (net twitter v1.1)
    (export
     ;; Timeline
     twitter-mentions-timeline
     twitter-user-timeline
     twitter-home-timeline
     twitter-retweets-of-me
     ;; Tweet
     twitter-retweets
     twitter-show
     twitter-destroy
     twitter-update
     twitter-retweet
     twitter-oembed
     twitter-retweeters-ids
     ;; Search
     twitter-search-tweets
     ;; Friends & Followers
     twitter-friendships-no-retweeets-ids
     twitter-friends-ids
     twitter-followers-ids
     ;; Users
     twitter-users-lookup
     twitter-users-show
     twitter-users-search
     )
    (import (rnrs)
	    (net twitter util))

  (define-syntax check-at-least-one
    (syntax-rules ()
      ((_ who params ...)
       (unless (or params ...)
	 (assertion-violation 'who "must specify at least one of the parameters"
			      '(params ...))))))
  ;; Twitter APIs
  ;; timeline related
  (define (twitter-mentions-timeline token 
				     :key (count #f)
					  (since-id #f)
					  (max-id #f)
					  (trim-user #f)
					  (contributor-details #f)
					  (include-entities #f)
				     :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1.1/statuses/mentions_timeline"
	   (make-query-params count since-id max-id
			      trim-user contributor-details
			      include-entities) opts))

  (define (twitter-user-timeline token 
				 :key (user-id #f)
				      (screen-name #f)
				      (count #f)
				      (since-id #f)
				      (max-id #f)
				      (trim-user #f)
				      (exclude-entities #f)
				      (contributor-details #f)
				      (include-rts #f)
				 :allow-other-keys opts)
    (check-at-least-one user-id screen-name)
    (apply call/twitter-api token 'GET "/1.1/statuses/user_timeline"
	   (make-query-params user-id screen-name count since-id max-id
			      trim-user contributor-details
			      exclude-entities include-rts) opts))

  (define (twitter-home-timeline token
				 :key (count #f)
				      (since-id #f)
				      (max-id #f)
				      (trim-user #f)
				      (exclude-replies #f)
				      (contributor-details #f)
				      (include-entities #f)
				 :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1.1/statuses/home_timeline" 
	   (make-query-params count since-id max-id 
			      trim-user include-entities
			      exclude-replies contributor-details) opts))

  (define (twitter-retweets-of-me token
				  :key (count #f)
				       (since-id #f)
				       (max-id #f)
				       (trim-user #f)
				       (include-entities #f)
				       (include-user-entities #f)
				  :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1.1/statuses/retweets_of_me"
	   (make-query-params count since-id max-id
			      trim-user include-entities
			      include-user-entities) opts))

  ;; Tweets
  (define (twitter-retweets token id
			    :key (count #f) (trim-user #f)
			    :allow-other-keys opts)
    (apply call/twitter-api token 'GET (format "/1.1/statuses/retweets/~a" id)
	   (make-query-params count trim-user) opts))

  (define (twitter-show token id 
			:key (trim-user #f) 
			     (include-my-retweet #f)
			     (include-entities #f) 
			:allow-other-keys opts)
    (apply call/twitter-api token 'GET (format "/1.1/statuses/show/~a" id)
	   (make-query-params trim-user include-my-retweet 
			      include-entities) opts))

  (define (twitter-destroy token id 
			   :key (trim-user #f)
			   :allow-other-keys opts)
    (apply call/twitter-api token 'POST (format "/1.1/statuses/destroy/~a" id)
	   (make-query-params include-entities) opts))

  (define (twitter-update token message
			  :key (in-reply-to-status-id #f)
			       (lat #f) 
			       (long #f)
			       (place-id #f)
			       (display-coordinates #f)
			       (trim-user #f)
			  :allow-other-keys opts)
    (apply call/twitter-api token 'POST "/1.1/statuses/update"
	   `(("status" ,message)
	     ,@(make-query-params in-reply-to-status-id
				  lat long place-id display-coordinates
				  trim-user)) opts))

  (define (twitter-retweet token id
			   :key (trim-user #f)
			   :allow-other-keys opts)
    (apply call/twitter-api token 'POST (format "/1.1/statuses/retweet/~a" id)
	   (make-query-params trim-user) opts))
  
  ;; TODO update-with-media

  (define (twitter-oembed token
			  :key (id #f)
			       (url #f)
			       (maxwidth #f)
			       (hide-media #t)
			       (hide-thread #t)
			       (omit-script #t)
			       (align #f)
			       (related #f)
			       (lang #f)
			  :allow-other-keys opts)
    (check-at-least-one id url)
    (apply call/twitter-api token 'GET "/1.1/statuses/oembed"
	   (make-query-params id url maxwidth hide-thread hide-thread
			      omit-script align related lang) opts))

  (define (twitter-retweeters-ids token id
				  :key (cursor #f) (stringify-ids #f)
				  :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1.1/statuses/retweetters/ids"
	   (make-query-params id cursor stringify-ids) opts))

  ;; Search
  (define (twitter-search-tweets token q
				 :key (geocode #f)
				      (lang #f)
				      (locale #f)
				      (result-type #f)
				      (count #f)
				      (until #f)
				      (since-id #f)
				      (max-id #f)
				      (include-entities #f)
				      (callback #f)
				 :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1.1/search/tweets"
	   (make-query-params q geocode lang locale count since-id max-id
			      include-entities callback) opts))

  ;; Friends & Followers
  (define (twitter-friendships-no-retweeets-ids token :key (stringify-ids #f)
						:allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1.1/friendships/no_retweets/ids"
	   (make-query-params stringify-ids) opts))

  (define (twitter-friends-ids token
			       :key (user-id #f)
				    (screen-name #f)
				    (cursor #f)
				    (stringify-ids #f)
				    (count #f)
			       :allow-other-keys opts)
    (check-at-least-one user-id screen-name)
    (apply call/twitter-api token 'GET "/1.1/friends/ids"
	   (make-query-params user-id screen-name cursor stringify-ids count)
	   opts))

  (define (twitter-followers-ids token
				 :key (user-id #f)
				      (screen-name #f)
				      (cursor #f)
				      (stringify-ids #f)
				      (count #f)
				 :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1.1/followers/ids"
	   (make-query-params user-id screen-name cursor stringify-ids count)
	   opts))

  ;; TODO the rest...

  ;; Users
  ;; TODO add acount settings
  (define (twitter-users-lookup token
			       :key (screen-name #f) (user-id #f)
				    (include-entities #f)
			       :allow-other-keys opts)
    (check-at-least-one user-id screen-name)
    (apply call/twitter-api token 'GET "/1.1/users/lookup"
	   (make-query-params user-id screen-name include-entities) opts))
  (define (twitter-users-show token 
			     :key (user-id #f) 
				  (screen-name #f) 
				  (include-entities #f)
			     :allow-other-keys opts)
    (check-at-least-one user-id screen-name)
    (apply call/twitter-api token 'GET "/1.1/users/show"
	   (make-query-params user-id screen-name include-entities) opts))

  (define (twitter-users-search token q
				:key (page #f) 
				     (count #f)
				     (include-entities #f)
			       :allow-other-keys opts)
    (apply call/twitter-api token 'GET "/1.1/users/search"
	   (make-query-params q page count include-entities) opts))

)