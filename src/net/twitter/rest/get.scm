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
	    twitter:friends/ids
	    twitter:friends/list
	    twitter:friendships/incoming
	    twitter:friendships/lookup
	    twitter:friendships/no-retweets/ids
	    twitter:friendships/outgoing
	    twitter:friendships/show
	    twitter:geo/id
	    twitter:geo/reverse-geocode
	    twitter:geo/search
	    twitter:help/configuration
	    twitter:help/languages
	    twitter:help/privacy
	    twitter:help/tos
	    twitter:lists/list
	    twitter:lists/members
	    twitter:lists/members/show
	    twitter:lists/memberships
	    twitter:lists/ownerships
	    twitter:lists/show
	    twitter:lists/statuses
	    twitter:lists/subscribers
	    twitter:lists/subscribers/show
	    twitter:lists/subscriptions
	    twitter:media/upload@status
	    twitter:mutes/users/ids
	    twitter:mutes/users/list
	    twitter:saved-searches/list
	    twitter:saved-searches/show
	    twitter:search/tweets
	    twitter:statuses/home-timeline
	    twitter:statuses/lookup
	    twitter:statuses/mentions-timeline
	    twitter:statuses/retweeters/ids
	    twitter:statuses/retweets
	    twitter:statuses/retweets-of-me
	    twitter:statuses/show
	    twitter:statuses/user-timeline
	    twitter:trends/available
	    twitter:trends/closest
	    twitter:trends/place
	    twitter:users/lookup
	    twitter:users/profile-banner
	    twitter:users/search
	    twitter:users/show
	    twitter:users/suggestions
	    twitter:users/suggestions/slug
	    twitter:users/suggestions/slug/members
	    )
    (import (rnrs)
	    (rename (rfc oauth)
		    (oauth-request/header-authorization twitter-request))
	    (rfc uri)
	    (rfc http-connections)
	    (sagittarius)
	    (net twitter rest util))

  (define (compose-query-string uri parameter)
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
      (define (->name k uri) (datum->syntax k (twitter-uri->api-name uri)))
      (syntax-case x ()
	((k uri)
	 (with-syntax ((name (->name #'k #'uri)))
	   #'(define (name conn . opt)
	       (let-values (((parameter headers)
			     (twitter-parameter&headers opt)))
		 (wrap-twitter-response
		  (apply twitter-request conn 'GET
			 (compose-query-string uri parameter) headers))))))
	((k uri req1 req* ...)
	 (with-syntax ((name (->name #'k #'uri))
		       (((key . req) ...)
			(datum->syntax #'k (map keyword&id #'(req1 req* ...)))))
	   #'(define (name conn req ... . opt)
	       (let-values (((parameter headers)
			     (twitter-parameter&headers opt)))
		 (wrap-twitter-response
		  (apply twitter-request conn 'GET
			 (compose-query-string uri (append `(key ,req) ...
							   parameter))
			 headers)))))))))
  
  (define-twitter-get-api "/1.1/account/settings.json")
  (define-twitter-get-api "/1.1/account/verify_credentials.json")
  (define-twitter-get-api "/1.1/application/rate_limit_status.json")
  (define-twitter-get-api "/1.1/blocks/ids.json")
  (define-twitter-get-api "/1.1/blocks/list.json")
  (define-twitter-get-api "/1.1/collections/entries.json")
  (define-twitter-get-api "/1.1/collections/list.json")
  (define-twitter-get-api "/1.1/collections/show.json" id)
  (define-twitter-get-api "/1.1/direct_messages.json")
  (define-twitter-get-api "/1.1/direct_messages/sent.json")
  (define-twitter-get-api "/1.1/direct_messages/show.json")
  (define-twitter-get-api "/1.1/direct_messages/events/list.json")
  (define-twitter-get-api "/1.1/direct_messages/events/show.json" id)
  (define-twitter-get-api "/1.1/direct_messages/welcome_messages/list.json")
  (define-twitter-get-api "/1.1/direct_messages/welcome_messages/show.json")
  (define-twitter-get-api "/1.1/direct_messages/welcome_messages/rules/list.json")
  (define-twitter-get-api "/1.1/direct_messages/welcome_messages/rules/show.json" id)
  (define-twitter-get-api "/1.1/favorites/list.json")
  (define-twitter-get-api "/1.1/followers/ids.json")
  (define-twitter-get-api "/1.1/followers/list.json")
  (define-twitter-get-api "/1.1/friends/ids.json")
  (define-twitter-get-api "/1.1/friends/list.json")
  (define-twitter-get-api "/1.1/friendships/incoming.json")
  (define-twitter-get-api "/1.1/friendships/lookup.json")
  (define-twitter-get-api "/1.1/friendships/no_retweets/ids.json")
  (define-twitter-get-api "/1.1/friendships/outgoing.json")
  (define-twitter-get-api "/1.1/friendships/show.json")
  (define-twitter-get-api "/1.1/geo/reverse_geocode.json" lat long)
  (define-twitter-get-api "/1.1/geo/search.json")
  (define-twitter-get-api "/1.1/help/configuration.json")
  (define-twitter-get-api "/1.1/help/languages.json")
  (define-twitter-get-api "/1.1/help/privacy.json")
  (define-twitter-get-api "/1.1/help/tos.json")
  (define-twitter-get-api "/1.1/lists/list.json")
  (define-twitter-get-api "/1.1/lists/members.json")
  (define-twitter-get-api "/1.1/lists/members/show.json")
  (define-twitter-get-api "/1.1/lists/memberships.json")
  (define-twitter-get-api "/1.1/lists/ownerships.json")
  (define-twitter-get-api "/1.1/lists/show.json")
  (define-twitter-get-api "/1.1/lists/statuses.json")
  (define-twitter-get-api "/1.1/lists/subscribers.json")
  (define-twitter-get-api "/1.1/lists/subscribers/show.json")
  (define-twitter-get-api "/1.1/lists/subscriptions.json")
  (define-twitter-get-api "/1.1/mutes/users/ids.json")
  (define-twitter-get-api "/1.1/mutes/users/list.json")
  (define-twitter-get-api "/1.1/saved_searches/list.json")
  (define-twitter-get-api "/1.1/search/tweets.json" q)
  (define-twitter-get-api "/1.1/statuses/home_timeline.json")
  (define-twitter-get-api "/1.1/statuses/lookup.json" id)
  (define-twitter-get-api "/1.1/statuses/mentions_timeline.json")
  (define-twitter-get-api "/1.1/statuses/retweeters/ids.json" id)
  (define-twitter-get-api "/1.1/statuses/retweets_of_me.json")
  (define-twitter-get-api "/1.1/statuses/show.json" id)
  (define-twitter-get-api "/1.1/statuses/user_timeline.json")
  (define-twitter-get-api "/1.1/trends/available.json")
  (define-twitter-get-api "/1.1/trends/closest.json" lat long)
  (define-twitter-get-api "/1.1/trends/place.json" id)
  (define-twitter-get-api "/1.1/users/profile_banner.json")
  (define-twitter-get-api "/1.1/users/search.json" q)
  (define-twitter-get-api "/1.1/users/show.json")
  (define-twitter-get-api "/1.1/users/suggestions.json")

  ;; encouraged to be POST if it's big
  ;; (define-twitter-get-api "/1.1/users/lookup.json")
  (define (twitter:users/lookup conn . opt)
    (define uri "/1.1/users/lookup.json")
    (define (use-post? ids screen-names)
      (let ((check (cadr (or ids screen-names))))
	;; well, 50 is enough?
	(> (string-length check) 50)))
    (let-values (((parameters header) (twitter-parameter&headers opt)))
      (let ((ids (memq :user_id parameters))
	    (screen-names (memq :screen_name parameters)))
	(when (and ids screen-names)
	  (assertion-violation 'twitter:users/lookup
			       "user_id and screen_name must not be together"))
	(wrap-twitter-response
	 (if (use-post? ids screen-names)
	     (twitter-send-post-request conn uri parameters header)
	     (apply twitter-request conn 'GET
		    (compose-query-string uri parameters) header))))))
  
  ;; different name convension
  (define (twitter:media/upload@status conn media-id . opt)
    (let-values (((parameter header) (twitter-parameter&headers opt)))
      (wrap-twitter-response
       (apply twitter-request
	      (twitter-connection-ensure-domain conn +twitter-upload-server+)
	      'GET
	      (compose-query-string "/1.1/media/upload.json"
				    `(:command "STATUS" :media_id ,media-id))
	      header))))
  
  ;; path param
  (define (twitter:geo/id conn place-id . opt)
    (let-values (((parameter header) (twitter-parameter&headers opt)))
      (wrap-twitter-response
       (apply twitter-request conn 'GET
	      (format "/1.1/geo/id/~a.json" (uri-encode-string place-id))
	      header))))
  (define (twitter:saved-searches/show conn id . opt)
    (let-values (((parameter header) (twitter-parameter&headers opt)))
      (wrap-twitter-response
       (apply twitter-request conn 'GET
	      (format "/1.1/saved_searches/show/~a.json" (uri-encode-string id))
	      header))))
  (define (twitter:statuses/retweets conn id . opt)
    (define uri (format "/1.1/statuses/retweets/~a.json"
			(uri-encode-string id)))
    (let-values (((parameter header) (twitter-parameter&headers opt)))
      (wrap-twitter-response
       (apply twitter-request conn 'GET
	      (if (null? parameter)
		  uri
		  (apply compose-query-string uri parameter))
	      header))))

  (define (twitter:users/suggestions/slug conn slug . opt)
    (define uri (format "/1.1/users/suggestions/~a.json"
			(uri-encode-string slug)))
    (let-values (((parameter header) (twitter-parameter&headers opt)))
      (wrap-twitter-response
       (apply twitter-request conn 'GET
	      (if (null? parameter)
		  uri
		  (apply compose-query-string uri parameter))
	      header))))
  (define (twitter:users/suggestions/slug/members conn slug . opt)
    (define uri (format "/1.1/users/suggestions/~a/members.json"
			(uri-encode-string slug)))
    (let-values (((parameter header) (twitter-parameter&headers opt)))
      (wrap-twitter-response
       (apply twitter-request conn 'GET
	      (if (null? parameter)
		  uri
		  (apply compose-query-string uri parameter))
	      header))))
)
