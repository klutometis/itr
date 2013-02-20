#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/itr/TODO.org::*Airline%20example][Airline-example:1]]

(use alist-lib
     call-with-query
     debug
     define-record-and-printer
     http-client
     matchable
     medea)

;;; Some notion of expiry.
(define-record-and-printer user
  id
  progress)

(define users (make-hash-table))

(define-record-and-printer airline-user
  record)

(define airline-users (make-hash-table))

(define (airline-user-ref user)
  (hash-table-ref airline-users (user-id user)))

(define (token-match? token)
  (lambda (dictum)
    (member token (map string-downcase (string-tokenize dictum)))))

(define (airline-charge-card? user dictum)
  (let ((airline-user (airline-user-ref user)))
    (match dictum
      ((? (token-match? "yes"))
       (user-progress-set! user airline)
       "Thanks; your flight is changed. Can I help you with anything else?")
      ((? (token-match? "no"))
       (user-progress-set! user airline)
       "Your flight will stay the same; can I help you with anything else?")
      (_
       "Sorry; can I charge your card?"))))

(define (airline-change-flight user dictum)
  (let ((airline-user (airline-user-ref user)))
    (user-progress-set! user airline-charge-card?)
    (format "Changing your flight to ~a will cost $45.90; can I charge your card?" dictum)))

(define (airline-get-record user dictum)
  (let ((airline-user (airline-user-ref user)))
    (airline-user-record-set! airline-user dictum)
    (user-progress-set! user airline-change-flight)
    "Where would you like to change your flight to?"))

;;; Need labels, probably, for loops.
(define (airline user dictum)
  (let ((airline-user
         (hash-table-update!
          airline-users
          (user-id user)
          identity
          (lambda () (make-airline-user #f)))))
    dictum
    (match dictum
      ((? (token-match? "change"))
       (user-progress-set! user airline-get-record)
       "What is your record locator?")
      (_ "Sorry; can I help you change your flight?"))))

(define projects
  (alist->hash-table `(("1" . ,airline))))

(define (crunch user dictum)
  (let ((metadata
         (with-input-from-request
          (format
           "http://staging.crunchr.co/api/texthacker/user?phone=~a"
           (user-id user))
          #f
          read-json)))
    (if (vector? metadata)
        (values "It looks like you're a new customer; I'll forward you to an agent."
                "agent")
        (match dictum
          ((? (token-match? "reorder"))
           (let ((user (alist-ref metadata 'user))
                 (order (alist-ref metadata 'order)))
             (let ((restaurant (alist-ref order '_restaurant)))
               (values (format "Hey ~a, good to see you again; I'll reorder from ~a and charge you $~a."
                               (car (string-tokenize (alist-ref user 'name)))
                               (alist-ref restaurant 'name)
                               (alist-ref order 'price))
                       "customer"))))
          ((or (? (token-match? "where"))
               (? (token-match? "status")))
           (values "I'll forward you to an agent that can help you check the status of your order."
                   "agent"))
          (_ (values "Sorry, I didn't understand you. Text \"agent\" to get to a real person."
                     "customer"))))))

(call-with-dynamic-fastcgi-query
 (lambda (query)
   (let ((user (query-client-any query 'user))
         (project (query-client-any query 'project))
         (dictum (query-client-any query 'dictum)))
     (display-content-type-&c. 'json)
     (when (and user project dictum)
       (let ((user (hash-table-update!
                    users
                    (cons user project)
                    identity
                    (lambda ()
                      (make-user
                       user
                       crunch
                       ;; (hash-table-ref/default
                       ;;  projects
                       ;;  project
                       ;;  (lambda (user dictum) "Harro!"))
                       )))))
         (receive (response recipient)
           ((user-progress user) user dictum)
           (write-json `((response . ,response)
                         (recipient . ,recipient)))))))))

;; Airline-example:1 ends here
