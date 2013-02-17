#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/itr/TODO.org::*Airline%20example][Airline-example:1]]

(use alist-lib
     call-with-query
     debug
     define-record-and-printer
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
      ((? (takon-match? "no"))
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

(call-with-dynamic-fastcgi-query
 (lambda (query)
   (let ((user (query-client-any query 'user))
         (project (query-client-any query 'project))
         (dictum (query-client-any query 'dictum)))
     (let ((user (hash-table-update!
                   users
                   (cons user project)
                   identity
                   (lambda ()
                     (make-user
                      user
                      airline
                      ;; (hash-table-ref/default
                      ;;  projects
                      ;;  project
                      ;;  (lambda (user dictum) "Harro!"))
                      )))))
       (display-content-type-&c. 'json)
       (display ((user-progress user) user dictum))))))

;; Airline-example:1 ends here
