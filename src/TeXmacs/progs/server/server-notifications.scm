
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-notifications.scm
;; DESCRIPTION : Push notifications and notification services
;; COPYRIGHT   : (C) 2020  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-notifications)
  (:use (server server-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Push/Pull Notifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-pending-notification uid kind data)
  (let* ((nid (with-time-stamp #t
                (db-create-entry `(("type" "notification")
                                   ("owner" ,uid)
                                   ;("status" "unread")
                                   ("kind" ,kind)
                                   ("data" ,data))))))
    nid))

(define (get-user-notifications uid kind)
  (with query `(("type" "notification")
                ("owner" ,uid))
    (db-search (if (== kind 'all) query (append query `(("kind" ,kind)))))))

(define (push-notification to kind data)
  ;; Store a notification for the user with pseudo 'to' and push it
  ;; to this user if logged in
  (with nid (add-pending-notification (server-find-user to) kind data)
    (when (pseudo-logged? to)
      (server-remote-eval
        (pseudo-logged? to)
        `(client-push-notifications ,nid ,(db-get-entry nid))
        (lambda (ok?) (noop))))))

(tm-define (server-push-message msg)
  (with (action pseudo full-name date doc to mid) msg
    (push-notification to 'message msg)))

(tm-define (server-push-chat-notification to room msg mid)
  ;; Notify 'to' of the new message 'msg' with identifier 'mid' in the
  ;; chat room 'room'; database fields hold strings, so the notification
  ;; records the name of the room, which the client opens
  (push-notification to 'chat room))

(tm-define (server-clean-user-notifications uid kind)
  (let* ((notifs (get-user-notifications uid kind)))
    (for-each db-remove-entry notifs)))

(tm-service (remote-pending-notifications)
  (if (!= (get-preference "server service notifications") "on")
    (server-return envelope "not allowed")
    (let* ((uid (server-get-user envelope))
           (notifs (map (lambda (nid) `(,nid ,(db-get-entry nid)))
                        (get-user-notifications uid 'all))))
      (server-return envelope notifs))))

(tm-service (remote-ack-notifications kind)
  (if (!= (get-preference "server service notifications") "on")
    (server-return envelope "not allowed")
    (let* ((uid (server-get-user envelope)))
      (server-clean-user-notifications uid kind)
      (server-return envelope "done"))))

