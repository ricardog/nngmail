(eval-and-compile
  (require 'nnheader)
  ;; In Emacs 24, `open-protocol-stream' is an autoloaded alias for
  ;; `make-network-stream'.
  (unless (fboundp 'open-protocol-stream)
    (require 'proto-stream)))

(eval-when-compile
  (require 'cl))

(require 'nnheader)
(require 'gnus-util)
(require 'gnus)
(require 'nnoo)
(require 'netrc)
(require 'utf7)
(require 'tls)
(require 'parse-time)
(require 'nnmail)

(nnoo-declare nngmail)

(defvoo nngmail-server "localhost"
  "The address of the gmail proxy.")

(defvoo nngmail-user nil
  "Username to use for authentication to the IMAP server.")

(defvoo nngmail-port 5000
  "The port the gmail proxy listens on.")

(defvoo nngmail-base-url
    (format "http://%s:%d/api/v1.0" nngmail-server nngmail-port)
  "The base URL for talking to the nngmail server.")

(defvoo nngmail-split-methods nil
  "How mail is split.
Uses the same syntax as `nnmail-split-methods'.")

(defvoo nngmail-split-fancy nil
  "Uses the same syntax as `nnmail-split-fancy'.")

(defvoo nngmail-header-format "nov")

(defun nngmail-decode-gnus-group (group)
  (decode-coding-string group 'utf-8))

(defun nngmail-encode-gnus-group (group)
  (encode-coding-string group 'utf-8))

(defvar nngmail-last-account-id nil)

(defvar nngmail-status-string "")

(defconst nngmail-src-dir (file-name-directory load-file-name))

(defun nngmail-start-server ()
  "Start an nnserver to sync the user's gmail account(s)."
  (let* ((program (expand-file-name "~/.pyenv/shims/nnserver"))
	 (buf (generate-new-buffer (concat "*nngmail*")))
	 (config-file (expand-file-name "config.yaml" nngmail-src-dir))
	 (some-other-stuff "blah blah blah")
	 (proc (start-process "nngmail" buf program config-file)))
    (message "nngmail sync daemon started")))

(defvar nngmail-servers ())

(defun nngmail-get-account-params (elem)
  (let ((nickname (plist-get elem 'nickname))
	(email (plist-get elem 'email))
	(id (plist-get elem 'id)))
    (cons nickname (list id email))))

(defun nngmail-get-account (nickname)
  (cdr (assoc-string nickname nngmail-servers)))

(defun nngmail-get-account-email (nickname)
  (elt (nngmail-get-account nickname) 1))

(defun nngmail-get-account-id (nickname)
  (elt (nngmail-get-account nickname) 0))

(defun nngmail-parse-accounts ()
  (let ((json-object-type 'plist)
        (json-key-type 'symbol)
        (json-array-type 'vector))
    (let* ((result (json-read))
	   (accounts (plist-get result 'accounts))
	   )
      ;; Do something with RESULT here
      (seq-map
       (lambda (elem)
	 (push (nngmail-get-account-params elem) nngmail-servers))
       accounts)
      )))

(defun nngmail-url-for (resource &rest id method)
  "Generate a URL to probe the resource."
  (format "%s/%ss/" nngmail-base-url resource))

(defun nngmail-get-accounts ()
  (let ((buffer (url-retrieve-synchronously (nngmail-url-for "account") t)))
    (setq nngmail-servers ())
    (with-current-buffer buffer
      (goto-char url-http-end-of-headers)
      (nngmail-parse-accounts buffer))))

(defun nngmail-touch-server (server)
  (setq nngmail-last-account-id (nngmail-get-account server)))
	
(deffoo nngmail-open-server (server &rest definitions)
  "Verify the nngmail server syncs the account."
  (interactive)
  (nngmail-get-accounts)
  (unless (nngmail-get-account server)
    (error
     (format "You are not synching the account %s" server)))
  (and
   (assoc-string "email" definitions)
   (let ((email-def (assoc-string "email" definitions))
	 (email-ser (nngmail-get-account-email server)))
     (unless
       (string-equal email-ser email-def)
       (error (format "Email address mismatch %s != %s"
		   email-ser email-def)))))
  (message (format "nngmail: opened server '%s'" server))
  (nngmail-touch-server server))

(deffoo nngmail-close-server (server)
  "Close connection to server.  Removes the server from the
accounts alist."
  (setq nngmail-servers
	(delq (assoc-string server nngmail-servers) nngmail-servers))
  (message (format "nngmail: closed server '%s'" server))
  (and (eq nngmail-last-account-id
	   (nngmail-get-account-id server))
       (setq nngmail-last-acount-id nil)))

(deffoo nngmail-request-close ()
  "Close connection to all servers.  Removes all entries from the
accounts alist."
  (setq nngmail-servers ()
	nngmail-last-account-id nil)
  (and
   (message (format "nngmail: closed server '%s'" server))
   nil))

(deffoo nngmail-server-opened (&optional server)
  "Returns whether the server exists in the accounts alist"
  (nngmail-get-account-id server))

(deffoo nngmail-retrieve-headers (articles &optional group server fetch-old)
  "Retrieve headers for the specified group (label)."
  nil
  )

(deffoo nngmail-status-message (&optional server)
  nngmail-status-string)

(deffoo nngmail-request-article (article &optional group server to-buffer)
  "Issue an HTTP request for the raw article body."
  (and
   (cons group article)
   nil))

(deffoo nngmail-request-group (group &optional server fast info)
  "Retrieve information about GROUP."
  ;;; 211 56 1000 1059 ifi.discussion
  nil)
  
(deffoo nngmail-close-group (group &optional server)
  "Close the group.  A nop."
  nil)

(deffoo nngmail-request-list (&optional server)
  "Return a list of all groups available on SERVER."
  ;;; An example of a server with two groups
  ;;;
  ;;; ifi.test 0000002200 0000002000 y
  ;;; ifi.discussion 3324 3300 n
  nil)

(deffoo nngmail-request-post (&optional server)
  (setq nngmail-status-string "Read-only server")
  nil)

(deffoo nngmail-request-type (group &optional article)
  'mail)

(defvar nngmail-mark-alist
  '((read "UNREAD")
    (tick "FLAGGED")
    (reply "Answered")
    (expire "expired")
    (dormant "dormant")
    (score "score")
    (save "save")
    (download "download")
    (forward "forward")))

(deffoo nngmail-request-set-mark (group action &optional server)
  "Set/remove/add marks on articles."
  ;;; ACTION is a list of mark setting requests, having this format:
  ;;; (RANGE ACTION MARK)
  ;;; Gnus marks are:

  nil)

(deffoo nngmail-request-scan (&optional group server)
  "Check for new articles.  If possible only on GROUP (although
that makes no sense for this backend since new (unread) mail will
appear in INBOX."
  nil)

(deffoo nngmail-request-newsgroups (date &optional server)
  "Returns a list of newsgroups created after DATE."
  nil)

(deffoo nngmail-request-expire-articles (articles &optional group server force)
  "Expire articles."
  nil)

(deffoo nngmail-request-move-article (article group server accept-form
					      &optional last)
  "Move article to a new group."
  nil)

(deffoo nngmail-request-accept-article (group &optional server last)
  "Used for respooling?"
  nil)

(deffoo nngmail-request-delete-group (group force &optional server)
  ""
  nil)

(deffoo nngmail-request-rename-group (group new-name &optional server)
  ""
  nil)


(provide 'nngmail)
;;; nngmail.el ends here
