(eval-and-compile
  (require 'nnheader)
  ;; In Emacs 24, `open-protocol-stream' is an autoloaded alias for
  ;; `make-network-stream'.
  (unless (fboundp 'open-protocol-stream)
    (require 'proto-stream)))

(eval-when-compile
  (require 'cl))

(require 'ht)
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
(gnus-declare-backend "nngmail" 'mail 'address)

(defvoo nngmail-host "localhost"
  "The address of the gmail proxy.")

(defvoo nngmail-user nil
  "Username to use for authentication to the IMAP server.")

(defvoo nngmail-port 5000
  "The port the gmail proxy listens on.")

(defvoo nngmail-base-url
    (format "http://%s:%d/api/v1.0" nngmail-host nngmail-port)
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
(defvar nngmail-last-account nil)

(defvar nngmail-status-string "")

(defvar nngmail-servers ()
  "An alist of accounts the server knows about.
What I call an account in the server is what gnus calls a server.  This list has all the accounts the server we connect to synchs.")

(defun nngmail-get-account (nickname)
  (cdr (assoc nickname nngmail-servers)))

(defun nngmail-get-account-id (nickname)
  (cdr (assq 'id (nngmail-get-account nickname))))

(defun nngmail-get-account-email (nickname)
  (cdr (assq 'email (nngmail-get-account nickname))))

(defun nngmail-get-account-groups (nickname)
  (cdr (assq 'groups (nngmail-get-account nickname))))

(defun nngmail-get-account-message (nickname)
  (cdr (assq 'message (nngmail-get-account nickname))))

(defun nngmail-get-account-group (nickname)
  (cdr (assq 'group (nngmail-get-account nickname))))

(defun nngmail-set-account-groups (nickname groups)
  (setcdr (assoc 'groups (assoc nickname nngmail-servers)) groups))

(defun nngmail-set-account-group (nickname group)
  (setcdr (assoc 'group (assoc nickname nngmail-servers)) group))

(defun nngmail-get-group-min (nickname group)
  (cdr (assq 'min (ht-get (nngmail-get-groups nickname) group))))

(defun nngmail-get-group-max (nickname group)
  (cdr (assq 'max (ht-get (nngmail-get-groups nickname) group))))

(defun nngmail-get-group-count (nickname group)
  (cdr (assq 'count (ht-get (nngmail-get-groups nickname) group))))

(defun nngmail-get-error-string (response)
  (plist-get response 'error))
  
(defun nngmail-get-account-params (elem)
  (let ((nickname (plist-get elem 'nickname))
	(email (plist-get elem 'email))
	(id (plist-get elem 'id))
	(groups (ht)))
    (cons nickname `((id      . ,id)
		     (email   . ,email)
		     (groups  . ,groups)
		     (message . nil)
		     (group   . nil)))))

(defun nngmail-get-group-params (elem)
  (let ((id (plist-get elem 'id))
	(name (plist-get elem 'name))
	(google_id (plist-get elem 'gid))
	(min (plist-get elem 'min))
	(max (plist-get elem 'max))
	(count (plist-get elem 'count))
	)
    (cons name `((id . ,id)
		 (google_id . ,google_id)
		 (min . ,min)
		 (max . ,max)
		 (count . ,count)))))

(defun nngmail-url-for (resource &optional id account-id args)
  "Generate a URL to probe the resource."
  (let ((base-url (if account-id
		      (format "%s/accounts/%d" nngmail-base-url account-id)
		    nngmail-base-url))
	(url-args (mapconcat (function (lambda (value)
					 (format "%s=%s" (car value)
						 (cdr value))))
			     args "&")))
    (if id
	(format "%s/%ss/%d?%s" base-url resource id url-args)
      (format "%s/%ss/?%s" base-url resource url-args))))

(defun nngmail-handle-response ()
  "Handle the response from a `url-retrieve-synchronously' call.
 Parse the HTTP response and throw if an error occurred.  The url
 package seems to require extra processing for this.  This should
 be called in a `save-excursion', in the download buffer.  It
 will move point to somewhere in the headers."
   ;; We assume HTTP here.
   (require 'url-http)
   (let ((response (url-http-parse-response)))
     (when (or (< response 200) (>= response 300))
       (let ((json-object-type 'plist)
	     (json-key-type 'symbol)
	     (json-array-type 'vector))
	 (goto-char (point-min))
	 (re-search-forward "^$")
	 (setq nngmail-status-string
	       (nngmail-get-error-string (json-read))))
       (error "Error: nngmail server responded with error"))))

(defun nngmail-parse-json (buffer)
  (with-current-buffer buffer
    (nngmail-handle-response)
    (goto-char url-http-end-of-headers)
    (let* ((json-object-type 'plist)
	   (json-key-type 'symbol)
	   (json-array-type 'vector))
      (json-read))))

(defmacro safe-parse (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
	 (condition-case ex
	     (setq retval (progn ,fn))
	   ('error
	    (message (format "Caught exception: [%s]" ex))
	    (nnheader-report 'nngmail "Could not request resource")
	    (setq retval (cons 'exception ex))))
	 retval)
     ,@clean-up))

(defun nngmail-fetch-resource (resource &optional id account-id args)
  "Retrieve a resource from the nngmail server."
  (let* ((url (nngmail-url-for resource id account-id args))
	 (buffer (condition-case ex
		     (url-retrieve-synchronously url t)
		   ('file-error
		    (message (format "Connection error fetching %s" url))
		    (setq nngmail-status-string
			  (format "Connection error fetching %s" url))
		    (nnheader-report
		     'nngmail (format "Connection error fetching %s" url))
		    (setq retval nil))))
	 (response (if (bufferp buffer)
		       (safe-parse
			(nngmail-parse-json buffer)
			(kill-buffer buffer)
			))))
    response))

(defun nngmail-get-accounts ()
  "Get a list of accounts, with their respective ID's, nicknames,
and email addresses, from the server.  The list of accounts is
store in `nngmail-servers` for fast access."
  (let* ((resource (nngmail-fetch-resource "account"))
	 (accounts (plist-get resource 'accounts)))
    (seq-map
     (lambda (elem)
       (push (nngmail-get-account-params elem) nngmail-servers))
     accounts)
    ))

(defun nngmail-get-groups (server)
  "Get a list of groups/labels, with their respective ID's, nicknames,
and email addresses, from the server.  The list of accounts is
store in `nngmail-servers` for fast access."
  (message (format "in nngmail-get-groups for %s" server))
  (let* ((groups (nngmail-get-account-groups server))
	 (account-id (nngmail-get-account-id server))
	 (resource (nngmail-fetch-resource "label" nil account-id
					   '((info . 1))))
	 (data (plist-get resource 'labels))
	 (tmp ()))
    (seq-map
     (lambda (elem)
       (push (nngmail-get-group-params elem) tmp))
     data)
    (nngmail-set-account-groups server (ht-from-alist tmp))
    ))

(defun nngmail-touch-server (server)
  (when (nngmail-get-account-id server)
    (setq nngmail-last-account-id (nngmail-get-account server)
	  nngmail-last-account server)))

(defun nngmail-change-group (server group)
  (message (format "in nngmail-change-groups for %s %s" server group))
  (and (ht-get (nngmail-get-account-groups server) group)
       (nngmail-set-account-group server group)))

;;;
;;; Required functions in gnus back end API
;;;
(deffoo nngmail-open-server (server &rest definitions)
  "Verify the nngmail server syncs the account."
  (message (format "in nngmail-open-server for %s" server))
  (nngmail-get-accounts)
  (if (not (nngmail-get-account server))
      (nnheader-report
       'nngmail (format "You are ntot syncing %s" server)))
  (progn
    (and
     definitions
     (assoc-string "email" definitions)
     (let ((email-def (assoc-string "email" definitions))
	   (email-ser (nngmail-get-account-email server)))
       (unless
	   (string-equal email-ser email-def)
	 (error (format "Email address mismatch %s != %s"
			email-ser email-def)))))
    (message (format "nngmail: opened server '%s'" server))
    (nngmail-touch-server server)))

(deffoo nngmail-close-server (server)
  "Close connection to server.  Removes the server from the
accounts alist."
  (message (format "in nngmail-close-server for %s" server))
  (setq nngmail-servers
	(delq (assoc-string server nngmail-servers) nngmail-servers))
  (message (format "nngmail: closed server '%s'" server))
  (and (eq nngmail-last-account-id
	   (nngmail-get-account-id server))
       (setq nngmail-last-account-id nil
	     nngmail-last-account nil)))

(deffoo nngmail-request-close ()
  "Close connection to all servers.  Removes all entries from the
accounts alist."
  (message (format "in nngmail-request-close"))
  (setq nngmail-servers ()
	nngmail-last-account-id nil
	nngmail-last-account nil
	nngmail-status-string nil))

(deffoo nngmail-server-opened (&optional server)
  "Returns whether the server exists in the accounts alist"
  (nngmail-touch-server server))

(deffoo nngmail-status-message (&optional server)
  (or (nngmail-get-account-message (or server nngmail-last-account))
      nngmail-status-string))

(deffoo nngmail-request-article (article &optional group server to-buffer)
  "Issue an HTTP request for the raw article body."
  (when group
    (setq group (nngmail-decode-gnus-group group)))
  (when (stringp article)
    (let* ((account (or server nngmail-last-account))
	   (query (format "message_id=%s" article))
	   (message (nngmail-fetch-resource "message" nil account
					    `((q . ,query)))))
      (setq article (plist-get message 'id))))
  (let* ((url (nngmail-url-for "message" article nil '((format . "raw"))))
	 (buffer (url-retrieve-synchronously url t)))
    (with-current-buffer (or to-buffer nntp-server-buffer)
      (erase-buffer)
      (url-insert-buffer-contents buffer url nil)
      (kill-buffer buffer)
      (goto-char (point-min))
      ;;; FIXME: is this necessary?
      ;;;(nnheader-insert-buffer-substring buffer)
      (nnheader-ms-strip-cr)))
  (cons group article))

(deffoo nngmail-request-group (group &optional server fast info)
  "Retrieve information about GROUP."
  ;;; 211 56 1000 1059 ifi.discussion
  (when group
    (setq group (nngmail-decode-gnus-group group)))
  (message (format "in nngmail-request-group %s" group))
  (let* ((account (or server nngmail-last-account))
	 (result (nngmail-change-group account group)))
    (with-current-buffer nntp-server-buffer
      (when result
	(and (not fast)
	     (nngmail-get-groups account))
	(erase-buffer)
	(insert (format "211 %d %d %d %S\n"
			(nngmail-get-group-count account group)
			(nngmail-get-group-min account group)
			(nngmail-get-group-max account group)
			group))))
    (nngmail-touch-server account)))
  
(deffoo nngmail-close-group (group &optional server)
  "Close the group.  A nop."
  (message (format "in nngmail-close-group for %s" group)))

(deffoo nngmail-request-list (&optional server)
  "Return a list of all groups available on SERVER."
  ;;; An example of a server with two groups
  ;;;
  ;;; ifi.test 0000002200 0000002000 y
  ;;; ifi.discussion 3324 3300 n
  (let ((account (or server nngmail-last-account)))
    (if account
	(with-current-buffer nntp-server-buffer
	  (nngmail-get-groups account)
	  (erase-buffer)
	  (maphash (lambda (key value)
		     (insert (format "%S %d %d n\n"
				     key
				     (cdr (assq 'max value))
				     (cdr (assq 'max value)))))
		   (nngmail-get-account-groups account))
	  (nngmail-touch-server account))
      nil)))

(defun nngmail-article-ranges (ranges)
  (let (result)
    (cond
     ((numberp ranges)
      (number-to-string ranges))
     ((numberp (cdr ranges))
      (format "%d:%d" (car ranges) (cdr ranges)))
     (t
      (dolist (elem ranges)
	(push
	 (if (consp elem)
	     (format "%d:%d" (car elem) (cdr elem))
	   (number-to-string elem))
	 result))
      (mapconcat #'identity (nreverse result) ",")))))

(deffoo nngmail-retrieve-headers (articles &optional group server fetch-old)
  "Retrieve headers for the specified group (label)."
  (when group
    (setq group (nngmail-decode-gnus-group group)))
  (message (format "in nngmail-retrieve-headers for %s" group))
  (let* ((account (or server nngmail-last-account))
	 (ids (nngmail-article-ranges (gnus-compress-sequence articles)))
	 (url (nngmail-url-for "message" nil
			      (nngmail-get-account-id account)
			      `((format . "nov")
				(id . ,ids))))
	 (buffer (url-retrieve-synchronously url t)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (url-insert-buffer-contents buffer url nil)
      (kill-buffer buffer)
      (goto-char (point-min))
      ;;; FIXME: is this necessary?
      ;;;(nnheader-insert-buffer-substring buffer)
      (nnheader-remove-cr-followed-by-lf)))
  'nov)

(deffoo nngmail-request-post (&optional server)
  (let ((account (or server nngmail-last-account)))
    (if account
	(nngmail-set-account-message "Read-only server")))
  nil)

(deffoo nngmail-request-type (group &optional article)
  'mail)

;;;
;;; Optional functions in gnus back end API
;;;
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
