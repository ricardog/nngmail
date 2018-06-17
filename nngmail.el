;;; package --- Summary

;;; Commentary:


;;; Code:
(eval-and-compile
  (require 'nnheader)
  ;; In Emacs 24, `open-protocol-stream' is an autoloaded alias for
  ;; `make-network-stream'.
  (unless (fboundp 'open-protocol-stream)
    (require 'proto-stream)))

(eval-when-compile
  (require 'cl))

(require 'ht)
(require 'url-http)
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

(defun nngmail-get-group-id (nickname group)
  (cdr (assq 'id (ht-get (nngmail-get-groups nickname) group))))

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
		     (message)
		     (group)
		     ))))

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

(defun nngmail-url-for (resource &optional account-id id args)
  "Generate a URL to probe the resource."
  (let ((base-url (cond
		   ((stringp account-id)
		    (format "%s/accounts/%s" nngmail-base-url account-id))
		   (account-id
		    (format "%s/accounts/%d" nngmail-base-url account-id))
		   (t nngmail-base-url)))
	(res-name (symbol-name resource))
	(url-args
	 (mapconcat (function (lambda (value)
				(format "%s=%s" (car value)
					(if (stringp (cdr value))
					    (url-hexify-string (cdr value))
					  (cdr value)))))
			     args "&")))
    (cond
     ((stringp id)
      (format "%s/%ss/%s?%s" base-url res-name id url-args))
     (id
      (format "%s/%ss/%d?%s" base-url res-name id url-args))
     (t
      (format "%s/%ss/?%s" base-url res-name url-args)))))

(defun nngmail-handle-response ()
  "Handle the response from a `url-retrieve-synchronously' call.
 Parse the HTTP response and throw if an error occurred.  The url
 package seems to require extra processing for this.  This should
 be called in a `save-excursion', in the download buffer.  It
 will move point to somewhere in the headers."
   ;; We assume HTTP here.
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

(defun nngmail-fetch-resource-url (url)
  (let* ((buffer (condition-case ex
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

(defun nngmail-fetch-resource (resource &optional account-id id args)
  "Retrieve a resource from the nngmail server."
  (let* ((url (nngmail-url-for resource account-id id args)))
    (nngmail-fetch-resource-url url)))

(defun nngmail-get-accounts ()
  "Get a list of accounts, with their respective ID's, nicknames,
and email addresses, from the server.  The list of accounts is
store in `nngmail-servers` for fast access."
  (let* ((resource (nngmail-fetch-resource 'account))
	 (accounts (plist-get resource 'accounts))
	 servers)
    (seq-map
     (lambda (elem)
       (push (nngmail-get-account-params elem) servers))
     accounts)
    servers))

(defun nngmail-get-groups (server)
  "Get a list of groups/labels, with their respective ID's, nicknames,
and email addresses, from the server.  The list of accounts is
store in `nngmail-servers` for fast access."
  (message (format "in nngmail-get-groups for %s" server))
  (let* ((groups (nngmail-get-account-groups server))
	 (resource (nngmail-fetch-resource 'label server nil
					   '((format . "info"))))
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
  (let ((servers (nngmail-get-accounts)))
    (when (not (assoc server servers))
      (nnheader-report
       'nngmail (format "You are not syncing %s" server)))
    (push (cons server (cdr (assoc server servers))) nngmail-servers))
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
	     nngmail-last-account nil))
  t)

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
  (if (nngmail-get-account server)
      t
    nil))

(deffoo nngmail-status-message (&optional server)
  (or (nngmail-get-account-message (or server nngmail-last-account))
      nngmail-status-string))

(defun nngmail-handle-article-request-response (buffer)
  "Check for errors when fetching articles."
  (let (rbuffer)
    (with-current-buffer buffer
      (let ((response (url-http-parse-response)))
	(cond
	 ((= response 409)
	  ;; Message not available, retry
	  (setq rbuffer nil))
	 ((or (< response 200) (>= response 300))
	  (let ((json-object-type 'plist)
		(json-key-type 'symbol)
		(json-array-type 'vector))
	    (goto-char (point-min))
	    (re-search-forward "^$")
	    (setq nngmail-status-string
		  (nngmail-get-error-string (json-read)))
	    (setq rbuffer nil)))
	 (t
	  ;;(error "Error: nngmail server responded with error"))))
	  (setq rbuffer buffer)))))
    (when (not rbuffer)
      (kill-buffer buffer))
    rbuffer))

(defun nngmail-fetch-article (url)
  "Issue an http request for article.  Check the response and
retry if necessary--which happens when the article needs to be
read from gmail."
  (let ((retries 2)
	buffer)
    (while (and (not buffer) (> retries 0))
      (setq buffer (nngmail-handle-article-request-response
		    (url-retrieve-synchronously url t)))
      (setq retries (- retries 1))
      (when (not buffer)
	(sleep-for 0 400)))
    buffer))

(defun nngmail-message-id-to-id (article account)
  "Query the server to map a Message-ID to the message's ID (the
primary key in the database)."
  (let* ((message (nngmail-fetch-resource 'message account article)))
    (plist-get message 'id)))
 
(deffoo nngmail-request-article (article &optional group server to-buffer)
  "Issue an HTTP request for the raw article body."
  (when group
    (setq group (nngmail-decode-gnus-group group)))
  (when (not server)
    (setq server (or server nngmail-last-account)))
  (let* ((dest-buffer (or to-buffer nntp-server-buffer))
	 (article-id (nngmail-message-id-to-id article server))
	 (url (nngmail-url-for 'message server article '((format . "raw"))))
	 (buffer (nngmail-fetch-article url)))
    (if buffer
      (with-current-buffer dest-buffer
	(erase-buffer)
	(url-insert-buffer-contents buffer url nil)
	(kill-buffer buffer)
	(goto-char (point-min))
      ;;; FIXME: is this necessary?
      ;;;(nnheader-insert-buffer-substring buffer)
	(nnheader-ms-strip-cr)
	(cons group article-id))
      nil)))
    

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
	(when info
	  (nngmail-update-info group account info))
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
	  (message (format "in nngmail-request list for %s" account))
	  (nngmail-get-groups account)
	  (erase-buffer)
	  (maphash (lambda (key value)
		     (insert (format "%S %d %d n\n"
				     key
				     (cdr (assq 'max value))
				     (cdr (assq 'min value)))))
		   (nngmail-get-account-groups account))
	  (nngmail-touch-server account))
      t)))

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
	 (limit (or (and (eq fetch-old t)
			 (min (nngmail-get-group-count account group) 5000))
		    fetch-old
		    (length articles)))
	 (ids (nngmail-article-ranges (gnus-compress-sequence articles)))
	 (url (nngmail-url-for 'message (nngmail-get-account-id account) nil
			       `((format . "nov")
				 (id . ,ids)
				 (label . ,group))))
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

(deffoo nngmail-request-thread (header &optional group server)
  (when group
    (setq group (nnimap-decode-gnus-group group)))
  (message (format "in nngmail-request-tread  %d" (elt header 0)))
  (let* ((message (nngmail-fetch-resource 'message nil (elt header 0)))
	 (thread-id (plist-get message 'thread_id))
	 (url (nngmail-url-for "thread" thread-id nil
			       `((format . "nov"))))
	 (buffer (url-retrieve-synchronously url t)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (url-insert-buffer-contents buffer url nil)
      (kill-buffer buffer)
      (goto-char (point-min))
      ;;; FIXME: is this necessary?
      ;;;(nnheader-insert-buffer-substring buffer)
      (nnheader-remove-cr-followed-by-lf)))
  t)

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

(deffoo nngmail-retrive-groups (groups &optional server)
  (message (format "in nngmail-retrieve-group"))
  (nngmail-request-list server)
  'active)

(defun v2l (arg)
  (if (vectorp arg)
      `(,(elt arg 0) .  ,(elt arg 1))
    arg))

(defun vector-to-list (vec)
  (cl-map 'list #'v2l vec))

(deffoo nngmail-update-info (group account info)
  (message (format "in nngmail-request-update-info for %s" group))
  (let* ((timestamp (gnus-group-timestamp
		     (format "nngmail+%s:%s" account group)))
	 (args (or (and timestamp
			(format "?timestamp_low=%d&timestamp_high=%d"
				(elt timestamp 1) (elt timestamp 0)))
		    "?"))
	 (url (concat
	       (substring (nngmail-url-for 'label account group) 0 -1)
	       (format "/flags%s" args)))
	 (flags (nngmail-fetch-resource-url url))
	 (marks (gnus-info-marks info)))
    (gnus-info-set-read info (vector-to-list (plist-get flags 'seen)))
    (loop for (k v) on flags by (function cddr)
          do
	  (progn
	    (let ((new-list (vector-to-list v)))
	      (if (assoc k marks)
		  (setcdr (assoc k marks) new-list)
		(push (cons k new-list) marks)))))
    (gnus-info-set-marks info marks)
    ))

(deffoo nngmail-finish-retrieve-group-infos (server infos sequences
						    &optional dont-insert)
  (message (format "in nngmail-request-update-infos for %s" group))
  ;; Iterate through all groups updating flags in group info.
  nil)

(deffoo nngmail-request-set-mark (group action &optional server)
  "Set/remove/add marks on articles."
  ;;; ACTION is a list of mark setting requests, having this format:
  ;;; (RANGE ACTION MARK)
  ;;; Gnus marks are:
  (message (format "in nngmail-request-set-mark for %s" group))
  nil)

(deffoo nngmail-request-update-mark (group article mark)
  (let ((name (cond
	       ((eq mark gnus-unread-mark)
		"unread")
	       ((eq mark gnus-ticked-mark)
		"ticked")
	       ((eq mark gnus-dormant-mark)
		"dorman")
	       ((eq mark gnus-del-mark)
		"del")
	       ((eq mark gnus-read-mark)
		"read")
	       ((eq mark gnus-expirable-mark)
		"expirable")
	       ((eq mark gnus-killed-mark)
		"killed")
	       ((eq mark gnus-spam-mark)
		"spam")
	       ((eq mark gnus-kill-file-mark)
		"kill-file")
	       ((eq mark gnus-low-score-mark)
		"low score")
	       ((eq mark gnus-catchup-mark)
		"catchup")
	       ((eq mark gnus-replied-mark)
		"replied")
	       ((eq mark gnus-forwarded-mark)
		"forwarded")
	       ((eq mark gnus-recent-mark)
		"recent")
	       ((eq mark gnus-cached-mark)
		"cached")
	       ((eq mark gnus-unseen-mark)
		"unseen")
	       ((eq mark gnus-no-mark)
		"no mark")
	       (t
		"unknown mark"))))
	(message (format "in nngmail-request-update-mark for %s:%d %s"
			 group article name)))
  mark)

(deffoo nngmail-request-scan (group &optional server info)
  "Check for new articles.  If possible only on GROUP (although
that makes no sense for this backend since new (unread) mail will
appear in INBOX."
  (message (format "in nngmail-request-scan %s" group))
  (when group
    (setq group (nngmail-decode-gnus-group group)))
  (let ((account (or server nngmail-last-account)))
    (nngmail-get-groups account)
    (when info
      (nnimap-update-info group account info)))
  nil)

(deffoo nngmail-request-newsgroups (date &optional server)
  "Returns a list of newsgroups created after DATE."
  (message (format "in nngmail-request-newsgroups %s" date))
  nil)

(deffoo nngmail-request-expire-articles (articles &optional group server force)
  "Expire articles."
  (message (format "in nngmail-request-expire-articles %s" group))
  nil)

(deffoo nngmail-request-move-article (article group server accept-form
					      &optional last)
  "Move article to a new group."
  (message (format "in nngmail-request-move-article %d" article))
  nil)

(deffoo nngmail-request-accept-article (group &optional server last)
  "Used for respooling?"
  (message (format "in nngmail-request-accept-article %d" article))
  nil)

(deffoo nngmail-request-delete-group (group force &optional server)
  ""
  (message (format "in nngmail-request-delete-group %s" group))
  nil)

(deffoo nngmail-request-rename-group (group new-name &optional server)
  ""
  (message (format "in nngmail-request-rename-group %s" group))
  nil)

(defun nnir-run-nngmail (query srv &optional groups)
  (let ((qstring (cdr (assq 'query query)))
	(server (cadr (gnus-server-to-method srv)))
	(defs (caddr (gnus-server-to-method srv)))
	(creteria (or (cdr (asq 'creteria query))
		      (cdr (assoc nnir-nngmail-default-search-key
				  nnir-nngmail-search-arguments))))

	(groups (mapconcat gnus-group-short-name
			   (or groups (nnir-get-active-srv) ","))))
    (let ((response (nngmail-fetch-resource 'query server nil
					    '((query . ,qstring)
					      (labels . ,groups))))
	  result)
      (mapcar (lambda (res)
		(vector (format "nngmail+%s:%s" server (elt res 0))
			(elt res 1) 100))
	      (plist-get 'result response)
	      ))))

(provide 'nngmail)
;;; nngmail.el ends here
