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

;(defconst nngmail-src-dir (file-name-directory load-file-name))

(defvar nngmail-servers ()
  "An alist of accounts the server knows about.
What I call an account in the server is what gnus calls a server.  This list has all the accounts the server we connect to synchs.")

(defvar nngmail-groups ()
  "An alist of per-account groups the server knows about.
A server's entry holds a per-group alist with the min ID, max ID,
and artcile count for the group.")

(defun nngmail-get-account (nickname)
  (cdr (assoc-string nickname nngmail-servers)))

(defun nngmail-get-account-id (nickname)
  (elt (nngmail-get-account nickname) 0))

(defun nngmail-get-account-email (nickname)
  (elt (nngmail-get-account nickname) 1))

(defun nngmail-get-account-groups (nickname)
  (elt (nngmail-get-account nickname) 2))

(defun nngmail-set-account-groups (nickname groups)
  (setcdr (assoc nickname nngmail-servers)
	  (list (nngmail-get-account-id nickname)
		(nngmail-get-account-email nickname)
		groups)))

(defun nngmail-get-error-string (response)
  (plist-get response 'error))
  
(defun nngmail-get-account-params (elem)
  (let ((nickname (plist-get elem 'nickname))
	(email (plist-get elem 'email))
	(id (plist-get elem 'id)))
    (cons nickname (list id email (ht)))))

(defun nngmail-get-group-params (elem)
  (let ((google_id (plist-get elem 'gid))
	(id (plist-get elem 'id))
	(name (plist-get elem 'name))
	(min (plist-get elem 'min))
	(max (plist-get elem 'max))
	(count (plist-get elem 'count))
	)
    (cons name (list id google_id min max count))))

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
       (error "Error during download request:%s"
              (buffer-substring-no-properties (point) (progn
                                                        (end-of-line)
                                                        (point)))))))

(defun nngmail-parse-json (buffer)
  (with-current-buffer buffer
    (nngmail-handle-response)
    (goto-char url-http-end-of-headers)
    (let* ((json-object-type 'plist)
	   (json-key-type 'symbol)
	   (json-array-type 'vector))
      (json-read))))

(defun nngmail-fetch-resource (resource &optional id account-id args)
  "Retrieve a resource from the nngmail server."
  (let* ((url (nngmail-url-for resource id account-id args))
	 (buffer (url-retrieve-synchronously url t))
	 (response (nngmail-parse-json buffer)))
;    (kill-buffer buffer)
    response))

(defun nngmail-get-accounts ()
  "Get a list of accounts, with their respective ID's, nicknames,
and email addresses, from the server.  The list of accounts is
store in `nngmail-servers` for fast access."
  (let* ((resource (nngmail-fetch-resource "account"))
	 (accounts (if resource
		       (plist-get resource 'accounts)
		     ())))
    (seq-map
     (lambda (elem)
       (push (nngmail-get-account-params elem) nngmail-servers))
     accounts)
    ))

(defun nngmail-get-groups (server)
  "Get a list of groups/labels, with their respective ID's, nicknames,
and email addresses, from the server.  The list of accounts is
store in `nngmail-servers` for fast access."
  (let* ((groups (nngmail-get-account-groups server))
	 (account-id (nngmail-get-account-id server))
	 (resource (nngmail-fetch-resource "label" nil account-id
					   '((info . 1))))
	 (data (if resource
		     (plist-get resource 'labels)
		 ()))
	 (tmp ()))
    (seq-map
     (lambda (elem)
       (push (nngmail-get-group-params elem) tmp))
     data)
    (nngmail-set-account-groups server (ht-from-alist tmp))
    ))

(defun nngmail-touch-server (server)
  (and (nngmail-get-account-id server)
       (setq nngmail-last-account-id (nngmail-get-account server))))

;;;
;;; Required functions in gnus back end API
;;;
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
  (nngmail-touch-server server))

(deffoo nngmail-retrieve-headers (articles &optional group server fetch-old)
  "Retrieve headers for the specified group (label)."
  (when group
    (setq group (nnimap-decode-gnus-group group)))
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
  
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
