;;; package --- A Gmail REST API back end for Gnus

;; Copyright (C) 2018  Itineris, Inc.

;; Author: Ricardo E. Gonzalez  <ricardog@itinerisinc.com>
;; URL: https://bitbucket.org/ricardog00/nngmail/nngmail.git
;; Version: 0.1
;; Keywords: Gnus, Gmail, mail
;; Package-Requires: ((emacs "25.3") (gnus "5.13") (ht 2.3")

;;; Commentary:

;;; This file provides a Gnus back end for reading email via the Gmail
;;; REST API.  It relies on a local synchronization services that store
;;; all message metadata plus caches recent messages.  Recent refers to
;;; recently received or recently read.

;;; This code implements as well an nnir back end (`nnir-run-gmail')
;;; which will run searches on Gmail from within Gnus (similar to the
;;; nnimap interface.  This allows powerful searching capabilities with
;;; minimal effort.  The search query is passed as-is to Gmail, except
;;; that we limit the search to specified labels (for example if you
;;; want to search for messages in one group only).

;;; This file also implements a few functions used by the `helm-nngmail'
;;; source for quickly dealing with email.

;;; To use this back end you must first set up the synchronizing proxy
;;; (see the README for details).  Once that is setup and running, all
;;; something like the following to your `gnus-secondary-methods'

;;;		(nngmail "personal"
;;;			 (nngmail-email "personal@example.com")
;;;			 (nngmail-address "localhost")
;;;			 (nngmail-server-port 5000)
;;;			 )
;;;		(nngmail "work"
;;;			 (nngmail-email "work@example.com")
;;;			 (nngmail-address "localhost")
;;;			 (nngmail-server-port 5000)
;;;			 )

;;; The next time you run Gnus it should be able to open the "servers"
;;; and you can subscribe to whatever groups you want.  Like the Gmail
;;; IMAP interface, Gmail labels are mapped to groups.

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
(require 'nnir)
(require 'gnus-util)
(require 'gnus)
(require 'nnoo)
(require 'netrc)
(require 'utf7)
(require 'tls)
(require 'parse-time)
(require 'nnmail)

(nnoo-declare nngmail)
(gnus-declare-backend "nngmail" 'mail 'address 'server-marks)

(defvoo nngmail-host "localhost"
  "The address of the gmail proxy / sync server (nnsync).")

(defvoo nngmail-user nil
  "Username to use for authentication to the IMAP server.")

(defvoo nngmail-port 5000
  "The port the gmail proxy listens on.")

(defvoo nngmail-base-url
    (format "http://%s:%d/api/v1.0" nngmail-host nngmail-port)
  "The base URL for talking to the nngmail server.")

(defvoo nngmail-split-methods nil
  "How mail is split.
Uses the same syntax as `nnmail-split-methods'.  Not supported as
because nngmail is not a respooling source.")

(defvoo nngmail-split-fancy nil
  "Uses the same syntax as `nnmail-split-fancy'.  Not supported
  because nngmail is not a respooling source. ")

(defun nngmail-decode-gnus-group (group)
  "Function to decode a GROUP name.
Stolen from nnimap."
  (decode-coding-string group 'utf-8))

(defun nngmail-encode-gnus-group (group)
  "Function to encode a GROUP name.
Stolen from nnimap."
  (encode-coding-string group 'utf-8))

(defvar nngmail-last-account-id nil
  "ID of last nngmail account used by any Gnus-initiated operation.")
(defvar nngmail-last-account nil
  "Nickname of last nngmail account used by any Gnus-initiated operation.")

(defvar nngmail-status-string ""
  "Last error string repoted by this source.")

(defvar nngmail-servers ()
  "An alist of accounts the server knows about.
What I call an account in the server is what gnus calls a server.
This list has all the accounts the server we connect to synchs.")

;; These functions should get turned into a macro
(defun nngmail-get-account (nickname)
  "Get info alist for account NICKNAME."
  (cdr (assoc nickname nngmail-servers)))

(defun nngmail-get-account-x (nickname what)
  "Get parameter WHAT for account NICKNAME."
  (cdr (assq what (nngmail-get-account nickname))))

(defun nngmail-get-account-id (nickname)
  "Get ID of account NICKNAME."
  (nngmail-get-account-x nickname 'id))

(defun nngmail-get-account-email (nickname)
  "Get email of account NICKNAME."
  (nngmail-get-account-x nickname 'email))

(defun nngmail-get-account-groups (nickname)
  "Get groups hash table of account NICKNAME."
  (nngmail-get-account-x nickname 'groups))

(defun nngmail-get-account-writable (nickname)
  "Get writable flag of account NICKNAME."
  (nngmail-get-account-x nickname 'writable))

(defun nngmail-get-account-can-send (nickname)
  "Get can-send flag of account NICKNAME."
  (nngmail-get-account-x nickname 'can-send))

(defun nngmail-get-account-message (nickname)
  "Get error string of account NICKNAME."
  (nngmail-get-account-x nickname 'message))

(defun nngmail-get-account-group (nickname)
  "Get last group we switch to for account NICKNAME."
  (nngmail-get-account-x nickname 'group))

(defun nngmail-set-account-groups (nickname groups)
  "Set GROUPS hash table for account NICKNAME."
  (setcdr (assoc 'groups (assoc nickname nngmail-servers)) groups))

(defun nngmail-set-account-group (nickname group)
  "Switch to GROUP in account NICKNAME."
  (setcdr (assoc 'group (assoc nickname nngmail-servers)) group))

(defun nngmail-get-group-x (nickname group what)
  "Get parameter WHAT from GROUP for account NICKNAME."
  (cdr (assq what (ht-get (nngmail-get-groups nickname) group))))

(defun nngmail-get-group-id (nickname group)
  "Get ID of GROUP for account NICKNAME."
  (nngmail-get-group-x nickname group 'id))

(defun nngmail-get-group-min (nickname group)
  "Get min article number in GROUP for account NICKNAME."
  (nngmail-get-group-x nickname group 'min))

(defun nngmail-get-group-max (nickname group)
  "Get max article number in GROUP for account NICKNAME."
  (nngmail-get-group-x nickname group 'max))

(defun nngmail-get-group-count (nickname group)
  "Get article count in GROUP for account NICKNAME."
  (nngmail-get-group-x nickname group 'count))

(defun nngmail-get-error-string (response)
  "Get the error string of a JSON RESPONSE from the proxy."
  (plist-get response 'error))
  
(defun nngmail-get-account-params (elem)
  "Get account parameters from plist ELEM.
The JSON parser returns a plist.  This function extracts the
account parameters we want to keep around and stores them in an
alist.  It's all a bit bogus at the moment and I should perhas
change the JSON parser to return an alist and be done."
  (let ((nickname (plist-get elem 'nickname))
	(email (plist-get elem 'email))
	(id (plist-get elem 'id))
	(writable (plist-get elem 'writable))
	(can-send (plist-get elem 'can_send))
	(groups (ht)))
    (cons nickname `((id       . ,id)
		     (email    . ,email)
		     (groups   . ,groups)
		     (writable . ,writable)
		     (can-send . ,can-send)
		     (message)
		     (group)
		     ))))

(defun nngmail-get-group-params (elem)
  "Get group parameters from plist ELEM.
The JSON parser returns a plist.  This function extracts the
group parameters we want to keep around and stores them in an
alist.  It's all a bit bogus at the moment and I should perhas
change the JSON parser to return an alist and be done."
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

(defun nngmail-get-message-params (elem)
  "Get message parameters from plist ELEM.
The JSON parser returns a plist.  This function extracts the
message parameters we want to keep around and stores them in an
alist.  It's all a bit bogus at the moment and I should perhas
change the JSON parser to return an alist and be done."
  (let ((id (plist-get elem 'id))
	(google_id (plist-get elem 'google_id))
	(name (plist-get (plist-get elem 'sender) 'name))
	(email (plist-get (plist-get elem 'sender) 'email))
	(from (format "%s <%s>"
		      (plist-get (plist-get elem 'sender) 'name)
		      (plist-get (plist-get elem 'sender) 'email)))
	(subject (plist-get elem 'subject))
	(snippet (plist-get elem 'snippet))
	(labels (mapcar (lambda (label)
			  (cons (plist-get label 'gid)
				(plist-get label 'name)))
			(plist-get elem 'labels)))
	)
    (cons id `((id . ,id)
	       (google_id . ,google_id)
	       (from . ,from)
	       (name . ,name)
	       (email . ,email)
	       (subject . ,subject)
	       (snippet . ,snippet)
	       (labels . ,labels)))))

(defun nngmail-url-for (resource &optional account-id id args)
  "Generate a URL for a RESOURCE.
Optional ACCOUNT-ID and resource ID are used to construct the URL
by convention.  If given, ARGS should be an alist of URL
parameters to send to the server.

FiXME: Use discovery to get te URL for parameters so we don't
have to hard-code URL rules."
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
be called in a `save-excursion', in the download buffer.  It will
move point to somewhere in the headers."
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
  "Parse JSON response in BUFFER.
Currently converts objects to plists and arrays to vectors."
  (with-current-buffer buffer
    (nngmail-handle-response)
    (goto-char url-http-end-of-headers)
    (let* ((json-object-type 'plist)
	   (json-key-type 'symbol)
	   (json-array-type 'vector))
      (json-read))))

(defmacro safe-parse (fn &rest clean-up)
  "Call function FN to parse a response.
If the request failed (or the parser fails) call CLEAN-UP
function."
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
  "Fetch a URL resource by URL.
This assumed we already have a URL to tickle.  Most use cases
should prefer `nngmail-fetch-resource' which will generate the
URL for the resource."
  (let* ((url-mime-accept-string "application/json")
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

(defun nngmail-fetch-resource (resource &optional account-id id args)
  "Retrieve a RESOURCE from the nngmail server.

See `nngmail-url-for' for a description of optional
arguments (ACCOUNT-ID, ID, ARGS)."
  (let* ((url (nngmail-url-for resource account-id id args)))
    (nngmail-fetch-resource-url url)))

(defun nngmail-get-accounts ()
  "Get a list of accounts for the server.
Each account will have an ID, a nickname, and and email
addresses.  The list of accounts is stored in `nngmail-servers'
for fast access."
  (let* ((resource (nngmail-fetch-resource 'account))
	 (accounts (plist-get resource 'accounts))
	 servers)
    (seq-map
     (lambda (elem)
       (push (nngmail-get-account-params elem) servers))
     accounts)
    servers))

(defun nngmail-get-groups (server)
  "Get a list of groups/labels for account SERVER.

SERVER is the Gnus virtual server name that maps to the email
account we are retrieving information for.

Each group has an ID, an article range(min, max) and an article
count.  The function returns a hash table with information for
all groups.  Gnus-related functions store the hash table in
`nngmail-servers' for fast access."
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
    (ht-from-alist tmp)
    ))

(defun nngmail-touch-server (server)
  "Mark SERVER as the last used server.

Gnus back end API function signatures often have an optional
server.  So we need to remember which account we last used in
case an API function is called without an explicit server."
  (when (nngmail-get-account-id server)
    (setq nngmail-last-account-id (nngmail-get-account server)
	  nngmail-last-account server)))

(defun nngmail-change-group (server group)
  "Verify GROUP exists for SERVER and switch to it.
Switching is implemented by remembering the group name for future
reference."
  (message (format "in nngmail-change-groups for %s %s" server group))
  (and (ht-get (nngmail-get-account-groups server) group)
       (nngmail-set-account-group server group)))

;;;
;;; Required functions in gnus back end API
;;;
(deffoo nngmail-open-server (server &rest definitions)
  "Verify the nngmail server syncs the account SERVER.

FIXME: Understand what gets passed in DEFINITIONS and use the data."
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
  "Check for errors when fetching articles.

This functions takes as input a pointer to a BUFFER with the
response from `url-retrieve-synchronously'.

Fetching articles retruns plain text (text/plain) on success and
JSON on error hence we can't use the normal response parsing
routine.  If an error occurred (via the response code), parse the
JSON and extract the error message.  Without error, return a
pointer to the BUFFER.

FIXME: Remove re-try mechanism (no longer used)."
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
  "Request and article body from the server using URL.

Check the response and retry if necessary--which happens when the
article needs to be fetched from gmail."
  (let ((retries 2)
	buffer)
    (while (and (not buffer) (> retries 0))
      (setq buffer (nngmail-handle-article-request-response
		    (url-retrieve-synchronously url t)))
      (setq retries (- retries 1))
      (when (not buffer)
	(sleep-for 0 400)))
    buffer))

(defun nngmail-message-id-to-id (message-id account)
  "Fetch the ID (article number) for message with MESSAGE-ID in ACCOUNT..

In this context message-ID refers to the mail header value.  To
get the ID request the metadata for the article and extrat the
ID.

This function is used when fetching referring articles."
  (let* ((message (nngmail-fetch-resource 'message account message-id)))
    (plist-get message 'id)))
 
(deffoo nngmail-request-article (article &optional group server to-buffer)
  "Issue an HTTP request for ARTICLE body.

Optional GROUP and SERVER specify the group and server for the
article.  If not given, use the last switched-to group and
server.

ARTICLE can be either an article number or a Message-ID (the
header value).  If the latter, first map it to the article number
using `nngmail-message-id-to-id'.

If TO-BUFFER is non-`nil' place the data there instead of the
normal data buffer. "
  (when group
    (setq group (nngmail-decode-gnus-group group)))
  (when (not server)
    (setq server nngmail-last-account))
  (let* ((dest-buffer (or to-buffer nntp-server-buffer))
	 (article-id (if (stringp article)
			 (nngmail-message-id-to-id article server)
		       article))
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
  "Retrieve information about GROUP.

If FAST is non`nil', refresh the group information from the
server.  Otherwise use data previously fetched and stored in
`nngmail-servers'."
  ;;; 211 56 1000 1059 ifi.discussion
  (when group
    (setq group (nngmail-decode-gnus-group group)))
  (message (format "in nngmail-request-group %s" group))
  (let* ((account (or server nngmail-last-account))
	 (result (nngmail-change-group account group)))
    (with-current-buffer nntp-server-buffer
      (when result
	(and (not fast)
	     (nngmail-set-account-groups account
					 (nngmail-get-groups account)))
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
  "Close the group.  A nop for this back end."
  (message (format "in nngmail-close-group for %s" group)))

(deffoo nngmail-request-list (&optional server)
  "Return a list of all groups available on SERVER.

Each Gmail label is considered a group.  If the account was
created as read-only, i.e. it did not specify modify scope on
creation, then list all groups as read-only."
  ;;; An example of a server with two groups
  ;;;
  ;;; ifi.test 0000002200 0000002000 y
  ;;; ifi.discussion 3324 3300 n
  (let ((account (or server nngmail-last-account)))
    (if account
	(with-current-buffer nntp-server-buffer
	  (message (format "in nngmail-request list for %s" account))
	  (nngmail-set-account-groups account (nngmail-get-groups account))
	  (erase-buffer)
	  (maphash (lambda (key value)
		     (insert (format "%S %d %d %s\n"
				     key
				     (cdr (assq 'max value))
				     (cdr (assq 'min value))
				     (if (nngmail-get-account-writable account)
					 "y"
				       "n")
				     )))
		   (nngmail-get-account-groups account))
	  (nngmail-touch-server account))
      t)))

(defun nngmail-article-ranges (ranges)
  "Convert article RANGES to a string representation.

The result is a comma-separated list of ranges.  Each range is
either a single number, or a pair (low, high) separated by ':'."
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
  "Retrieve headers for the specified group (label).

The server is kind enough to return NOV format so twe don't need
to grovel over the response."
  (when group
    (setq group (nngmail-decode-gnus-group group)))
  (message (format "in nngmail-retrieve-headers for %s" group))
  (let* ((account (or server nngmail-last-account))
	 (limit (or (and (eq fetch-old t)
			 (min (nngmail-get-group-count account group) 5000))
		    fetch-old
		    (length articles)))
	 (ids (nngmail-article-ranges (gnus-compress-sequence articles)))
	 (url (nngmail-url-for 'message account nil
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
  "Request a list of messages for thread HEADER.

This function is not described in the Info node `(Gnus) Back End
Interface' but is implemented in `nnimap' and gets called when
the user invokes \\[gnus-summary-refer-thread].

The HEADER argument is a Gnuism whose first element is the
article ID.  First fetch the article metadata and extract the
thread ID.  Then request a list of messages (in NOV format) for
that thread.

Most of the work is done by the server.  All we do here is do two
requests from the server."
  (when group
    (setq group (nnimap-decode-gnus-group group)))
  (setq server (or server nngmail-last-account))
  (message (format "in nngmail-request-tread  %d" (elt header 0)))
  (let* ((message (nngmail-fetch-resource 'message server (elt header 0)))
	 (thread-id (plist-get message 'thread_id))
	 (url (nngmail-url-for 'thread thread-id nil
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
  "Unused."
  (let ((account (or server nngmail-last-account)))
    (if account
	(nngmail-set-account-message "Read-only server")))
  nil)

;; This function is not needed because the back-end is defined as
;; mail-only.  But will leave it here for now.
(deffoo nngmail-request-type (group &optional article)
  "Return the type of a GROUP for follow-ups.

This refers to how to do follow-ups.  Because we declare 'nngmail
to be a mail only back end, this function never gets called."
  'mail)

;;;
;;; Optional functions in gnus back end API
;;;
(defvar nngmail-mark-alist
  '((read "-UNREAD")
    (unread "UNREAD")
    (tick "IMPORTANT")
    (reply "answered")
    (expire "TRASH")
    (dormant "dormant")
    (score "score")
    (save "save")
    (download "download")
    (forward "forward"))
  "Map from Gnus marks to Gmail labels.

Gnus uses a read mark while Gmail uses an UNREAD label.  This
causes a bit of complexity when dealing with updating marks.")

(deffoo nngmail-retrive-groups (groups &optional server)
  "Retrieve information for GROUPS.

This calls `nngmailrequest-list' for SERVER and returns 'active
to Gnus can make sense of the data."
  (message (format "in nngmail-retrieve-group"))
  (nngmail-request-list server)
  'active)

(defun v2l (arg)
  "Convert vector ARG to a list."
  (if (vectorp arg)
      `(,(elt arg 0) .  ,(elt arg 1))
    arg))

(defun vector-to-list (vec)
  "Convert vector VEC to a list."
  (cl-map 'list #'v2l vec))

(deffoo nngmail-update-info (group account info)
  "Update the GROUP info structure.

Given an info structure (INFO) for GROUP in ACCOUNT, update the
structure with marks from the server, c.f. Info node `(Gnus)
Group Info'.  Currently this propagates read and unexist marks,
but I may implement support for other marks in the future."
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
    (gnus-info-set-read info (vector-to-list (plist-get flags 'read)))
    (loop for (k v) on flags by (function cddr)
          do
	  (progn
	    (let ((new-list (vector-to-list v)))
	      (if (assoc k marks)
		  (setcdr (assoc k marks) new-list)
		(push (cons k new-list) marks)))))
    (gnus-info-set-marks info marks t)
    ))

(deffoo nngmail-finish-retrieve-group-infos (server infos sequences
						    &optional dont-insert)
  "FIXME: what is this function for?"
  (message (format "in nngmail-request-update-infos for %s" group))
  ;; Iterate through all groups updating flags in group info.
  nil)

(defun nngmail-marks-to-labels (marks)
  "Convert Gnus MARKS to labels using `nngmail-mark-alist'."
  (let (flags flag)
    (dolist (mark marks)
      (when (setq flag (cadr (assq mark nngmail-mark-alist)))
	(push flag flags)))
    flags))

(deffoo nngmail-request-set-mark (group actions &optional server)
  "Set/remove/add marks from ACTIONS for GROUP.

This function is called when exiting a group and propagates marks
to the server.  Should only be called when the group is declared
a non read-only.  If function gets called when the group is
read-only the requests will fail.  Unfortunately there is no way
to signal to Gnus that this happened.  Well, there is a way but
Gnus drop the data on the floor, c.f. Info node `(Gnus) Optional
Back End Functions'.

Because Gnus uses a read mark but Gmail uses an UNREAD label, we
need to invert the action, e.g. add becomes del.  The way I
handle this is to call the function recursively with the inverted
action."
  ;;; ACTION is a list of mark setting requests, having this format:
  ;;; (RANGE ACTION MARK), e.g.
  ;;; ((((2157 . 2160)) add (read)))
  ;;; Gnus marks are:
  (message (format "in nngmail-request-set-mark for %s" group))
  (setq server (or server nngmail-last-account))
  (let (failures)
    (dolist (action actions)
      (destructuring-bind (range action marks) action
	(let ((url-request-method "PUT")
	      (labels (nngmail-marks-to-labels marks)))
	  ;; Invert add/del for "read" marks since Gmail u
	  (when (and (eq action 'add)
		     (member "-UNREAD" labels))
	    (push (nngmail-request-set-mark
		   group `((,range del ('unread))) server)
		  failures)
	    (setq labels (delete "-UNREAD" labels)))
	  (when (and (eq action 'del)
		     (member "-UNREAD" labels))
	    (push (nngmail-request-set-mark
		   group `((,range add (unread))) server)
		  failures)
	    (setq labels (delete "-UNREAD" labels)))
	  (when labels
	    (let* ((articles (nngmail-article-ranges range))
		   (data `((id . ,articles)
			   (add_labels . ,(if (eq action 'add) labels ()))
			   (rm_labels . ,(if (eq action 'del) labels ()))))
		   (entity (json-encode data))
		   (url-request-extra-headers
		    '(("Content-Type" . "application/json")))
		   (url-request-data (encode-coding-string entity 'utf-8)))
	      (let ((response (nngmail-fetch-resource 'message server)))
		(when (eq (car response) 'exception)
		  ;; FIXME: Determine which updates failes and return
		  ;; the appropriate list to Gnus.  For now assume all
		  ;; articles failed update.  Fortunately Gnus does not
		  ;; use the return value of this function.
		  (push range failures)))))
	  )))
    (flatten failures)))

(deffoo nngmail-request-update-mark (group article mark)
  "Filter MARK for GROUP.

This function is called whenever a mark is set, e.g. as soon as
an article is displayed it is mark as read.

FIXME: unsed."
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
  "Check for new articles.

If possible only on GROUP (although that makes no sense for this
backend since new (unread) mail will appear in INBOX."
  (message (format "in nngmail-request-scan %s" group))
  (when group
    (setq group (nngmail-decode-gnus-group group)))
  (let ((account (or server nngmail-last-account)))
    (nngmail-set-account-groups account (nngmail-get-groups account))
    (when info
      (nnimap-update-info group account info)))
  nil)

(deffoo nngmail-request-newsgroups (date &optional server)
  "Returns a list of newsgroups created after DATE."
  (message (format "in nngmail-request-newsgroups %s" date))
  nil)

(deffoo nngmail-request-expire-articles (articles &optional group server force)
  "Expire ARTICLES in optional GROUP and SERVER.

Make a DELETE request for each article and let the server handle
things."
  (message (format "in nngmail-request-expire-articles %s" group))
  (when (not server)
    (setq server nngmail-last-account))
  (let ((url-request-method "DELETE"))
    (delq nil
	  (mapcar (lambda (article)
		    (let ((result
			   (nngmail-fetch-resource 'message server article)))
		      (if (eq (car result) 'exception)
			  article
			nil)))
		  articles)))
  )

(deffoo nngmail-request-move-article (article group server accept-form
					      &optional last)
  "Move article to a new group.

FIXME: not implemented."
  (message (format "in nngmail-request-move-article %d" article))
  nil)

(deffoo nngmail-request-accept-article (group &optional server last)
  "Used for respooling?

FIXME: not implemented."
  (message (format "in nngmail-request-accept-article %d" article))
  nil)

(deffoo nngmail-request-delete-group (group force &optional server)
  "Delete the GROUP.

Will remove the label from the Gmail account.

FIXME: not implemented."
  (message (format "in nngmail-request-delete-group %s" group))
  nil)

(deffoo nngmail-request-rename-group (group new-name &optional server)
  "Renae GROUP in SERVER to NEW-NAME.

Rename the label in the Gmail account.

FIXME: not implemented."
  (message (format "in nngmail-request-rename-group %s" group))
  nil)
 
(defun nnir-run-gmail (query srv &optional groups)
  "Implements the back end for nnir search.

In the common case we are passed a query string in QUERY and pass
that along to the local proxy.  The local nnsync server will
query Gmail and return a list, where each element is a
two-element tuple (list in JSON) conssting of (id, group).  SRV
is the name of the server (acount or nickname) and GROUPS
specifies which groups to search.

This function also acts as the back end for the helm read action.
In this case the QUERY alist includes an entry (articles) with a
list of cons cells consisting of (group . id).  The group should
be a full group name."
  (let ((qstring (cdr (assq 'query query)))
	(articles (cdr (assq 'articles query)))
	(server (cadr (gnus-server-to-method srv)))
	(defs (caddr (gnus-server-to-method srv)))
	(groups (mapconcat (lambda (group) (gnus-group-short-name group))
			   (or groups (nnir-get-active srv)) ",")))
    
    (if articles
	(vconcat
	 (mapcar (lambda (art)
		   (vector (car art) (cdr art) 100))
		 articles))
      (let ((response (nngmail-fetch-resource 'query server nil
					      `((q . ,qstring)
						(labels . ,groups))))
	    result)
	(vconcat
	 (mapcar (lambda (res)
		   (let ((group (gnus-group-full-name (elt res 1) srv))
			 (id (elt res 0)))
		     (vector group id 100)))
		 (plist-get response 'result)
		 ))))))

;;;
;;; Add nngmail back end to nnir.
;;;
(push (cons 'nngmail 'gmail) nnir-method-default-engines)
(push (list 'gmail 'nnir-run-gmail nil) nnir-engines)

;;;
;;; Functions used by helm interface
;;;
(defun nngmail-get-messages (server group)
  "Return a list of messages for GROUP in SERVER.

This returns an alist for each message.  Unlike the
Gnus-equivalent which return NOV data in a buffer.  The data is used to construct the list of candidates for te `helm-nngmail' source."
  (let ((url (concat
	      (substring (nngmail-url-for 'label server group) 0 -1)
	      (format "/messages/"))))
    (cons server
	  (mapcar (lambda (result)
		    (let ((msg (cdr (nngmail-get-message-params result))))
		      (push `(server . ,server) msg)
		      (push `(group . ,group) msg)))
		  (plist-get (nngmail-fetch-resource-url url) 'messages)))
    ))

(defun flatten (list)
  "Flatten LIST."
  (mapcan (lambda (x) (if (listp x) x nil)) list))

(defun nngmail-get-all-labels ()
  "Return a list of all labels in all accounts.

The return list is used to construct the completion list of
`helm-nngmail' so only valid labels can be searched for."
  (let ((servers (nngmail-get-accounts))
	)
    (delq nil
	  (sort
	   (delete-dups
	    (flatten
	     (mapcar (lambda (server)
		       (ht-keys (nngmail-get-groups (car server))))
		     servers)))
	   'string<))
    ))
     
(provide 'nngmail)
;;; nngmail.el ends here
