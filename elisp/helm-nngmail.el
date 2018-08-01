;;; helm-nngmail --- helm browse unread email using nngmail back end
;;; -*- lexical-binding: t *-*

;; Copyright © 2018 Itineris, Inc.
;;
;; Author: Ricardo E. Gonzalez <ricardog@itinerisinc.com>
;; URL: http://www.github.com/ricardog00/nngmail.git
;; Version: 0.1
;; Keywords: Helm, Gnus, Gmail
;; Package-Requires: ((emacs "25.3") (helm) (gnus "5.13") (nngmail "0.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Provides a helm source for quickly processing email based on nngmail
;;; Gnus back end.  Because the message metadata is kept in a local
;;; database, search are very quick which makes the helm interaction
;;; model possible.

;;; Once the user selects one (or more) messages to process we fall back
;;; on Gnus (via nnir) to display them.  Some actions, such as marking
;;; as read or deleting (expiring in Gnus parlance) are implemented
;;; directly.  There is need to rely on Gnus since the operations are
;;; straightforward in elisp.

;;; To get a helm buffer with all your UNREAD messages (from all
;;; accounts) simply run M-x helm-nngmail.  With a prefix-arg it will
;;; prompt for a label to display (also across all accounts).

;;; The way I use this is to quickly process emails as they arrive.  A
;;; couple of times per day I run M-x helm-nngmail and dispose of
;;; messages (reply, delete, mark for follow-up, etc.)  More
;;; infrequently I open Gnus to deal with messages that require more
;;; interaction.

;; 
;;; Code:
(require 'helm)
(require 'nngmail)

(defun trunc (len s &optional ellipsis)
  "If S is longer than LEN, cut it down and add ELLIPSIS to the end.
The resulting string, including ellipsis, will be LEN characters
long.
When not specified, ELLIPSIS defaults to ‘...’."
  (unless ellipsis
    (setq ellipsis "..."))
  (if (> (length s) len)
      (format "%s%s" (substring s 0 (- len (length ellipsis))) ellipsis)
    s))

(defun helm-nngmail-format-message (message)
  "Generate the display sumary for a MESSAGE."
  (let ((text (format (concat
		       (propertize "[%-20s] " 'face 'message-header-name)
		       (propertize "%-55s\n" 'face 'message-subject-face)
		       (propertize "  %-78s" 'face 'italic))
		      (trunc 20 (if (string= "" (cdr (assq 'name message)))
				    (cdr (assq 'email message))
				  (cdr (assq 'name message))))
		      (trunc 55 (cdr (assq 'subject message)))
		      (trunc 78 (cdr (assq 'snippet message)))
		      )
	      ))
    text))

(defun helm-source-nngmail-get-candidates (account group messages)
  "Generate the actual list of candidates from ACCOUNT, GROUP, MESSAGES."
  (mapcar (lambda (message)
	    (cons (helm-nngmail-format-message message) message))
	  messages))
  
(defun helm-source-nngmail-build (&optional group unread)
  "Create a helm source of all unread messages in all accounts/servers.

If optional GROUP parameter is given, then create a source with
all messages from that group."
  (let ((data (mapcar (lambda (srv)
			(nngmail-get-messages (car srv)
					      (or group "INBOX")
					      unread))
		      (nngmail-get-accounts))))
    (message "fetched accounts")
    (mapcar (lambda (srv)
	      (let* ((account (car srv))
		     (messages (cdr srv))
		     (candidates (helm-source-nngmail-get-candidates account
								     (or group "UNREAD") messages)))
		`((name . ,account)
		  (candidates . ,candidates)
		  (multilne . 2)
		  (action
		   ("Read" . helm-nngmail-action-read)
		   ("Mark read" . helm-nngmail-action-mark-read)
		   ("Delete" . helm-nngmail-action-expire)))))
	    data)
    ))

(defun helm-nngmail-candidate-server (candidate)
  "Genrate the full server name for a CANDIDATE."
  (format "nngmail:%s" (cdr (assq 'server candidate))))

(defun helm-nngmail-candidate-group (candidate)
  "Genrate the full group name for a CANDIDATE."
  (gnus-group-full-name (cdr (assq 'group candidate))
			(helm-nngmail-candidate-server candidate)))


(defun helm-nngmail-action-read (candidates)
  "Handle read actions for helm buffers.

The user just selected one (or more) candidate articles/messages
to read.  This function uses the `nnir` interface to create an
ephemeral group that holds only the articles the user is
interested in.

Read the list of selected/marked candidates from
`helm-marked-candidates` and ignore CANDIDATES."
  (setq candidates (helm-marked-candidates :all-sources t))
  (let ((articles (mapcar (lambda (candidate)
			    (let ((server
				   (helm-nngmail-candidate-server candidate))
				  (group
				   (helm-nngmail-candidate-group candidate))
				  (id (cdr (assq 'article-id candidate))))
			      (cons group id)))
			  candidates))
	  ;; Pick first server and group of selected candidates because
	  ;; (I think) we need to pass something to nnir-run-query
	  ;; although the nngmail back end (nnir-run-gmail) will ignore
	  ;; the server/group lists.
	(srv (helm-nngmail-candidate-server (elt candidates 0)))
	(grp (helm-nngmail-candidate-group (elt candidates 0))))
      (gnus-group-make-nnir-group
       nil
       `((nnir-query-spec . ((query . nil) (articles . ,articles)))
	 (nnir-group-spec . ((,srv (,grp)))))))
  )

(defun helm-nngmail-group-by-group (candidates)
  "Group a list of CANDIDATES by server and group."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (candidate candidates)
      (let ((group (helm-nngmail-candidate-group candidate))
	    (id (cdr (assq 'article-id candidate))))
	(push id (gethash group ht ()))))
    ht))

(defun helm-nngmail-action-mark-read (candidates)
  "Handle mark as read actions for helm buffers."
  (message "helm-nngmail-action-mark-read")
  (setq candidates (helm-marked-candidates :all-sources t))
  (let ((ht (helm-nngmail-group-by-group candidates)))
    (maphash (lambda (key articles)
	       (let ((server (car (last (split-string
					 (gnus-group-server key)
					 ":"))))
		     (group (gnus-group-short-name key))
		     (range (gnus-compress-sequence articles)))
		 (nngmail-request-set-mark group
					   `((,range add (read)))
					   server))
	       )
	     ht)
    ))

(defun helm-nngmail-action-expire (candidates)
"Handle expire actions for helm buffers."
  (message "helm-nngmail-action-expire")
  (setq candidates (helm-marked-candidates :all-sources t))
  (let ((ht (helm-nngmail-group-by-group candidates)))
    (maphash (lambda (key articles)
	       (let ((server (car (last (split-string
					 (gnus-group-server key)
					 ":"))))
		     (group (gnus-group-short-name key)))
		 (nngmail-request-expire-articles articles group server))
	       )
	     ht)
    ))

(defvar helm-nngmail-actions
  '(("Read" . (lambda (candidate)
		(helm-nngmail-action-read candidate)))
    ("Mark as read" . (lambda (candidate)
			(helm-nngmail-action-mark-read candidate)))
    ("Delete" . (lambda (candidate)
		  (helm-nngmail-action-expire candidate)))))

(defvar helm-nngmail-history-input nil
  "Helm nngmail filter history.")
(defvar helm-nngmail-label-history nil
  "Helm nngmail label history.")

(defun helm-nngmail (&optional label)
  "Build a helm source from messagesin all nngmail accounts.
With optional LABEL fetch messages with that label or UNREAD by
default."
  (interactive)
  (let ((label (if current-prefix-arg
		   (completing-read "Label : "
				    (nngmail-get-all-labels)
				    nil t nil
				    helm-nngmail-label-history)
		 "INBOX")))
    (helm :sources (helm-source-nngmail-build label (not current-prefix-arg))
	  :buffer "*helm-nngmail*"
	  :prompt "Messages matching: "
	  :history 'helm-nngmail-history-input
	  :helm-candidate-number-limit 200)))

(provide 'helm-nngmail)
;;; helm-nngmail.el ends here
