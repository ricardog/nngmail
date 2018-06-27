;;; helm-nngmail --- helm browse unread email using nngmail back end
;;; -*- lexical-binding: t *-*

;; Copyright © 2018 Ricardo E. Gonzalez
;;
;; Author: Ricardo E. Gonzalez <ricardog@itinerisinc.com>
;; URL: http://www.github.com/ricardog00/nngmail.git
;; Version: 0.1
;; Keywords: Helm, Gnus, Gmail
;; Package-Requires:

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
  (let ((text (format "[%-20s] %-55s\n  %-78s"
		      (trunc 20 (or (cdr (assq 'name message))
				    (cdr (assq 'email message))))
		      (trunc 55 (cdr (assq 'subject message)))
		      (trunc 78 (cdr (assq 'snippet message)))
		      )
	      ))
    text))

(defun helm-source-nngmail-get-candidates (account group messages)
  (mapcar (lambda (message)
	    (cons (helm-nngmail-format-message message) message))
	  messages))
  
(defun helm-source-nngmail-build (&optional group)
  (let ((data (mapcar (lambda (srv)
			      (nngmail-get-messages (car srv) (or group "UNREAD")))
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
  (format "nngmail:%s" (cdr (assq 'server candidate))))

(defun helm-nngmail-candidate-group (candidate)
  (gnus-group-full-name (cdr (assq 'group candidate))
			(helm-nngmail-candidate-server candidate)))


(defun helm-nngmail-action-read (candidate)
  (let ((articles (mapcar (lambda (candidate)
			    (let ((server
				   (helm-nngmail-candidate-server candidate))
				  (group
				   (helm-nngmail-candidate-group candidate))
				  (id (cdr (assq 'id candidate))))
			      (list group id)))
			  (helm-marked-candidates)))
	  ;; Pick first server and group of selected candidates because (I
	  ;; think) we need to pass something to nnir-run-query although
	  ;; the nngmail back end doesn't care.
	(srv (helm-nngmail-candidate-server (elt (helm-marked-candidates) 0)))
	(grp (helm-nngmail-candidate-group (elt (helm-marked-candidates) 0))))	
      (gnus-group-make-nnir-group
       nil
       `((nnir-query-spec . ((query . nil) (articles . ,articles)))
	 (nnir-group-spec . ((,srv (,grp))))))))
  )

(defun helm-nngmail-action-mark-read (candidate)
  (message (format "mark read nngmail+%s:%s %d"
		   (cdr (assq 'server candidate))
		   (cdr (assq 'group candidate))
		   (cdr (assq  'id candidate))))
  )

(defun helm-nngmail-action-expire (candidate)
  (message (format "expire nngmail+%s:%s %d"
		   (cdr (assq 'server candidate))
		   (cdr (assq 'group candidate))
		   (cdr (assq  'id candidate))))
  )

(defvar helm-nngmail-actions
  '(("Read" . (lambda (candidate)
		(helm-nngmail-action-read candidate)))
    ("Mark read" . (lambda (candidate)
		     (helm-nngmail-action-mark-read candidate)))
    ("Delete" . (lambda (candidate)
		  (helm-nngmail-action-expire candidate)))))

(defvar helm-nngmail-history-input nil)
(defvar helm-nngmail-label-history nil)

(defun helm-nngmail (&optional label)
  (interactive)
  (let ((label (if current-prefix-arg
		   (completing-read "Label : "
				    (nngmail-get-all-labels)
				    nil t nil
				    helm-nngmail-label-history)
		 "UNREAD")))
    (helm :sources (helm-source-nngmail-build label)
	  :buffer "*helm-nngmail*"
	  :prompt "Messages matching: "
	  :history 'helm-nngmail-history-input
	  :helm-candidate-number-limit 200)))

