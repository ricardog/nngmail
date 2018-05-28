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

(defvoo nngmail-address "localhost"
  "The address of the gmail proxy.")

(defvoo nngmail-user nil
  "Username to use for authentication to the IMAP server.")

(defvoo nngmail-server-port 1234
  "The port the gmail proxy listens on.")

(defvoo nngmail-split-methods nil
  "How mail is split.
Uses the same syntax as `nnmail-split-methods'.")

(defvoo nngmail-split-fancy nil
  "Uses the same syntax as `nnmail-split-fancy'.")

(defun nngmail-decode-gnus-group (group)
  (decode-coding-string group 'utf-8))

(defun nngmail-encode-gnus-group (group)
  (encode-coding-string group 'utf-8))

(defvar nngmail-status-string "")

;(defconst nngmail-src-dir (file-name-directory load-file-name))

(defun nngmail-make-config (account)
  "Create the YAML file for synching the user's email account.
The user's email address comes from ACCOUNT."
  (let ((tfile (make-temp-file "nngmail" nil ".yml")))
    (with-temp-file tfile
      (insert-file-contents
       (expand-file-name "config.yaml.tmpl" nngmail-src-dir))
      (goto-char (point-min))
      (while (re-search-forward "%%[[:alnum:]]+%%" nil t)
	(message "found a template pattern")
	(or (and (string-equal (match-string 0) "%%account%%")
		 (replace-match account))
	    nil)))
    (expand-file-name tfile)))

(defun nngmail-start-server (account)
  "Start an nnserver for the user's email ACCOUNT."
  (let* ((program (expand-file-name "~/.pyenv/shims/nnserver"))
	 (buf (generate-new-buffer (concat "*" "nngmail-" account "*")))
	 (config-file (nngmail-make-config account))
	 (some-other-stuff "blah blah blah")
	 (proc (start-process "nngmail" buf program config-file)))
    (message "nngmail sync daemon started")))

(provide 'nngmail)
;;;
