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

(defconst nngmail-src-dir (file-name-directory load-file-name))

(defun nngmail-start-server ()
  "Start an nnserver to sync the user's gmail account(s)."
  (let* ((program (expand-file-name "~/.pyenv/shims/nnserver"))
	 (buf (generate-new-buffer (concat "*nngmail*")))
	 (config-file (expand-file-name "config.yaml" nngmail-src-dir))
	 (some-other-stuff "blah blah blah")
	 (proc (start-process "nngmail" buf program config-file)))
    (message "nngmail sync daemon started")))

(provide 'nngmail)
;;;
