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

(defvar nngmail-virtual-servers ())

(defun nngmail-get-account-params (elem)
  (let ((nickname (plist-get elem 'nickname))
	(email (plist-get elem 'email))
	(id (plist-get elem 'id)))
    (cons nickname (list id email))))

(defun nngmail-get-account (nickname)
  (cdr (assoc-string nickname nngmail-virtual-servers)))

(defun nngmail-get-account-email (nickname)
  (elt (nngmail-get-account nickname) 1))

(defun nngmail-get-account-id (nickname)
  (elt (nngmail-get-account nickname) 0))

(defun nngmail-parse-accounts ()
  (setq nngmail-virtual-servers ())
  (goto-char url-http-end-of-headers)
  (let ((json-object-type 'plist)
        (json-key-type 'symbol)
        (json-array-type 'vector))
    (let* ((result (json-read))
	   (accounts (plist-get result 'accounts))
	   )
      ;; Do something with RESULT here
      (seq-map
       (lambda (elem)
	 (push (nngmail-get-account-params elem) nngmail-virtual-servers))
       accounts)
      )))

(defun nngmail-url-for (resource &rest id method)
  "Generate a URL to probe the resource."
  (format "%s/%ss/" nngmail-base-url resource))

(defun nngmail-get-accounts ()
  (url-retrieve (nngmail-url-for "account")
		(lambda (events)
		  (nngmail-parse-accounts)))
  (message (format "%s: %s"
		   (nngmail-get-account-id "itineris")
		   (nngmail-get-account-email "itineris"))))

(deffoo nngmail-open-server (nickname email)
  "Verify the nngmail server syncs the account."
  (interactive)
  (nngmail-get-accounts)
  (unless (nngmail-get-account nickname)
    (error
     (format "You are not synching the account %s:%s" email nickname)))
  (unless
      (string-equal (nngmail-get-account-email nickname) email)
    (error (format "Email address mismatch %s != %s" email
		   (nngmail-get-account-email nickname)))))
			

(provide 'nngmail)
;;; nngmail.el ends here
