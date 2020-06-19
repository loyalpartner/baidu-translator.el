;;; -*- lexical-binding: t; -*-
;; Author:  lee <loyalpartner@163.com>
;; URL:     https://github.com/loyalpartner/baidu-translator.el

(defconst baidu-translator--api-host "https://api.fanyi.baidu.com/api/trans/vip/translate")

(defcustom baidu-translator-appid "20200607000488675"
  "baidu appid" :type 'string)

(defcustom baidu-translator-secret-key "Nb_cT61hFraVEUpkvp33"
  "baidu secret key" :type 'string)

(defcustom baidu-translator-show-delay 0.5
  "Delayed display" :type 'float)

(defvar baidu-translator-last-focused-thing "")

(defcustom baidu-translator-last-focused-thing-type 'sentence
  "baidu appid" :type 'symbol)

(defvar baidu-translator--timer nil "")

;; https://stackoverflow.com/questions/19649872/get-list-of-interactive-functions-in-elisp-emacs
;; (mapconcat #'symbol-name (seq-filter #'commandp (apropos-internal "^next-")) "\n")
(defvar baidu-translator-move-commands
  '(next-line
    previous-line
    backward-button
    backward-char
    backward-delete-char
    backward-delete-char-untabify
    backward-kill-paragraph
    backward-kill-sentence
    backward-kill-sexp
    backward-kill-word
    backward-list
    backward-page
    backward-paragraph
    backward-sentence
    backward-sexp
    backward-to-indentation
    backward-to-word
    backward-up-list
    backward-word
    forward-button
    forward-char
    forward-line
    forward-list
    forward-page
    forward-paragraph
    forward-same-syntax
    forward-sentence
    forward-sexp
    forward-symbol
    forward-to-indentation
    forward-to-word
    forward-whitespace
    forward-word
    evil-forward-WORD-begin
    evil-forward-WORD-end
    evil-forward-arg
    evil-forward-char
    evil-forward-paragraph
    evil-forward-section-begin
    evil-forward-section-end
    evil-forward-sentence-begin
    evil-forward-word-begin
    evil-forward-word-end
    evil-backward-WORD-begin
    evil-backward-WORD-end
    evil-backward-arg
    evil-backward-char
    evil-backward-paragraph
    evil-backward-section-begin
    evil-backward-section-end
    evil-backward-sentence-begin
    evil-backward-word-begin
    evil-backward-word-end
    evil-next-line
    evil-previous-line
    baidu-translator-translate-thing-at-point))

(defvar baidu-translator-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'baidu-translator-quit)
    map))

(define-derived-mode baidu-translator-mode nil "baidu-translator"
  "Major mode to show baidu translator result."
  (setq font-lock-defaults '(("\\<[a-zA-z-~]*\\>" . (1 font-lock-string-face)))))

(define-minor-mode baidu-translator-translate-mode
  "Instant translation mode"
  :lighter " foo"
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (cond (baidu-translator-translate-mode
         (progn
           (make-local-variable 'baidu-translator-last-focused-thing)
           (add-hook 'post-command-hook #'baidu-translator-translate-thing-at-point nil t)))
        (t (remove-hook 'post-command-hook #'baidu-translator-translate-thing-at-point t))))

(defun baidu-translator--trim-tail (text)
  (setq text (replace-regexp-in-string "^\\*" "\n\n*" text))
  (setq text (replace-regexp-in-string "\\(^\s*[0-9]+\\..*$\\)" "\\1\n" text))
  (setq text (replace-regexp-in-string "\\([^$]\\)\n\s*" "\\1 " text))
  ;; (setq text (replace-regexp-in-string "\\([^0-9]+\\)\\.\s" "\\1.\n" text))
  (setq text (replace-regexp-in-string "\\([^0-9]\\.\s\\)" "\\1\n" text))
  text)

(defun baidu-translator--chinese-p (word)
  (and word (string-match "\\cc" word)))

(defun baidu-translator-extract-result (string)
  (let* ((json (json-read-from-string string))
         (trans_result (assoc-default 'trans_result json)))
    (if trans_result
        (mapconcat (lambda (json)
                     (concat (assoc-default 'src json) "\n"
                             (assoc-default 'dst json) "\n"))
                   trans_result "\n")
      (assoc-default 'error_msg json))))

;; https://fanyi-api.baidu.com/doc/21
(defun baidu-translator-get-result (from to text)
  "url request"
  (let* ((url-request-method        "POST")
         (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
         (salt (number-to-string (random)))
         (url-request-data (format "q=%s&salt=%s&appid=%s&sign=%s&from=%s&to=%s"
                                   (url-hexify-string text)
                                   salt
                                   baidu-translator-appid
                                   (md5 (concat baidu-translator-appid text  salt baidu-translator-secret-key) nil nil (coding-system-from-name "utf-8"))
                                   from to)))
    (with-current-buffer (url-retrieve-synchronously baidu-translator--api-host)
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (baidu-translator-extract-result (buffer-substring-no-properties (point) (point-max))))))

(defun baidu-translator-translate (from to text)
  (with-current-buffer (get-buffer-create "*baidu translator*")
    (setq buffer-read-only nil)
    (baidu-translator-mode)
    (visual-line-mode 1)
    (erase-buffer)
    (use-local-map baidu-translator-map)
    (insert (baidu-translator-get-result from to (baidu-translator--trim-tail text)))
    (setq buffer-read-only t)
    (goto-char (point-min))
    ;; (display-buffer-reuse-window (current-buffer) '((side . bottom)))
    (display-buffer (current-buffer))
    ))


(defun baidu-translator-quit ()
  (interactive)
  (kill-buffer-and-window))

(defun baidu-translator-translate-thing-at-point ()
  (interactive)
  (let* ((sentence (thing-at-point baidu-translator-last-focused-thing-type t))
         (word (thing-at-point 'word t))
         (from (if (baidu-translator--chinese-p word) "zh" "en"))
         (to (if (baidu-translator--chinese-p word) "en" "zh")))
    (cond ((not sentence))
          ((not (memq this-command baidu-translator-move-commands)))
          ((string= sentence baidu-translator-last-focused-thing))
          
          (t (when baidu-translator--timer (cancel-timer baidu-translator--timer))
             (setq baidu-translator-last-focused-thing sentence
                   baidu-translator--timer (run-with-idle-timer
                                           baidu-translator-show-delay nil
                                           (apply-partially 'baidu-translator-translate from to sentence)))))))

(with-eval-after-load "evil"
  (evil-define-operator evil-baidu-translate-operator (beg end type)
    (interactive "<R>")
    (let* ((text (buffer-substring-no-properties beg end))
           (word (thing-at-point 'word))
           (source (if (baidu-translator--chinese-p word) "zh" "en"))
           (target (if (baidu-translator--chinese-p word) "en" "zh")))
      (baidu-translator-translate source target text))))

(provide 'baidu-translator)

