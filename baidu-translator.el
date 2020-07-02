;;; baidu-translator.el --- baidu translator         -*- lexical-binding: t; -*-

;; Copyright (C) 2020  lee

;; Author: lee;;; -*- lexical-binding: t; -*- <loyalpartner@163.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'json)

(defgroup baidu-translator nil
  ""
  :group 'tools)

(defconst baidu-translator--api-host "https://api.fanyi.baidu.com/api/trans/vip/translate")

(defcustom baidu-translator-appid "20200607000488675"
  "baidu appid"
  :type 'string
  :group 'baidu-translator)

(defcustom baidu-translator-secret-key "Nb_cT61hFraVEUpkvp33"
  "baidu secret key"
  :type 'string
  :group 'baidu-translator)

(defcustom baidu-translator-show-delay 0.3
  "Delayed display"
  :type 'float
  :group 'baidu-translator)

(defcustom baidu-translator-default-show-function 'baidu-translator-show-result-at-bottom
  ""
  :type 'function
  :group 'baidu-translator)

(defcustom baidu-translator--buffer "*baidu-translator*"
  "*baidu-translator*"
  :type 'string
  :group 'baidu-translator)

(defvar baidu-translator--timer nil "")

(defvar baidu-translator-last-focused-sentence "")

(defcustom baidu-translator-cache-file "~/.baidu-translator" ""
  :type 'string
  :group 'baidu-translator)

(defvar baidu-translator--cache-data
  (if (file-exists-p baidu-translator-cache-file)
      (with-temp-buffer
        (insert-file-contents baidu-translator-cache-file)
        (read (buffer-string)))
    (make-hash-table :test #'equal)))

(defcustom baidu-translator-target-language "zh" ""
  :type 'string
  :group 'baidu-translator)

(defface baidu-translator-tooltip-face
  '((t (:foreground "green" :background "gray12")))
  "Face for sdcv tooltip"
  :group 'sdcv)

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
    isearch-exit
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
           (make-local-variable 'baidu-translator-last-focused-sentence)
           (add-hook 'post-command-hook #'baidu-translator-translate-thing-at-point nil t)))
        (t (remove-hook 'post-command-hook #'baidu-translator-translate-thing-at-point t))))

(defun baidu-translator--transform-special-text (text)
  (when text
    (when (derived-mode-p 'Info-mode)
      (setq text (replace-regexp-in-string "^‘\\(.*\\)’$" "\\1\n" text)) ;; ‘inhibit-same-window’
      (setq text (replace-regexp-in-string "\\*Note \\([^:]*\\)::" "See \\1" text))
      (setq text (replace-regexp-in-string "^[\\*-]{2,}" "" text)) ; remove ****** or ------------
      (setq text (replace-regexp-in-string "^\s*-- .*$" "\\&\n" text)) ; -- function 
      (setq text (replace-regexp-in-string "^[=\\*-]+$" "" text)) ; ============ -----------
      )
    (setq text (replace-regexp-in-string "\\([^$]\\)\n\s*" "\\1 " text))) ;
  text)

(defun baidu-translator--chinese-p (word)
  (and word (string-match "\\cc" word)))

(defun baidu-translator--http-post (url data)
  (let* ((url-request-method        "POST")
         (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data data))
    (with-current-buffer (url-retrieve-synchronously url)
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (buffer-substring-no-properties (point) (point-max)))))

(defun baidu-translator--generate-sign (text salt)
  (md5 (concat baidu-translator-appid text salt baidu-translator-secret-key) nil nil (coding-system-from-name "utf-8")))

(defun baidu-translator--need-translate-p ()
  (and (sentence-at-point)
       (memq this-command baidu-translator-move-commands)
       (not (string= (baidu-translator--sentence-at-point)
                     baidu-translator-last-focused-sentence))))

(defun baidu-translator--sentence-at-point ()
  (let ((sentence-end-double-space (if (derived-mode-p '(Info-mode)) t nil)))
    (baidu-translator--transform-special-text
     (thing-at-point 'sentence t))))

;; https://fanyi-api.baidu.com/doc/21
(defun baidu-translator-get-result (from to text)
  "url request"
  (let* ((salt (number-to-string (random)))
         (data (format "q=%s&salt=%s&appid=%s&sign=%s&from=%s&to=%s"
                       (url-hexify-string text) salt baidu-translator-appid
                       (baidu-translator--generate-sign text salt) from to)))
    (baidu-translator-extract-result (baidu-translator--http-post baidu-translator--api-host data))))

(defun baidu-translator-extract-result (string)
  (let* ((json (json-read-from-string string))
         (trans_result (assoc-default 'trans_result json)))
    (if trans_result
        (mapconcat (lambda (json)
                     (concat (assoc-default 'src json) "\n"
                             (assoc-default 'dst json) "\n"))
                   trans_result "\n")
      (assoc-default 'error_msg json))))

(defun baidu-translator-show-result-at-bottom (result)
  (with-current-buffer (get-buffer-create baidu-translator--buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert result)
    (baidu-translator-mode)
    (visual-line-mode 1)
    (use-local-map baidu-translator-map)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (display-buffer-in-side-window (current-buffer) '((side . bottom)
                                                      (slot)
                                                      (window-height . 0.2)))
    ;; (display-buffer (current-buffer))
    ))

(defun baidu-translator-show-result-with-posframe (result)
  (require 'posframe)
  (when (posframe-workable-p)
    (posframe-show
     baidu-translator--buffer
     :string result
     :timeout 20
     :poshandler 'posframe-poshandler-frame-bottom-center
     :min-width (frame-width)
     :background-color (face-attribute 'baidu-translator-tooltip-face :background)
     :foreground-color (face-attribute 'baidu-translator-tooltip-face :foreground)
     :internal-border-width 10)
    (unwind-protect
        (push (read-event) unread-command-events)
      (posframe-delete baidu-translator--buffer))))

(defun baidu-translator-translate (from to text)
  (let* ((result (baidu-translator-get-result from to text)))
    (funcall baidu-translator-default-show-function result)
    (setq baidu-translator-last-focused-sentence text)
    (unless (or (string= "Invalid Access Limit" result)
                (string= "TIMEOUT" result))
      
      (puthash text result baidu-translator--cache-data))))

(defmacro baidu-translator-delay-handle (func &rest args)
  `(setq baidu-translator--timer
         (run-with-idle-timer
          ,baidu-translator-show-delay nil
          (lambda (&rest args)
            (apply ,func args))
          ,@args)))

(defun baidu-translator-translate-thing-at-point ()
  (interactive)
  (when-let ((_ (baidu-translator--need-translate-p))
             (sentence (baidu-translator--sentence-at-point)))

    (when baidu-translator--timer
      (cancel-timer baidu-translator--timer))

    (setq baidu-translator-last-focused-sentence sentence)
    (if-let ((cached-result
              (gethash sentence baidu-translator--cache-data)))
        (baidu-translator-delay-handle baidu-translator-default-show-function cached-result)
      (baidu-translator-delay-handle #'baidu-translator-translate
                                     "auto" baidu-translator-target-language sentence))))

(defun baidu-translator-quit ()
  (interactive)
  (kill-buffer-and-window))

(defun baidu-translator-persist-cache ()
  (interactive)
  (with-temp-file baidu-translator-cache-file
    (insert (prin1-to-string baidu-translator--cache-data))))


(provide 'baidu-translator)
