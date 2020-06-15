;;; -*- lexical-binding: t; -*-
;; Author:  lee <loyalpartner@163.com>
;; URL:     https://github.com/loyalpartner/baidu-translator.el

(defconst baidu-translator-api-host "https://api.fanyi.baidu.com/api/trans/vip/translate")

(defcustom baidu-translator-appid "20200607000488675"
  "baidu appid" :type 'string)

(defcustom baidu-translator-secret-key "Nb_cT61hFraVEUpkvp33"
  "baidu secret key" :type 'string)

(define-derived-mode baidu-translator-mode nil "baidu-translator"
  "Major mode to show baidu translator result."
  (setq font-lock-defaults '(("\\<[a-zA-z-~]*\\>" . (1 font-lock-string-face)))))


(defvar baidu-translator-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'baidu-translator-quit)
    map))

(defun baidu-translator-quit ()
  (interactive)
  (bury-buffer))

;; https://fanyi-api.baidu.com/doc/21
(defun baidu-translator-get-result (from to text)
  (let* ((url-request-method        "POST")
        (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
        (salt (number-to-string (random)))
        (url-request-data (format "q=%s&salt=%s&appid=%s&sign=%s&from=%s&to=%s"
                                  (url-hexify-string text)
                                  salt
                                  baidu-translator-appid
                                  (md5 (concat baidu-translator-appid text  salt baidu-translator-secret-key) nil nil (coding-system-from-name "utf-8"))
                                  from to)))
    (with-current-buffer (url-retrieve-synchronously baidu-translator-api-host)
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (baidu-translator-extract-result (buffer-substring-no-properties (point) (point-max))))))

(defun baidu-translator-extract-result (string)
  (let* ((json (json-read-from-string string))
         (trans_result (assoc-default 'trans_result json)))
    (if trans_result
        (mapconcat (lambda (t)
                     (concat (assoc-default 'src t) "\n"
                             (assoc-default 'dst t) "\n"))
                   trans_result "\n")
      (assoc-default 'error_msg json))))

(defun baidu-translator-trim-tail (text)
  (setq text (replace-regexp-in-string "^\\*" "\n\n*" text))
  (setq text (replace-regexp-in-string "\\([^$]\\)\n\s*" "\\1 " text))
  ;; (setq text (replace-regexp-in-string "\\([^$]\\)\n\s*" "\\1 " text))
  (setq text (replace-regexp-in-string "\\([^0-9]+\\)\\.\s" "\\1.\n" text))
  ;; (setq text (replace-regexp-in-string ";" ";\n" text))
  text)

(defun baidu-translator-translate (from to text)
  (with-current-buffer (get-buffer-create "*baidu translator*")
    (setq buffer-read-only nil)
    (baidu-translator-mode)
    (visual-line-mode 1)
    (erase-buffer)
    (use-local-map baidu-translator-map)
    (insert (baidu-translator-get-result from to (baidu-translator-trim-tail text)))
    (setq buffer-read-only t)
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(defun baidu-translator-chinese-p (word)
  (and word (string-match "\\cc" word)))

(defun baidu-translator-at-point ()
  (interactive)
  (let ((word (thing-at-point 'word)))
    (baidu-translator-translate
     (if (baidu-translator-chinese-p word) "zh" "en")
     (if (baidu-translator-chinese-p word) "en" "zh")
     (thing-at-point 'sentence t))))

(with-eval-after-load "evil"
  (evil-define-operator evil-baidu-translate-operator (beg end type)
    (interactive "<R>")
    (let* ((text (buffer-substring-no-properties beg end))
           (word (thing-at-point 'word))
           (source (if (baidu-translator-chinese-p word) "zh" "en"))
           (target (if (baidu-translator-chinese-p word) "en" "zh")))
      (baidu-translator-translate source target text))))

(provide 'baidu-translator)
