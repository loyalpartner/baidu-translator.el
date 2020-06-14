;;; -*- lexical-binding: t; -*-

(defconst baidu-translator-api-host "https://api.fanyi.baidu.com/api/trans/vip/translate")

(defcustom baidu-translator-appid "20200607000488675"
  "baidu appid" :type 'string)

(defcustom baidu-translator-secret-key "Nb_cT61hFraVEUpkvp33"
  "baidu secret key" :type 'string)

(define-derived-mode baidu-translator-mode nil "baidu-translator")

(defvar baidu-translator-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'baidu-translator-quit)
    map))

(defun baidu-translator-quit ()
  (interactive)
  (bury-buffer))

(defun baidu-translator-get-result (from to text)
  (let* ((url-request-method        "POST")
        (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
        (salt (number-to-string (random)))
        (url-request-data (format "q=%s&salt=%s&appid=%s&sign=%s&from=%s&to=%s"
                                  (url-encode-url text)
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
    (mapconcat (lambda (t)
                 (concat (assoc-default 'src t)
                         "\n"
                         (assoc-default 'dst t)))
               trans_result "\n")))

(defun baidu-translator-trim-tail (text)
  (setq text (replace-regexp-in-string "\n\s*" " " text))
  (setq text (replace-regexp-in-string "\\.\s" ".\n" text))
  (setq text (replace-regexp-in-string ";" ";\n" text))
  text)

(defun baidu-translator-translate (from to text)
  (with-current-buffer (get-buffer-create "*baidu translator*")
    (setq buffer-read-only nil)
    (baidu-translator-mode)
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
     (substring-no-properties (thing-at-point 'sentence) 0))))

(with-eval-after-load "evil"
  (evil-define-operator evil-baidu-translate-operator (beg end type)
    (interactive "<R>")
    (let* ((text (buffer-substring-no-properties beg end))
           (word (thing-at-point 'word))
           (source (if (baidu-translator-chinese-p word) "zh" "en"))
           (target (if (baidu-translator-chinese-p word) "en" "zh")))
      (baidu-translator-translate source target text))))

(provide 'baidu-translator)
