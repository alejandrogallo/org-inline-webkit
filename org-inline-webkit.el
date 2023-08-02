(require 'xwidget)

(defgroup org-inline-webkit nil
  "Inline webkit blocks for org-mode"
  :group 'org-inline-webkit)

(defcustom org-inline-webkit-regex
  (rx (and "file:"
           (group
            (or
             ;; either https?://links
             (and (1+ (or "https:"
                          "http:"))
                  (1+ (not (any ":\n]"))))
             ;; or files:.*html?
             (and (1+ (not (any " \n]")))
                  (or ".htm"
                      ".html"))))))
  "The regex TODO"
  :type 'regex
  :group 'org-inline-webkit)

(defun org-inline-webkit--get-xwidget-or-create (path)
  (let ((title (format "%s widget: %s"
                       (current-buffer)
                       path)))
    (if-let ((found (cl-find-if (lambda (widget)
                                  (and (string=
                                        (xwidget-get widget :title)
                                        title)))
                                xwidget-list)))
        (prog1
            found
          (org-inline-webkit--resize found))
      (let ((new-widget (make-xwidget 'webkit
                                      title
                                      0
                                      0)))
        (org-inline-webkit--resize new-widget)
        (xwidget-put new-widget :title title)
        new-widget))))

(defun org-inline-webkit--match-current-line ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward org-inline-webkit-regex
                       (save-excursion (end-of-line)
                                       (point))
                       t 1)))

(defmacro org-inline-webkit--with-xwidget-at-point (xwidget-var &rest body)
  `(when (org-inline-webkit--match-current-line)
     (save-excursion
       (goto-char (match-beginning 0))
       (if-let ((,xwidget-var (xwidget-at (point))))
           ,@body))))

(defun org-inline-webkit--get-size (&optional pos)
  (save-excursion
    ;; be sure to be sometimes in a right positio
    ;; so that (org-element-at-point) works
    (when pos (goto-char pos))
    (cl-flet ((-plist-get-or (plist propname alternative)
                (if-let ((value (plist-get plist propname)))
                    (string-to-number value)
                  alternative)))
      (let* ((default-width (/ (window-pixel-width) 2))
             (default-height (/ (window-pixel-height) 4))
             (org-attrs (org-export-read-attribute
                         :attr_org
                         (org-element-at-point)))
             (html-attrs (org-export-read-attribute
                          :attr_html
                          (org-element-at-point)))
             (org-size (cons (-plist-get-or org-attrs :width default-width)
                             (-plist-get-or org-attrs :height default-height))))
        (list :org org-size
              ;; get the org-size as a fallback
              :html (cons (-plist-get-or html-attrs :width (car org-size))
                          (-plist-get-or html-attrs :height (cdr org-size))))))))

(defun org-inline-webkit--resize (widget)
  "Resize a widget with the org-inline-webkit machinery.
First check for a #+org_attr nearby and then the defaults."
  (cl-destructuring-bind (&key org &allow-other-keys)
      (org-inline-webkit--get-size)
    (cl-destructuring-bind (width . height) org
      (xwidget-resize widget width height))))

(defun org-inline-webkit-remove-all ()
  "Remove all org-inline-webkit widgets. The processes continue living."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward org-inline-webkit-regex nil t)
      (org-inline-webkit-remove))))

(defun org-inline-webkit-remove ()
  "Remove the org-inline-webkit widget at point. The proces continues living."
  (interactive)
  (org-inline-webkit--with-xwidget-at-point
   widget
   (replace-match (match-string-no-properties 0)
                  nil nil nil 0)))

(defun org-inline-webkit-kill ()
  "Remove and kill the org-inline-webkit widget at point.
This also kills the webkit proces in addition to removing it."
  (interactive)
  (org-inline-webkit--with-xwidget-at-point
   widget
   (org-inline-webkit-remove)
   (kill-xwidget widget)))

(defun org-inline-webkit-kill-all ()
  (interactive)
  (org-inline-webkit-remove-all)
  (dolist (widget (get-buffer-xwidgets (current-buffer)))
    (kill-xwidget widget)))

(defun org-inline-webkit-display-all (&optional arg)
  (interactive "P")
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward org-inline-webkit-regex nil t)
      (org-inline-webkit-display arg))))


(defun org-inline-webkit-display (&optional arg)
  (interactive "P")
  (save-excursion
    (when (org-inline-webkit--match-current-line)
      (goto-char (match-beginning 0))
      (let ((uri (match-string-no-properties 1))
            (path (match-string-no-properties 0))
            (match-data (match-data)))
        ;; if uri is a regular file, we need the file://
        ;; prefix
        (when (file-exists-p uri)
          (setq uri (format "file://%s"
                            (expand-file-name uri))))
        (if-let ((this-widget (xwidget-at (point))))
            (when arg
              (org-inline-webkit--resize this-widget)
              (xwidget-webkit-goto-uri this-widget uri))
          (let* ((widget (org-inline-webkit--get-xwidget-or-create path))
                 (replacement (propertize path
                                          'display
                                          (list 'xwidget :xwidget widget))))
            (xwidget-webkit-goto-uri widget uri)
            (set-match-data match-data)
            (replace-match replacement nil nil nil 0)))))))


(defun org-inline-webkit-export-iframe-p ()
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward (rx bol
                           "#+ORG_INLINE_WEBKIT_EXPORT_IFRAME:"
                           (0+ (any " "))
                           "t"
                           (0+ (any " "))
                           eol)
                       nil t)))

(advice-add
 'org-html-link
 :around
 (defun org-inline-webkit--format-image (fn link desc info)
   (if-let* ((do-export (org-inline-webkit-export-iframe-p))
             (raw-link (org-element-property :raw-link link))
             (uri (org-element-property :path link))
             (type (org-element-property :type link))
             (matched (string-match-p org-inline-webkit-regex
                                      raw-link)))
       (cl-destructuring-bind (&key html &allow-other-keys)
           (org-inline-webkit--get-size (org-element-property :begin
                                                              link))
         (cl-destructuring-bind (width . height) html
           (format "<iframe src='%s' frameBorder='0' width='%d' height='%d'></iframe>"
                   uri width height)))
     (funcall fn link desc info))))


(provide 'org-inline-webkit)
