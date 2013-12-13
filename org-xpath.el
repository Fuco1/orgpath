(defun orgpath-get (&optional query)
  (let* ((query (if (not (s-prefix-p "/" query)) (concat "//" query) query))
         (parsed-query (orgpath-split query)))
    ;; TODO: replace `org-element-parse-buffer' with something that
    ;; adds only :begin and :raw-value properties to make it faster
    (orgpath-filter parsed-query (cddr (org-element-parse-buffer 'headline)))))

;; /foo/bar/baz -> split into (foo bar baz), turn that into regexps, search for such header hierarchy
;; /foo//baz -> // means any depth
;; foo/bar -> bar under foo but at any depth
;; foo//bar -> bar any depth under foo at any depth
;; /foo[STYLE=habit]text() -> select text content under this node
;; foo/bar/[+work]/baz

;; /foo/bar/*[2] -> select 2nd child of /foo/bar whatever its header name is. Has some special values such as last() etc.

(setq my-struct '(("foo" ("bar" ("baz")) ("brum" ("quux")) ("klask" ("brum"))) ("boo") ("zoo" ("tralala")) ("foo" ("bar" ("baz"))) ("alpha" ("bravo" ("foo" ("baz"))))))
(setq my-struct '(("foo" ("bar" ("baz") ("qux")))))

(defun orgpath-is-command (query-raw)
  (and query-raw
       (or (string-match-p "text()" query-raw)
           (string-match-p "attr(.+?)" query-raw))))

(defun orgpath-parse-command (query-raw)
  (save-match-data
    (cond
     ((string-match-p "text()" query-raw)
      '(lambda (elem) (orgpath-command-text elem))
      )
     ((string-match "attr(\\(.+?\\))" query-raw)
      `(lambda (elem) (orgpath-command-property elem ,(match-string 1 query-raw)))))))

(defun orgpath-filter (query struct)
  (cond
   ((null query) struct)
   ;; otherwise filter
   (t (let* ((current-query-raw (car query))
             (current-query-re (save-match-data
                                 (let ((start (progn
                                                (string-match "/*\\(.*?\\)\\'" current-query-raw)
                                                (match-beginning 1)))
                                       (end (string-match "\\[\\|\\'" current-query-raw)))
                                   (substring current-query-raw start end))))
             (current-query-attr (save-match-data
                                   (when (string-match "\\[\\(.*?\\)\\]" current-query-raw)
                                     (let (todo-only)
                                       (cdr (org-make-tags-matcher
                                             (orgpath-format-attributes (match-string 1 current-query-raw))))))))
             (next-level (-keep (lambda (it)
                                  (orgpath-match query
                                                 current-query-raw
                                                 current-query-re
                                                 current-query-attr
                                                 it))
                                struct)))
        (-keep
         (lambda (headline)
           (if (car headline)
               (cond
                ((orgpath-is-command (caar headline))
                 (let ((command (orgpath-parse-command (caar headline))))
                   (cons (cadr headline) (cons (caddr headline)
                                               (progn
                                                 (goto-char (plist-get (caddr headline) :begin))
                                                 (funcall command (org-element-at-point)))))))
                (t (--when-let (orgpath-filter (car headline) (cdddr headline))
                     (cons (cadr headline) (cons (caddr headline) it)))))
             (cdr headline)))
         next-level)))))

(defun orgpath-match (query query-raw query-re query-attr struct)
  (let ((header-name (plist-get (cadr struct) :raw-value)))
    (cond
     ((s-starts-with-p "//" query-raw)
      (if (and (string-match-p query-re header-name)
               (if (not query-attr) t
                 (goto-char (plist-get (cadr struct) :begin))
                 (let ((tags-list (org-get-tags-at)))
                   (eval query-attr))))
          (cons (cdr query) struct)
        (cons query struct)))
     ((s-starts-with-p "/" query-raw)
      (when (and (string-match-p query-re header-name)
                 (if (not query-attr) t
                   (goto-char (plist-get (cadr struct) :begin))
                   (eval query-attr)))
        (cons (cdr query) struct))))))

(defun orgpath-command-text (elem)
  (save-excursion
    (let* ((contents-begin (plist-get (cadr elem) :begin)))
      (goto-char contents-begin)
      (buffer-substring-no-properties
       (org-end-of-meta-data-and-drawers)
       (progn
         (unless (org-on-heading-p)
           (outline-next-heading)
           (when (looking-back "^") (backward-char)))
         (point))))))

(defun orgpath-command-property (elem name)
  (let* ((value (plist-get (cadr elem) (intern (concat ":" (upcase name))))))
    value))

(defun -map-filter (pred fn list)
  (-map fn (-filter pred list)))

(defmacro --map-filter (pred-form fn-form list)
  `(--map ,fn-form (--filter ,pred-form ,list)))

(defun orgpath-split (string)
  (save-match-data
    (let (re)
      (while (string-match "\\(/?.+?\\)\\(?:/\\|\\'\\)" string)
        (push (match-string 1 string) re)
        (setq string (substring string (match-end 1))))
      (reverse re))))

(defun orgpath-format-attributes (query-attr)
  (with-temp-buffer
    (insert query-attr)
    (goto-char (point-min))
    (while (re-search-forward "@\\(.*?\\)=" nil t)
      (replace-match (concat (upcase (match-string 1)) "=")))
    (goto-char (point-min))
    (while (re-search-forward "=\\(\\sw+\\)" nil t)
      (replace-match (concat "=\"" (match-string 1) "\"")))
    (buffer-string)))

(defun s-replace-complex (needle replace string)
  (with-temp-buffer
    (insert string)
    (while (re-search-forward needle nil t)
      (replace-match ))
    (buffer-string))
  )

(--map-filter (evenp it) (+ 1 it) '(1 2 3 4 5))
