;; TODO: add handling of tag/property queries
;; TODO: add functions to recover: text, properties
(defun orgpath-get (&optional query)
  (let* ((query (if (not (s-prefix-p "/" query)) (concat "//" query) query))
         (parsed-query (orgpath-split query)))
    (orgpath-filter parsed-query my-struct))
  )

;; /foo/bar/baz -> split into (foo bar baz), turn that into regexps, search for such header hierarchy
;; /foo//baz -> // means any depth
;; foo/bar -> bar under foo but at any depth
;; foo//bar -> bar any depth under foo at any depth

(setq my-struct '(("foo" ("bar" ("baz")) ("brum" ("quux")) ("klask" ("brum"))) ("boo") ("zoo" ("tralala")) ("foo" ("bar" ("baz"))) ("alpha" ("bravo" ("foo" ("baz"))))))
(setq my-struct '(("foo" ("bar" ("baz") ("qux")))))

(defun orgpath-filter (query struct)
  (cond
   ((null query) struct)
   (t (let* ((current-query-raw (car query))
             (current-query-re (save-match-data (string-match "/*\\(.*?\\)\\'" current-query-raw) (match-string 1 current-query-raw)))
             (next-level (-keep (lambda (it)
                                  (orgpath-match query
                                                 current-query-raw
                                                 current-query-re
                                                 it))
                                struct)))
        (-keep
         (lambda (str) (if (car str)
                           (--when-let (orgpath-filter (car str) (cddr str)) (cons (cadr str) it))
                         (cdr str)))
         next-level)))))

(defun orgpath-match (query query-raw query-re struct)
  (cond
   ((s-starts-with-p "//" query-raw)
    (if (string-match-p query-re (car struct))
        (cons (cdr query) struct)
      (cons query struct)))
   ((s-starts-with-p "/" query-raw)
    (when (string-match-p query-re (car struct))
      (cons (cdr query) struct)))))

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

(--map-filter (evenp it) (+ 1 it) '(1 2 3 4 5))
