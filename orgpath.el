;;; orgpath.el --- XPath-like selection for org-mode.

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Version: 0.1
;; Created: 22nd December 2013
;; Keywords: org
;; Package-Requires: ((dash "2.4.0") (s "1.8.0"))
;; URL: https://github.com/Fuco1/orgpath

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides XPath-like navigation for org-mode.
;; Currently it is only an API for a programmer, later some other
;; features, such as sparse displays based on the xpath query are
;; planned.
;;
;; For complete description see https://github.com/Fuco1/orgpath

;;; Code:
(require 'dash)
(require 's)
(require 'org)

(defun orgpath-get (query &optional deep)
  (save-excursion
    (let* ((query (if (not (s-prefix-p "/" query)) (concat "//" query) query))
           (parsed-query (orgpath-split query)))
      (orgpath-filter parsed-query (orgpath-parse-buffer) deep))))

(defun orgpath-parse-buffer ()
  "Parse an `org-mode' buffer and return a tree structure
representing all headers and subheaders.

This is similar to `org-element-parse-buffer' but keeps only
information relevant for further processing, and so it is
significantly faster."
  (flet ((add-to-end
          (new-end stack)
          (setcdr (car stack) new-end)
          (setcar stack new-end)))
    (let* ((headers-raw (org-map-entries (lambda () (cons (point) (org-heading-components)))))
           (headers (--map (list 'headline (list :raw-value (nth 5 it)
                                                 :begin (car it)
                                                 :level (cadr it)
                                                 )) headers-raw))
           (last-level 1)
           (start (cons nil nil))
           (stack (cons start nil)))
      ;; warning: this modifies headers by side effect
      (--each headers
        (let ((current-level (plist-get (cadr it) :level)))
          (cond
           ((= current-level last-level)
            (add-to-end (cons it nil) stack))
           ((> current-level last-level)
            (push (last (caar stack)) stack)
            (add-to-end (cons it nil) stack))
           ((< current-level last-level)
            (let ((diff (- last-level current-level)))
              (dotimes (i diff) (pop stack)))
            (add-to-end (cons it nil) stack)))
          (setq last-level current-level)))
      (cdr start))))

;; /foo/bar/baz -> split into (foo bar baz), turn that into regexps, search for such header hierarchy
;; /foo//baz -> // means any depth
;; foo/bar -> bar under foo but at any depth
;; foo//bar -> bar any depth under foo at any depth
;; /foo[STYLE=habit]/text() -> select text content under this node
;; foo/bar/[+work]/baz

;; /foo/bar/*[2] -> select 2nd child of /foo/bar whatever its header name is. Has some special values such as last() etc.

(defun orgpath-is-command (query-raw)
  (and query-raw
       (or (string-match-p "text()" query-raw)
           (string-match-p "attr(.+?)" query-raw))))

(defun orgpath-parse-command (query-raw)
  (save-match-data
    (cond
     ((string-match-p "text()" query-raw)
      '(lambda (elem) (orgpath-command-text elem)))
     ((string-match "attr(\\(.+?\\))" query-raw)
      `(lambda (elem) (orgpath-command-property elem ,(match-string 1 query-raw)))))))

(defun orgpath-last-match (query string)
  (let ((last-match-data))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward query nil t)
        (setq last-match-data (match-data)))
      (set-match-data (-map '1- last-match-data))
      (match-beginning 0))))

(defun orgpath-filter (query struct deep)
  (cond
   ((null query) struct)
   ;; otherwise filter
   (t (let* ((current-query-raw (car query))
             ;; the query can contain [] with the regexp semantics,
             ;; the attribute filter has to be the last [] block in
             ;; the query.  If regexp [] is at the end, we need to add
             ;; an empty block to signify this, e.g. "header[12][]"
             (current-query-re (save-match-data
                                 (let ((start (progn
                                                (string-match "/*\\(.*?\\)\\'" current-query-raw)
                                                (match-beginning 1)))
                                       (end (or (and (string-match-p "\\]\\'" current-query-raw)
                                                     (orgpath-last-match "\\[" current-query-raw))
                                                (length current-query-raw))))
                                   (substring current-query-raw start end))))
             (current-query-attr (save-match-data
                                   (when (string-match "\\]\\'" current-query-raw)
                                     (let (todo-only)
                                       (cdr (org-make-tags-matcher
                                             (orgpath-format-attributes
                                              (substring current-query-raw
                                                         (1+ (orgpath-last-match "\\[" current-query-raw))
                                                         (1- (length current-query-raw))))))))))
             (next-level (-keep (lambda (it)
                                  (orgpath-match query
                                                 current-query-raw
                                                 current-query-re
                                                 current-query-attr
                                                 it))
                                struct)))
        (let ((re (-keep
                   (lambda (headline)
                     (if (car headline)
                         (cond
                          ((orgpath-is-command (caar headline))
                           (let ((command (orgpath-parse-command (caar headline))))
                             (-snoc (cons (cadr headline) (list (caddr headline)))
                                    (progn
                                      (goto-char (plist-get (caddr headline) :begin))
                                      (funcall command (org-element-at-point))))))
                          (t (--when-let (orgpath-filter (car headline) (cdddr headline) deep)
                               (if (and (not deep)
                                        (= (length (car headline)) (length query)))
                                   (cons 'unwrap it)
                                 (cons (cadr headline) (cons (caddr headline) it))))))
                       (cdr headline)))
                   next-level)))
          (if (and (not deep) re)
              (--mapcat (if (eq (car it) 'unwrap) (cdr it) (list it)) re)
            re))))))

;; TODO: refactor the common attribute part
(defun orgpath-match (query query-raw query-re query-attr struct)
  (let ((header-name (plist-get (cadr struct) :raw-value)))
    (cond
     ((s-starts-with-p "//" query-raw)
      (if (and (string-match-p query-re header-name)
               (if (not query-attr) t
                 (goto-char (plist-get (cadr struct) :begin))
                 (let ((tags-list (org-get-tags-at))
                       (todo (org-get-todo-state)))
                   (eval query-attr))))
          (cons (cdr query) struct)
        (cons query struct)))
     ((s-starts-with-p "/" query-raw)
      (when (and (string-match-p query-re header-name)
                 (if (not query-attr) t
                   (goto-char (plist-get (cadr struct) :begin))
                   (let ((tags-list (org-get-tags-at))
                         (todo (org-get-todo-state)))
                     (eval query-attr))))
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

(provide 'orgpath)
;;; orgpath.el ends here
