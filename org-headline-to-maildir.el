;;; org-headline-to-maildir.el --- Download links in org headlines to maildir  -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Dr. Ian FitzPatrick

;; Author: Dr. Ian FitzPatrick <ian@ianfitzpatrick.eu>
;; URL: https://github.com/ifitzpat/org-headline-to-maildir
;; Version: 0.1-pre
;; Package-Requires: ((emacs "27.0"))
;; Keywords: maildir, org-mode, mu4e

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

;; This package downloads links from org-mode headlines as maildir entries

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + foo
;; + bar

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'org-headline-to-maildir)

;;;; Usage

;; Run one of these commands:

;; `org-headline-to-maildir-command': Frobnicate the flange.

;;;; Tips

;; + You can customize settings in the `org-headline-to-maildir' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-element)
(require 'thingatpt)

;;;; Customization

;; (defgroup org-headline-to-maildir nil
;;   "Settings for `org-headline-to-maildir'."
;;   :link '(url-link "https://github.com/ifitzpat/org-headline-to-maildir"))

;; (defcustom org-headline-to-maildir-something nil
;;   "This setting does something."
;;   :type 'something)

;;;; Variables

(defvar machine-hostname
  (replace-regexp-in-string
   "[_-]" "" (replace-regexp-in-string
	      "\n" "" (shell-command-to-string "hostname"))))

(defvar org-headline-to-maildir-file nil
  "A variable.")

(defvar org-headline-to-maildir-directory nil
  "A variable.")


;;;; Commands

;;;###autoload
(defun org-headline-to-maildir ()
  "Frobnicate the flange."
  (interactive)
  (with-current-buffer
      (or (get-file-buffer org-headline-to-maildir-file)
	  (find-file-noselect org-headline-to-maildir-file t))
    (org-map-entries 'send-untagged-to-maildir)
    (save-buffer)))

;;;; Functions

;;;;; Public


;;;;; Private
(defun retrieve-val-in-metadata (key metadata)
  (car (cdr (car (seq-filter (lambda (x) (string= key (car x))) metadata)))))

(defun format-as-maildir (url)
  (let* ((date (format-time-string "%A, %d %b %Y %H:%M:%S %z (%Z)"))
	 (metadata (url2org-reader-metadata url))
	 (subject (retrieve-val-in-metadata "Title" metadata))
	 (author (or (retrieve-val-in-metadata "Byline" metadata) "unknown"))
	 (hash (secure-hash 'sha256 (concat date subject author)))
	 (body (url2org-reader-body url)))
    (format "MIME-Version: 1.0
Date: %s
Subject: %s
From: \"%s\" <none@example.com>
List-ID: <refile>
Content-Type: text/html;
              charset=\"utf-8\"
X-org-headline-to-maildir-simple-hash: %s

<!DOCTYPE html><html><body>
<p><a href=\"%s\">Original</a></p>

%s
</body></html>
  " date subject author hash url body)))


(defun retrieve-url-in-body (upper-bound)
  (let ((urls nil))
    (save-excursion
      (while (re-search-forward "http" upper-bound t)
	(push (thing-at-point-url-at-point) urls)))
    urls))

(defun write-url-to-maildir (url)
  (with-temp-buffer
    (insert (format-as-maildir url))
    (write-region nil nil (concat org-headline-to-maildir-directory "/new/" (maildir-filename)) nil nil nil 'excl)))

(defun url2org-reader-metadata (url)
  (let* ((metadata (shell-command-to-string (concat "rdrview -M " url)))
	 (metadata (replace-regexp-in-string ": " "," metadata))
	 (metadata (split-string metadata "\n"))
	 (metadata (mapcar (lambda (x) (split-string x ",")) metadata)))
    metadata))

(defun url2org-reader-body (url)
  (shell-command-to-string (concat "rdrview -H " url)))


(defun maildir-filename ()
  (concat (format-time-string
	   "%s") "."
	   (replace-regexp-in-string "\n" ""
				     (shell-command-to-string
				      "pidof emacs | cut -f 1 -d ' '"))
	   "_" (number-to-string (random 99999)) "." machine-hostname))


(defun end-of-org-element-at-point ()
  (org-element-property :contents-end (org-element-context)))

(defun start-of-org-element-at-point ()
  (org-element-property :begin (org-element-context)))

(defun all-tags-at-point ()
  (car (last (org-heading-components))))

(defun org-headline-to-maildir--find-all (regex string)
  (let ((matches))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward regex (point-max) t)
	(push (match-string 1) matches)))
    matches))

(defun tagged? ()
  (org-headline-to-maildir--find-all machine-hostname (or (all-tags-at-point) "")))

(defun toggle-machine-name ()
  (org-toggle-tag machine-hostname 'on)
  (save-buffer))

(defun send-untagged-to-maildir ()
  (let ((start (start-of-org-element-at-point)))
    (if (not (tagged?))
	(save-excursion
	  (mapc 'write-url-to-maildir (retrieve-url-in-body (end-of-org-element-at-point)))
	  (goto-char start)
	  (toggle-machine-name)))))

;;;; Footer

(provide 'org-headline-to-maildir)

;;; org-headline-to-maildir.el ends here
