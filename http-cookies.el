;;; http-cookies.el --- simple HTTP cookies implementation

;; Copyright (C) 2004, David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>
;; Version: 1.0.0
;; Keywords: hypermedia
;; URL: no URL yet.

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; see http://www.faqs.org/rfcs/rfc2109.html and
;; http://wp.netscape.com/newsref/std/cookie_spec.html

;;; Change log:

;;; TODO:

;;    - secure attribute will screw the parser
;;    - maybe convert max-age to expires (we don't send it back)
;;    - hash: hostname -> cookie, domain -> cookie
;;    - whitelist
;;    - blacklist

;;; Code:

(defgroup http-emacs ()
  "Simple HTTP client implementation in elisp.")

(defcustom http-emacs-use-cookies nil
  "Use cookies in the http-emacs package. *EXPERIMENTAL*"
  :type 'boolean
  :group 'http-emacs)

(defcustom http-emacs-cookie-file "~/.emacs-cookies"
  "*File where to store the cookies."
  :type 'file
  :group 'http-emacs)

(defconst http-token-value-regexp
  "^[ \t]*\\(.*?\\)[ \t]*=[ \t]*\"?\\(.*?\\)\"?[ \t]*;?[ \t]*$"
  "Regexp to match a token=\"value\"; in a cookie.")

(defvar http-cookies-accept-functions
  '(http-cookie-check-path
    http-cookie-check-domain
    http-cookie-check-hostname)
  "*List of functions used to determine if we accept a cookie or not.
If one of these function returns nil the cookie will be rejected.  Each
function can access the free variables `cookie', `host' (from the url)
`path' (from the URL) and `url' to make its decision.")



;; functions for parsing the header

(defun http-cookies-ns-to-rfc (line)
  "Make the header value LINE a bit more RFC compatible.
Make old netscape cookies a bit more RFC 2109 compatible by quoting
the \"expires\" value.  We need this to be able to properly split
the header value if there is more than one cookie."
  (let ((start 0))
    (while (string-match "expires[ \t]*=[ \t]*\\([^\";]+?\\)\\(;\\|$\\)"
                         line start)
      (setq start (match-end 0))
      (setq line (replace-match "\"\\1\"" t nil line 1)))
    line))

(defun http-find-char-in-string (char string &optional start)
  "Return the first position of CHAR in STRING.
If START is non-nil start at position START."
  (unless start
    (setq start 0))
  (let ((i start) (len (length string)) pos)
    (while (and (not pos) (< i len))
      (when (= (aref string i) char)
        (setq pos i))
      (setq i (1+ i)))
    pos))

(defun http-cookies-find-quoted-strings (header-value)
  "Return list of positions of quoted strings in HEADER_VALUE.
Return a list of pairs with the beginning and end of quoted strings
in a \"Set-cookie: \" header value."
  (let ((start 0) qstring-pos)
    (while (string-match "=[ \t]*\\(\".*?[^\\]\"\\)" header-value start)
      (add-to-list 'qstring-pos (cons (match-beginning 1) (1- (match-end 1))))
      (setq start (match-end 1)))
    qstring-pos))

(defun http-cookies-split-string (header-value sep-char)
  "Split the HEADER-VALUE at the character SEP-CHAR.
Ignores SEP-CHAR if it is in a quoted string.  Return a list of the
substrings."
  (let ((qstrings (http-cookies-find-quoted-strings header-value))
        (start 0) (beg 0) pos in-qstring strings)
    (while (setq pos (http-find-char-in-string sep-char header-value start))
      (unless (= pos start)           ; ignore empty strings
        ;; check if pos is in a quoted string
        (dolist (qstring-pos qstrings)
          (unless in-qstring
            (when (and (> pos (car qstring-pos)) (< pos (cdr qstring-pos)))
              (setq in-qstring t))))
        (if in-qstring
            (setq in-qstring nil)
          (add-to-list 'strings (substring header-value beg pos))
          (setq beg (1+ pos))))
      (setq start (1+ pos)))
    ;; add the last token
    (add-to-list 'strings (substring header-value beg))
    strings))

(defun http-cookies-parse-cookie (string)
  "Parse one cookie.
Return an alist ((NAME . VALUE) (attr1 . value1) (attr2 . value2) ...)
or nil on error."
  (let (name value attrs error)
    (dolist (attr (http-cookies-split-string string ?\;))
      (if (string-match http-token-value-regexp attr)
          (add-to-list 'attrs (cons (match-string 1 attr)
                                    (match-string 2 attr)))
        ;; match the secure attribute
        (if (string-match "[ \t]*\\([a-zA-Z]+\\)[ \t]*"  attr)
            (add-to-list 'attrs (cons (match-string 0 attr t)))
          (setq error t)
          (message "Cannot parse cookie %s" str))))
    (unless error
      attrs)))

(defun http-cookies-set (url headers)
  ;; The server may send several "Set-Cookie:" headers.
  (let ((host (http-cookies-hostname url)) (path (http-cookies-path url))
        header-value cookie)
    (dolist (line headers)
      (when (equal (car line) "set-cookie")
        (setq header-value (http-cookies-ns-to-rfc (cdr line)))
        ;; there may be several cookies separated by ","
        (dolist (raw-cookie (http-cookies-split-string header-value ?\,))
          (setq cookie (http-cookies-parse-cookie raw-cookie))
          (http-cookies-accept))))))



;; extract parts of the url

(defun http-cookies-hostname (url)
  "Return the hostname of URL"
  (unless (string-match
           "http://\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/\\(.*/\\)?\\([^:]*\\)"
           url)
    (error "Cannot parse URL %s." url))
  (match-string 1 url))

(defun http-cookies-path (url)
  "Return the path of the URL."
  (unless (string-match
           "http://\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/\\(.*/\\)?\\([^:]*\\)"
           url)
    (error "Cannot parse URL %s." url))
  (concat "/" (or (match-string 4 url) "")))



;; functions to check the cookie (implementation of 4.3.2 of RFC 2109)

(defun http-cookies-accept ()
  (let ((accept t))
    (dolist (fun http-cookies-accept-functions)
      (when accept
        (setq accept (funcall fun))))
    accept))

(defun http-cookie-check-path ()
  "Return nil if the \"path\" attribute is not a prefix of th URL."
  (let ((cookie-path (cdr (assoc "path" cookie))))
    (if cookie-path
        (if (string-match (concat "^" cookie-path) path)
            t
          (message "Rejecting cookie: path attribute \"%s\" is not a prefix\
 of the URL %s." cookie-path url)
          nil)
      t)))

(defun http-cookie-check-domain ()
  "Return nil if the domain is bogus.
Return nil if the domain does not start with a \".\" or does not contain
an embedded dot."
  (let ((domain (cdr (assoc "domain" cookie))))
    (if domain
        (if (string-match "^\\.[^.]+\\.[^.]+" domain)
            t
          (message "Rejection cookie: domain \"%s\" does not start with a dot\
 or does not contain an embedded dot." domain)
          nil)
      t)))

(defun http-cookie-check-hostname ()
  "Return nil if the domain doesn't match the host.
Return nil if the domain attribute does not match the host name or the
host name without the domain attribute still contains one or more dots."
  ;; FIXME: hostname might be an IP address
  (let ((domain (cdr (assoc "domain" cookie))))
    (if (not domain)
        t
      (when (string-match (concat domain "$") host)
        (not (http-find-char-in-string
              ?\. (substring host 0 (match-beginning 0))))))))



(provide 'http-cookies)

;;; http-cookies.el ends here