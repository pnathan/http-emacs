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

;;

;;; Code:

(defconst http-token-regexp "[^][()<>@,;:\\\\\"/?={} \t\xf0-\xff\x00-\x1f]+"
  "Token as described in RFC 2616.")

(defgroup http-emacs ()
  "Simple HTTP client implementation in elisp.")

(defcustom http-emacs-use-cookies nil
  "Use cookies in the http-emacs package. *EXPERIMENTAL*"
  :type 'boolean
  :group 'http-emacs)

(defcustom http-emacs-cookie-file "~/.emacs-cookies"
  "File where to store the cookies."
  :type 'file
  :group 'http-emacs)


(defun http-cookies-ns-to-rfc (line)
  "Make the header value LINE RFC compatible.
Make old netscape cookies a bit more RFC 2109 compatible by quoting
the \"expires\" value."
  (let ((start 0))
    (while (string-match "expires[ \t]*=[ \t]*\\([^\";]+?\\);" line start)
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

(defun http-cookies-parse-cookie (str)
  (let (name value)
    (if (string-match "^[ \t]*\\(.*?\\)[ \t]*=[ \t]*\"?\\(.*?\\)\"?[ \t]*;" str)
        (progn
          (setq name (match-string 1 str))
          (setq value (match-string 2 str))
          (message "Cookie name: %s" name)    ; debug
          (message "Cookie value: %s" value)) ; debug
      (message "Cannot parse cookie %s" str))))

(defun http-cookies-set (host headers)
  ;; The server may send several "Set-Cookie:" headers. So we have to iterate
  ;; over the whole list.
  (let (cookies)
    (dolist (line headers)
      ;; the headers names are downcase
      (when (equal (car line) "set-cookie")
        (let ((header-value (http-cookies-ns-to-rfc (cdr line))))
        (message header-value)
        ;; there may be several cookies separated by ","
        (dolist (cookie (http-cookies-split-string header-value ?\,))
          (http-cookies-parse-cookie cookie)))))))

(provide 'http-cookies)

;;; http-cookies.el ends here