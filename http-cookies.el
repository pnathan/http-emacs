;;; http-cookies.el --- simple HTTP cookies

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

(defconst http-token-regexp "[^][()<>@,;:\\\\\"/?={} \t\xf0-\xff\x00-\x1f]"
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

(defun http-cookies-parse-line (line)
  (let ((name-re (concat "\\(" http-token-regexp "+\\)[ \t]*=[ \t]*"))
        name value comment domain path secure version expires pos)
    ;; get the name
    (message name-re)
    (if (string-match name-re line)
        (progn
          (setq pos (match-end 0))
          (setq name (match-string 1 line))
          (message "DEBUG cookie name: %s" name))
      (message "Error parsing cookie %s." line))))


(defun http-cookies-set (host headers)
  ;; The server may send several "Set-Cookie:" headers. So we have to iterate
  ;; over the whole list.
  (let (cookies)
    (dolist (line headers)
      ;; the headers names are downcase
      (when (equal (car line) "set-cookie")
        (http-cookies-parse-line (cdr line))
        ))))

(provide 'http-cookies)

;;; http-cookies.el ends here