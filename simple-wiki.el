;;; simple-wiki.el --- edit local raw wiki pages

;; Copyright (C) 2002, 2003  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Pierre Gaston <pierre@gaston-karlaouzou.com>
;; Version: 1.0.1
;; Keywords: hypermedia
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?SimpleWikiEditMode

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

;; Use `simple-wiki-mode' to edit raw wiki pages.  This is useful for
;; temp files when editing textareas in w3m, for example.  Here is how
;; to do that:
;;
;; (add-to-list 'auto-mode-alist '("w3mtmp" . simple-wiki-mode))

;;History
;; 1.0.1
;;  -- add a variable to set the WikiName Regex

;;; Code:

(defconst simple-wiki-link-pattern
  "\\<[A-Z\xc0-\xde]+[a-z\xdf-\xff]+\\([A-Z\xc0-\xde]+[a-z\xdf-\xff]*\\)+\\>"
  "The pattern used for finding WikiName.")


(defgroup simple-wiki ()
  "Edit raw wiki pages.")

(defgroup mldonkey-faces ()
  "Faces simple-wiki-mode." :group 'simple-wiki)

(defface simple-wiki-heading-1-face
  '((((type tty pc) (class color)) (:foreground "yellow" :weight bold))
    (t (:height 1.2 :inherit simple-wiki-heading-2-face)))
  "Face for WiKi headings at level 1."
  :group 'simple-wiki-faces)

(defface simple-wiki-heading-2-face
  '((((type tty pc) (class color)) (:foreground "lightblue" :weight bold))
    (t (:height 1.2 :inherit simple-wiki-heading-3-face)))
  "Face for WiKi headings at level 2."
  :group 'simple-wiki-faces)

(defface simple-wiki-heading-3-face
  '((((type tty pc) (class color)) (:weight bold))
    (t (:height 1.2 :inherit simple-wiki-heading-4-face)))
  "Face for WiKi headings at level 3."
  :group 'simple-wiki-faces)

(defface simple-wiki-heading-4-face
  '((((type tty pc) (class color)) (:weight bold))
    (t (:weight bold :inherit variable-pitch)))
  "Face for WiKi headings at level 4."
  :group 'simple-wiki-faces)

(defface simple-wiki-heading-5-face
  '((((type tty pc) (class color)) (:weight bold))
    (t (:weight bold :inherit variable-pitch)))
  "Face for WiKi headings at level 5."
  :group 'simple-wiki-faces)

(defface simple-wiki-heading-6-face
  '((((type tty pc) (class color)) (:weight bold))
    (t (:weight bold :inherit variable-pitch)))
  "Face for WiKi headings at level 6."
  :group 'simple-wiki-faces)

(defface simple-wiki-code-face
  '((((class color) (background dark)) (:background "dark slate gray"))
    (((class color) (background light)) (:background "moccasin")))
  "Face for code in Wiki pages."
  :group 'simple-wiki-faces)

(defconst simple-wiki-font-lock-keywords
  '(("^\\([0-9]+\\)[ \t]+\\(#.+?\\)\n"
     (1 'font-lock-constant-face)
     (2 'font-lock-warning-face))       ; the time stamp and warning at the top

    ;; headings
    ("^=\\([^\n=]+\\)=[^=]"
     (1 'simple-wiki-heading-1-face))
    ("^=\\{2\\}\\([^\n=]+\\)=\\{2\\}[^=]"
     (1 'simple-wiki-heading-2-face))
    ("^=\\{3\\}\\([^\n=]+\\)=\\{3\\}[^=]"
     (1 'simple-wiki-heading-3-face))
    ("^=\\{4\\}\\([^\n=]+\\)=\\{4\\}[^=]"
     (1 'simple-wiki-heading-4-face))
    ("^=\\{5\\}\\([^\n=]+\\)=\\{5\\}[^=]"
     (1 'simple-wiki-heading-5-face))
    ("^=\\{6\\}\\([^\n=]+\\)=\\{6\\}[^=]"
     (1 'simple-wiki-heading-6-face))

    ("^[\t ].+?$" . 'simple-wiki-code-face)                ; code
    ("<\\(/?[a-z]+\\)" (1 font-lock-function-name-face))   ; tags
    ("^[*#]\\([*#]+\\)" . 'font-lock-constant-face)        ; enums
    ("^\\([*#]\\)[^*#]" 1 font-lock-builtin-face)))        ; enums


(define-derived-mode simple-wiki-mode text-mode "Wiki"
  "Simple mode to edit wiki pages.

\\{simple-wiki-mode-map}"
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults  '(simple-wiki-font-lock-keywords t))

  ;; FIXME: how to get this into `simple-wiki-font-lock-keywords'?
  (font-lock-add-keywords
   nil
   (list (cons (symbol-value 'simple-wiki-link-pattern)
               'font-lock-keyword-face)))

  (font-lock-mode 1)
  (goto-address)
  (set (make-local-variable 'sgml-tag-alist)
       `(("b") ("code") ("em") ("i") ("strong") ("nowiki")
	 ("pre" \n) ("tt") ("u")))
  (set (make-local-variable 'skeleton-transformation) 'identity)
  (setq indent-tabs-mode nil))


(provide 'simple-wiki)

;;; simple-wiki.el ends here
