;;; simple-wiki.el --- edit local raw wiki pages

;; Copyright (C) 2002, 2003  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Pierre Gaston <pierre@gaston-karlaouzou.com>
;; Version: 1.0.4
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

;;; ChangeLog:

;; 1.0.4
;;   - Added face for local links
;;   - highlight of free links
;; 1.0.3
;;   - Changed `simple-wiki-link-pattern'.  Non ASCII chars should work now.
;; 1.0.2
;;   - Added a lot of font locking.
;; 1.0.1
;;   - Added a variable to set the WikiName Regex.

;;; Code:

;; don't remove this pattern.  Might be useful to match camel case
;; in pre emacs 21 or xemacs.
;; "\\<[A-Z\xc0-\xde]+[a-z\xdf-\xff]+\\([A-Z\xc0-\xde]+[a-z\xdf-\xff]*\\)+\\>"

(defconst simple-wiki-link-pattern
  "\\<\\([A-Z][[:lower:][:upper:]]+?[A-Z][[:lower:]]+[[:lower:][:upper:]]*\\)"
  "The pattern used for finding camel case links.")

(defconst simple-wiki-free-link-pattern
  "\\[\\[\\([^\n]+\\)\\]\\]"
  "The Pattern used for finding free links.")

;; custom groups
(defgroup simple-wiki ()
  "Edit raw wiki pages.")

(defgroup mldonkey-faces ()
  "Faces simple-wiki-mode." :group 'simple-wiki)

;; faces
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

(defface simple-wiki-emph-face
  '((t (:slant italic)))
  "Face for ''emphasis''"
  :group 'simple-wiki-faces)

(defface simple-wiki-strong-face
  '((t (:weight bold)))
  "Face for '''strong emphasis'''"
  :group 'simple-wiki-faces)

(defface simple-wiki-italic-face
  '((t (:slant italic)))
  "Face for <i>italic</i>"
  :group 'simple-wiki-faces)

(defface simple-wiki-bold-face
  '((t (:weight bold)))
  "Face for <b>bold</b>"
  :group 'simple-wiki-faces)

(defface simple-wiki-underline-face
  '((t (:underline t)))
  "Face for <u>underline</u>"
  :group 'simple-wiki-faces)

(defface simple-wiki-local-link-face
  '((((class color) (background dark))
     (:foreground "light sky blue" :weight bold))
    (((class color) (background light))
     (:foreground "royal blue" :weigth bold)))
  "Face for links to pages on the same wiki."
  :group 'simple-wiki-faces)

(defface simple-wiki-teletype-face
  '((((class color) (background dark)) (:background "grey15"))
    (((class color) (background light)) (:background "moccasin")))
  "Face for <tt>teletype</tt>."
  :group 'simple-wiki-faces)

(defface simple-wiki-code-face
  '((((class color) (background dark)) (:background "grey15"))
    (((class color) (background light)) (:background "moccasin")))
  "Face for code in Wiki pages."
  :group 'simple-wiki-faces)

(defconst simple-wiki-font-lock-keywords
  (list
   ;; time stamp at the beginning of the buffer
   '("\\`\\([0-9]+\\)[ \t]+\\(#.+?\\)\n"
     (1 font-lock-constant-face)
     (2 font-lock-warning-face))

   ;; headings, actually multi lines are possible but...
   '("^=\\([^\n=]+\\)=[^=]"
     (1 'simple-wiki-heading-1-face))
   '("^=\\{2\\}\\([^\n=]+\\)=\\{2\\}[^=]"
    (1 'simple-wiki-heading-2-face t))
   '("^=\\{3\\}\\([^\n=]+\\)=\\{3\\}[^=]"
     (1 'simple-wiki-heading-3-face t))
   '("^=\\{4\\}\\([^\n=]+\\)=\\{4\\}[^=]"
     (1 'simple-wiki-heading-4-face t))
   '("^=\\{5\\}\\([^\n=]+\\)=\\{5\\}[^=]"
     (1 'simple-wiki-heading-5-face t))
   '("^=\\{6\\}\\([^\n=]+\\)=\\{6\\}[^=]"
     (1 'simple-wiki-heading-6-face t))

   '("<\\(/?[a-z]+\\)" (1 font-lock-function-name-face t)) ; tags
   '("^[*#]\\([*#]+\\)" (0 font-lock-constant-face t))     ; enums
   '("^\\([*#]\\)[^*#]" (1 font-lock-builtin-face t))      ; enums

   ;; FIXME: emphasis and tags may be multi line but seems to work well this way

   ;; other tags
   '(simple-wiki-match-italic . (0 'simple-wiki-italic-face prepend))
   '(simple-wiki-match-bold . (0 'simple-wiki-bold-face prepend))
   '(simple-wiki-match-underline . (0 'simple-wiki-underline-face prepend))
   '(simple-wiki-match-teletype . '(0 'simple-wiki-teletype-face prepend))
   '(simple-wiki-match-pre . (0 'simple-wiki-code-face t))
   '(simple-wiki-match-code-tag . (0 'simple-wiki-code-face t))

   ;; emphasis
   '(simple-wiki-match-emph . 'simple-wiki-emph-face)
   '(simple-wiki-match-strong . 'simple-wiki-strong-face)

   ;; local links
   (cons simple-wiki-free-link-pattern '(1 'simple-wiki-local-link-face))
   (cons simple-wiki-link-pattern ''simple-wiki-local-link-face)

   ;; code blocks
   '("^[\t ]" (simple-wiki-match-code (simple-wiki-check-in-code-block) nil
                                      (0 'simple-wiki-code-face t)))))

(define-derived-mode simple-wiki-mode text-mode "Wiki"
  "Simple mode to edit wiki pages.

\\{simple-wiki-mode-map}"
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults  '(simple-wiki-font-lock-keywords t))

  (font-lock-mode 1)
  (goto-address)
  (set (make-local-variable 'sgml-tag-alist)
       `(("b") ("code") ("em") ("i") ("strong") ("nowiki")
	 ("pre" \n) ("tt") ("u")))
  (set (make-local-variable 'skeleton-transformation) 'identity)
  (setq indent-tabs-mode nil))


(defvar simple-wiki-in-code-block nil)


(defun simple-wiki-match-taged (limit tag)
  (when (search-forward (concat "<" tag ">") limit t)
    (let ((beg (match-end 0)) end)
      (if (search-forward (concat "</" tag ">") limit t)
          (setq end (match-beginning 0))
        (setq end (point)))
      (store-match-data (list beg end))
      t)))

(defun simple-wiki-match-italic (limit)
  (simple-wiki-match-taged limit "i"))

(defun simple-wiki-match-bold (limit)
  (simple-wiki-match-taged limit "b"))

(defun simple-wiki-match-underline (limit)
  (simple-wiki-match-taged limit "u"))

(defun simple-wiki-match-teletype (limit)
  (simple-wiki-match-taged limit "tt"))

(defun simple-wiki-match-pre (limit)
  (simple-wiki-match-taged limit "pre"))

(defun simple-wiki-match-code-tag (limit)
  (simple-wiki-match-taged limit "code"))

(defun simple-wiki-match-emph-classic (limit)
  (when (re-search-forward
         "[^']\\(''\\)[^']" limit t)
    (let ((beg (match-end 1)) end)
      (if (re-search-forward "''+" limit t)
          (setq end (match-beginning 0))
        (setq end (point)))
      (store-match-data (list beg end))
      t)))

(defun simple-wiki-match-strong-classic (limit)
  (when (re-search-forward "\\('''\\)[^']" limit t)
    (let ((beg (match-end 1)) end)
      (if (re-search-forward "'''+" limit t)
          (setq end (match-beginning 0))
        (setq end (point)))
      (store-match-data (list beg end))
      t)))

(defun simple-wiki-match-emph (limit)
  (or (simple-wiki-match-emph-classic limit)
      (simple-wiki-match-taged limit "em")))

(defun simple-wiki-match-strong (limit)
  (or (simple-wiki-match-strong-classic limit)
      (simple-wiki-match-taged limit "strong")))

(defun simple-wiki-check-in-code-block ()
  "Set the variable `simple-wiki-in-code-block'.
Set `simple-wiki-in-code-block' to non nil if the point is in a code block.
If we are in a code block return the point of the end of the block."
  ;; FIXME: we assume that the line before code is empty.
  ;; this is not necessary in all cases.  known issues:
  ;;        (a) code starts directly after a heading.
  (setq simple-wiki-in-code-block nil)
  (save-excursion
    ;; FIXME: this is problematic when the line before the paragraph
    ;; contains one or more whitespaces.
    (backward-paragraph)
    (forward-line 1)
    (let ((char (char-after (point))))
      (when (and char (or (= char ?\t) (= char ? )))
        (setq simple-wiki-in-code-block t)
        (forward-paragraph)
        (point)))))

(defun simple-wiki-match-code (limit)
  (when simple-wiki-in-code-block
    (store-match-data (list (point-at-bol) limit))
    (goto-char limit)
    t))


(provide 'simple-wiki)

;;; simple-wiki.el ends here
