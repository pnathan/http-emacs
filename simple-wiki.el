;;; simple-wiki.el --- edit local raw wiki pages

;; Copyright (C) 2002, 2003  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;;         David Hansen <david.hansen@physik.fu-berlin.de>
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>
;; Version: 1.0.6
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

;;; Change Log:

;; 1.0.6
;;   - interface to define different major modes for different wikis
;;   - insert emphasized text and tags functions
;; 1.0.5
;;   - Added some xemacs compatibility.
;; 1.0.4
;;   - Added face for local links
;;   - highlight of free links
;; 1.0.3
;;   - Changed `simple-wiki-link-pattern'.  Non ASCII chars should work now.
;; 1.0.2
;;   - Added a lot of font locking.
;; 1.0.1
;;   - Added a variable to set the WikiName Regexp.

;;; Code:

(require 'font-lock)

(defvar simple-wiki-version "1.0.6")



;; the default value are for the emacswiki

(defvar simple-wiki-tag-list
  '(("u" . t) ("b" . t) ("i" . t) ("strong" . t) ("em" . t)
    ("nowiki" . t) ("code" . t) ("tt" . t) ("pre". t))
  "List of supported tags.
xemacs requires an alist for `compleiting-read'  the cdr is not used at the
time.")
(make-variable-buffer-local 'simple-wiki-tag-list)

(if (featurep 'xemacs) ;; xemacs doesn't know character classes
    (defvar simple-wiki-link-pattern
      ;; c&p from oddmuse sources, weird doesn't work perfectly so use
      ;; a different for gnu emacs
      '("\\<\\([A-Z]+[a-z\x80-\xff]+[A-Z][A-Za-z\x80-\xff]*\\)" . 0)
      "The pattern matching camel case links.
A Pair of the pattern and the matching subexpression.")
  (defvar simple-wiki-link-pattern
    '("\\<\\([A-Z]+?[[:lower:][:nonascii:]]+?[A-Z][[:lower:][:upper:]]*\\)" . 0)
    "The pattern matching camel case links.
A Pair of the pattern and the matching subexpression."))
(make-variable-buffer-local 'simple-wiki-link-pattern)

(defvar simple-wiki-free-link-pattern
  '("\\[\\[\\([^\n]+?\\)\\]\\]" . 1)
  "The Pattern matching free links.
A Pair of the pattern and the matching subexpression.")
(make-variable-buffer-local 'simple-wiki-free-link-pattern)

(defvar simple-wiki-em-regexps
  '(("\\(\\W\\|^\\)''\\([^']\\|[^']'\\)*''" . 0)        ; ''emph''
    ("\\(\\W\\|^\\)'''\\([^']\\|[^']'\\)*'''" . 0)      ; '''strong'''
    ("\\(\\W\\|^\\)'''''\\([^']\\|[^']'\\)*'''''" . 0)) ; '''''strong emph'''''
  "List of regexps to match emphasized, strong and strong emphasized text.
Actually a list of pairs with the pattern and the number of the matching
subexpression.")
(make-variable-buffer-local 'simple-wiki-em-regexps)

(defvar simple-wiki-headline-regexps
  '(("^=\\([^\n=]+\\)=[^=]" . 1)
    ("^=\\{2\\}\\([^\n=]+\\)=\\{2\\}\\([^=]\\|$\\)" . 1)
    ("^=\\{3\\}\\([^\n=]+\\)=\\{3\\}\\([^=]\\|$\\)" . 1)
    ("^=\\{4\\}\\([^\n=]+\\)=\\{4\\}\\([^=]\\|$\\)" . 1)
    ("^=\\{5\\}\\([^\n=]+\\)=\\{5\\}\\([^=]\\|$\\)" . 1)
    ("^=\\{6\\}\\([^\n=]+\\)=\\{6\\}\\([^=]\\|$\\)" . 1))
  "List of regexps to match headlines.
Actually a list of pairs with the pattern and the number of the matching
subexpression.")
(make-variable-buffer-local 'simple-wiki-headline-regexps)

(defvar simple-wiki-smilies-pattern
  (cons (concat
         "[ \t]\\("
         ":-?D\\|:-?)\\|;-?\)\\|:-?]\\|8-\)\\|"
         ":-\\\\|\\|:-?[/\\\\]\\|:-?(\\|:-?{\\)"
         "\\W") 1)
  "Pair of the pattern used to match smilies an the matching subexpression.")
(make-variable-buffer-local 'simple-wiki-smilies-pattern)

(defvar simple-wiki-em-strings
  '("''" . "''")
  "Start and end string for emphasis text.")
(make-variable-buffer-local 'simple-wiki-em-strings)

(defvar simple-wiki-strong-strings
  '("'''" . "'''")
  "Start and end strings for strong text.")
(make-variable-buffer-local 'simple-wiki-strong-strings)

(defvar simple-wiki-strong-em-strings
  '("'''''" . "'''''")
  "Start and end string for strong text.")
(make-variable-buffer-local 'simple-wiki-strong-em-strings)

(defvar simple-wiki-additional-keywords
  (list
   ;; time stamp at the beginning of the buffer
   '("\\`\\([0-9]+\\)[ \t]+\\(#.+?\\)\n"
     (1 font-lock-constant-face)
     (2 font-lock-warning-face))

   '("<\\(/?[a-z]+\\)" (1 font-lock-function-name-face t)) ; tags
   '("^[*#]\\([*#]+\\)" (0 font-lock-constant-face t))     ; enums
   '("^\\([*#]\\)[^*#]" (1 font-lock-builtin-face t))      ; enums
   '("^\\:\\(\\:+\\)" (0 font-lock-comment-face))          ; indent
   '("^\\(\\:\\)[^\\:]" (1 font-lock-string-face))         ; indent

   '(simple-wiki-match-tag-i . (0 'simple-wiki-italic-face prepend))
   '(simple-wiki-match-tag-b . (0 'simple-wiki-bold-face prepend))
   '(simple-wiki-match-tag-u . (0 'simple-wiki-underline-face prepend))
   '(simple-wiki-match-tag-tt . (0 'simple-wiki-teletype-face prepend))
   '(simple-wiki-match-tag-em . (0 'simple-wiki-emph-face prepend))
   '(simple-wiki-match-tag-strong . (0 'simple-wiki-strong-face prepend))

   ;; code blocks
   ;; highlight of <code> and <pre> needs some more sophisticated stuff :(
   '(simple-wiki-match-tag-code . (0 'simple-wiki-code-face t))
   '(simple-wiki-match-tag-pre . (0 'simple-wiki-code-face t))

   '(simple-wiki-match-code-block . (0 'simple-wiki-code-face t))

   ;; should be the last
   '(simple-wiki-match-tag-nowiki . (0 'simple-wiki-nowiki-face t)))

  "Additional keywords for font locking.")
(make-variable-buffer-local 'simple-wiki-additional-keywords)

(defvar simple-wiki-font-lock-keywords nil
  "Font lock keywords for simple wiki mode.")
(make-variable-buffer-local 'simple-wiki-font-lock-keywords)



;; custom groups

(defgroup simple-wiki ()
  "Edit raw wiki pages.")

(defgroup simple-wiki-faces ()
  "Faces simple-wiki-mode." :group 'simple-wiki)



;; faces

;; xemacs doesn't know about :inherit.  Just set all heading to bold.
(if (featurep 'xemacs)
    (progn
      (defface simple-wiki-heading-1-face
        '((t (:bold t)))
        "Face for WiKi headings at level 1."
        :group 'simple-wiki-faces)

      (defface simple-wiki-heading-2-face
        '((t (:bold t)))
        "Face for WiKi headings at level 2."
        :group 'simple-wiki-faces)

      (defface simple-wiki-heading-3-face
        '((t (:bold t)))
        "Face for WiKi headings at level 3."
        :group 'simple-wiki-faces)

      (defface simple-wiki-heading-4-face
        '((t (:bold t)))
        "Face for WiKi headings at level 4."
        :group 'simple-wiki-faces)

      (defface simple-wiki-heading-5-face
        '((t (:bold t)))
        "Face for WiKi headings at level 5."
        :group 'simple-wiki-faces)

      (defface simple-wiki-heading-6-face
        '((t (:bold t)))
        "Face for WiKi headings at level 6."
        :group 'simple-wiki-faces))

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
    :group 'simple-wiki-faces))

(if (featurep 'xemacs)
    (defface simple-wiki-emph-face
      '((t (:italic t)))
      "Face for ''emphasis''."
      :group 'simple-wiki-faces)
  (defface simple-wiki-emph-face
    '((t (:slant italic)))
    "Face for ''emphasis''."
    :group 'simple-wiki-faces))

(if (featurep 'xemacs)
    (defface simple-wiki-strong-face
      '((t (:bold t)))
      "Face for '''strong emphasis'''."
      :group 'simple-wiki-faces)
  (defface simple-wiki-strong-face
    '((t (:weight bold)))
    "Face for '''strong emphasis'''."
    :group 'simple-wiki-faces))

(if (featurep 'xemacs)
    (defface simple-wiki-strong-emph-face
      '((t (:bold t :italic t)))
      "Face for '''''stronger emphasis'''''."
      :group 'simple-wiki-faces)
  (defface simple-wiki-strong-emph-face
    '((t (:weight bold :slant italic)))
    "Face for '''''stronger emphasis'''''."
    :group 'simple-wiki-faces))

(if (featurep 'xemacs)
    (defface simple-wiki-italic-face
      '((t (:italic t)))
      "Face for <i>italic</i>."
      :group 'simple-wiki-faces)
  (defface simple-wiki-italic-face
    '((t (:slant italic)))
    "Face for <i>italic</i>."
    :group 'simple-wiki-faces))

(if (featurep 'xemacs)
    (defface simple-wiki-bold-face
      '((t (:bold t)))
      "Face for <b>bold</b>."
      :group 'simple-wiki-faces)
  (defface simple-wiki-bold-face
    '((t (:weight bold)))
    "Face for <b>bold</b>."
    :group 'simple-wiki-faces))

(defface simple-wiki-underline-face
  '((t (:underline t)))
  "Face for <u>underline</u>."
  :group 'simple-wiki-faces)

(if (featurep 'xemacs)
    (defface simple-wiki-local-link-face
      '((((class color) (background dark))
         (:foreground "skyblue3" :bold t))
        (((class color) (background light))
         (:foreground "royal blue" :bold t)))
      "Face for links to pages on the same wiki."
      :group 'simple-wiki-faces)
  (defface simple-wiki-local-link-face
    '((((class color) (background dark))
       (:foreground "skyblue3" :weight bold))
      (((class color) (background light))
       (:foreground "royal blue" :weight bold)))
    "Face for links to pages on the same wiki."
    :group 'simple-wiki-faces))

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

(defface simple-wiki-nowiki-face
  '((((class color) (background dark))
     (:foreground "LightGoldenRod2"))
    (((class color) (background light))
     (:foreground "DarkGoldenRod2")))
  "Face for links to pages on the same wiki."
  :group 'simple-wiki-faces)

(if (featurep 'xemacs)
    (defface simple-wiki-smiley-face
      '((((class color) (background dark))
         (:foreground "gold" :bold t))
        (((class color) (background light))
         (:foreground "goldenrod" :bold t)))
      "Face for links to pages on the same wiki."
      :group 'simple-wiki-faces)
  (defface simple-wiki-smiley-face
    '((((class color) (background dark))
       (:foreground "gold" :weight bold))
      (((class color) (background light))
       (:foreground "goldenrod" :weight bold)))
    "Face for links to pages on the same wiki."
    :group 'simple-wiki-faces))



;; font lock matcher

(defun simple-wiki-match-tag (tag limit)
  "Font lock matcher for regions within <TAG></TAG>."
  (when (search-forward (concat "<" tag ">") limit t)
    (let ((beg (match-end 0)) end)
      (if (search-forward (concat "</" tag ">") limit t)
          (setq end (match-beginning 0))
        (setq end (point)))
      (store-match-data (list beg end))
      t)))

(defun simple-wiki-match-tag-i (limit)
  "Font lock matcher for regions within <i></i>."
  (simple-wiki-match-tag "i" limit))

(defun simple-wiki-match-tag-b (limit)
  "Font lock matcher for regions within <b></b>."
  (simple-wiki-match-tag "b" limit))

(defun simple-wiki-match-tag-u (limit)
  "Font lock matcher for regions within <u></u>."
  (simple-wiki-match-tag "u" limit))

(defun simple-wiki-match-tag-tt (limit)
  "Font lock matcher for regions within <tt></tt>."
  (simple-wiki-match-tag "tt" limit))

(defun simple-wiki-match-tag-nowiki (limit)
  "Font lock mather for regions within <nowiki></nowiki>."
  (simple-wiki-match-tag "nowiki" limit))

(defun simple-wiki-match-tag-code (limit)
  "Font lock matcher for regions within <code></code>."
  (simple-wiki-match-tag "code" limit))

(defun simple-wiki-match-tag-pre (limit)
  "Font lock matcher for regions within <pre></pre>."
  (simple-wiki-match-tag "pre" limit))

(defun simple-wiki-match-tag-em (limit)
  "Font lock matcher for regions within <em></em>."
  (simple-wiki-match-tag "em" limit))

(defun simple-wiki-match-tag-strong (limit)
  "Font lock matcher for regions within <strong></strong>."
  (simple-wiki-match-tag "strong" limit))

(defun simple-wiki-end-of-code-block ()
  "Return the end of a code block if the cursor is within a code block.
Return nil otherwise."
  ;; FIXME: we assume that the line before code is empty.
  ;; this is not necessary in all cases.  known issues:
  ;;        (a) code starts directly after a heading.
  (save-excursion
    (backward-paragraph)
    (when (string-match "^$" (buffer-substring (point-at-bol) (point-at-eol)))
      (forward-line 1))
    (let ((char (char-after (point))))
      (when (and char (or (= char ?\t) (= char ? )))
        (forward-paragraph)
        (point)))))

(defun simple-wiki-match-code-block (limit)
  (let (beg end)
    (when (re-search-forward "^[ \t]+[^ \t\n]" limit t)
      (setq beg (match-beginning 0))
      (setq end (simple-wiki-end-of-code-block))
      (when end
        (if  (<= end beg)
            nil
          (store-match-data (list beg end))
          t)))))



;; editing functions

(defun simple-wiki-insert-emph ()
  "Insert emphasized text."
  (interactive)
  (if (equal simple-wiki-em-strings 'none)
      (error "No emphasis strings defined.")
    (insert (car simple-wiki-em-strings))
    (save-excursion
      (insert (cdr simple-wiki-em-strings)))))

(defun simple-wiki-insert-strong ()
  "Insert strong text."
  (interactive)
  (if (equal simple-wiki-strong-strings 'none)
      (error "No string strings defined.")
    (insert (car simple-wiki-strong-strings))
    (save-excursion (insert (cdr simple-wiki-strong-strings)))))

(defun simple-wiki-insert-strong-emph ()
  "Insert strong emphasized text."
  (interactive)
  (if (equal simple-wiki-strong-em-strings 'none)
      (error "No string strings defined.")
    (insert (car simple-wiki-strong-em-strings))
    (save-excursion (insert (cdr simple-wiki-strong-em-strings)))))

(defun simple-wiki-insert-tag-string (tag &optional closing)
  "Insert a the string \"<TAG>\" or \"</TAG>\" if CLOSING is non-nil."
  (if closing (insert "</") (insert "<"))
  (insert tag)
  (insert ">"))

(defun simple-wiki-insert-tag (&optional tag)
  (interactive)
  "Insert a tag and put the cursor between the opening and closing tag."
  (unless tag
    (setq tag (completing-read "Tag: " simple-wiki-tag-list))
    (add-to-list 'simple-wiki-tag-list (cons tag t)))
  (simple-wiki-insert-tag-string tag)
  (save-excursion (simple-wiki-insert-tag-string tag t)))



;; mode definitions

(defun simple-wiki-add-keyword (match-pair face overwrite)
  "Add an element to `simple-wiki-font-lock-keywords'.
MATCH-PAIR has to be a pair with a regular expression and a
number for the subexpression: (REGEXP . NUMBER).  FACE is the
face used for highlighting and overwrite may be 'prepend,
'append, 'keep, t or nil.  See `font-lock-keywords'."
  (add-to-list
   'simple-wiki-font-lock-keywords
   (cons (car match-pair) (list (cdr match-pair) `(quote ,face) overwrite))))

(defun simple-wiki-add-font-lock-keywords ()
  "Add the default patterns to `simple-wiki-font-lock-keywords'."
  ;; additional keywords
  (if (equal simple-wiki-additional-keywords 'none)
      (setq simple-wiki-font-lock-keywords nil)
    (setq simple-wiki-font-lock-keywords simple-wiki-additional-keywords))
  ;; local links
  (unless (equal simple-wiki-link-pattern 'none)
    (simple-wiki-add-keyword simple-wiki-link-pattern
                             'simple-wiki-local-link-face
                             'prepend))
  (unless (equal simple-wiki-free-link-pattern 'none)
    (simple-wiki-add-keyword simple-wiki-free-link-pattern
                             'simple-wiki-local-link-face
                             'prepend))
  ;; smilies
  (unless (equal simple-wiki-smilies-pattern 'none)
    (simple-wiki-add-keyword simple-wiki-smilies-pattern
                             'simple-wiki-smiley-face
                             'prepend))
  ;; emphasis
  (let (em-re)
    (unless (equal simple-wiki-em-regexps 'none)
      (when (setq em-re (first simple-wiki-em-regexps))
        (simple-wiki-add-keyword em-re 'simple-wiki-emph-face 'prepend))
      (when (setq em-re (second simple-wiki-em-regexps))
        (simple-wiki-add-keyword em-re 'simple-wiki-strong-face 'prepend))
      (when (setq em-re (third simple-wiki-em-regexps))
        (simple-wiki-add-keyword em-re
                                 'simple-wiki-strong-emph-face
                                 'prepend))))
  ;; head lines
  (let (head-re)
    (unless (equal simple-wiki-headline-regexps 'none)
      (when (setq head-re (first simple-wiki-headline-regexps))
        (simple-wiki-add-keyword head-re 'simple-wiki-heading-1-face t))
      (when (setq head-re (second simple-wiki-headline-regexps))
        (simple-wiki-add-keyword head-re 'simple-wiki-heading-2-face t))
      (when (setq head-re (third simple-wiki-headline-regexps))
        (simple-wiki-add-keyword head-re 'simple-wiki-heading-3-face t))
      (when (setq head-re (fourth simple-wiki-headline-regexps))
        (simple-wiki-add-keyword head-re 'simple-wiki-heading-4-face t))
      (when (setq head-re (fifth simple-wiki-headline-regexps))
        (simple-wiki-add-keyword head-re 'simple-wiki-heading-5-face t))
      (when (setq head-re (sixth simple-wiki-headline-regexps))
        (simple-wiki-add-keyword head-re 'simple-wiki-heading-6-face t)))))

(defun simple-wiki-define-major-mode (mode name doc-string &rest properties)
  "Define a major mode for editing a wiki page.
MODE has to be a symbol which is used to build the major mode command:
e.g. 'emacswiki results in the command `simple-emacswiki-mode'. NAME
is a string which will appear in the status line (e.g. \"EmacsWiki\").
DOC-STRING is an an optional documentation string.  See
`definde-derived-mode'

To overwrite the default syntax (that should be fine for emacswiki or
any default oddmuse installation) you can specify various properties
as a list of keywords:

        :tags............... overwrite `simple-wiki-tag-list'
        :camelcase.......... overwrite `simple-wiki-link-pattern'
        :free-link.......... overwrite `simple-wiki-free-link-pattern'
        :smilies............ overwrite `simple-wiki-smilies-pattern'
        :em-strings......... overwrite `simple-wiki-em-strings'
        :strong-strings..... overwrite `simple-wiki-strong-strings'
        :strong-em-strings.. overwrite `simple-wiki-strong-em-strings'
        :em-regexps......... overwrite `simple-wiki-em-regexps'
        :headlines.......... overwrite `simple-wiki-headline-regexps'
        :keywords........... overwrite `simple-wiki-additional-keywords'

Use the symbol 'none as the value if the wiki doesn't support the property."
  (eval
   `(define-derived-mode
      ,(intern (concat "simple-" (symbol-name mode) "-mode"))
      text-mode ,name ,doc-string

      (when (quote ,(plist-get properties :tags))
        (setq simple-wiki-tag-list
              (quote ,(plist-get properties :tags))))
      (when (quote ,(plist-get properties :camelcase))
        (setq simple-wiki-link-pattern
              (quote ,(plist-get properties :camelcase))))
      (when (quote ,(plist-get properties :free-link))
        (setq simple-wiki-free-link-pattern
              (quote ,(plist-get properties :free-link))))
      (when (quote ,(plist-get properties :smilies))
        (setq simple-wiki-smilies-pattern
              (quote ,(plist-get properties :smilies))))
      (when (quote ,(plist-get properties :em-strings))
        (setq simple-wiki-em-strings
              (quote ,(plist-get properties :em-strings))))
      (when (quote ,(plist-get properties :strong-strings))
        (setq simple-wiki-strong-strings
              (quote ,(plist-get properties :strong-strings))))
      (when (quote ,(plist-get properties :strong-em-strings))
        (setq simple-wiki-strong-em-strings
              (quote ,(plist-get properties :strong-em-strings))))
      (when (quote ,(plist-get properties :em-regexps))
        (setq simple-wiki-em-regexps
              (quote ,(plist-get properties :em-regexps))))
      (when (quote ,(plist-get properties :headlines))
        (setq simple-wiki-headline-regexps
              (quote ,(plist-get properties :headlines))))
      (when (quote ,(plist-get properties :keywords))
        (setq simple-wiki-additional-keywords
              (quote ,(plist-get properties :keywords))))

      (define-key ,(intern (concat "simple-" (symbol-name mode) "-mode-map"))
        "\C-c\C-e" 'simple-wiki-insert-emph)
      (define-key ,(intern (concat "simple-" (symbol-name mode) "-mode-map"))
        "\C-c\C-s" 'simple-wiki-insert-strong)
      (define-key ,(intern (concat "simple-" (symbol-name mode) "-mode-map"))
        "\C-c\C-t" 'simple-wiki-insert-tag)

      (make-local-variable 'font-lock-defaults)
      (setq font-lock-multiline t)
      (simple-wiki-add-font-lock-keywords)
      (setq font-lock-defaults  '(simple-wiki-font-lock-keywords t))
      (goto-address)
      (font-lock-mode 1)
      (setq indent-tabs-mode nil))))



;; mode definitions

(simple-wiki-define-major-mode
 'wiki
 "Wiki"
 "Simple mode to edit wiki pages.
\\{simple-wiki-mode-map}")

(simple-wiki-define-major-mode
 'emacswiki
 "EmacsWiki"
  "Simple mode to edit wiki pages at http://www.emacswiki.org/.
\\{simple-emacswiki-mode-map}")

(simple-wiki-define-major-mode
 'oddmuse
 "OddMuse"
 "Simple mode to edit wiki pages at http://www.oddmuse.org/.
\\{simple-oddmuse-mode-map}"
 :camelcase 'none)

(provide 'simple-wiki)

;;; simple-wiki.el ends here
