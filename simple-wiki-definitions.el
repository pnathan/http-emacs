;;; simple-wiki-definitions.el --- common definitions for simple-wiki 

;; Copyright (C) 2003 D. Goel, Pierre Gaston
;; Emacs Lisp Archive entry
;; Filename: simple-wiki-definitions.el
;; Package: simple-wiki
;; Maintainer: Pierre Gaston <pierre@gaston-karlaouzou.com>

;; Keywords:
;; Version: 1.0.4
 
;; This file is NOT (yet) part of GNU Emacs.
 
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



(defcustom swd-wiki-defs-list '( 
	( "ew" 
	  "http://www.emacswiki.org/cgi-bin/wiki.pl"
	  "?action=browse&raw=2&id="
	  "?action=index&raw=1"
	  "?action=rc&raw=1"
	  1.1
	  swd-usemod-wiki-save
	  utf-8)
        ( "om" 
	  "http://www.emacswiki.org/cgi-bin/oddmuse.pl"
	  "?action=browse&raw=2&id="
	  "?action=index&raw=1"
	  "?action=rc&raw=1"
	  1.1
	  swd-usemod-wiki-save
	  utf-8)
	 ( "octave" 
	  "http://gnufans.net/octave.pl"
	  "?action=browse&raw=2&id="
	  "?action=index&raw=1"
	  "?action=rc&raw=1"
	  1.0
	  swd-usemod-wiki-save
	  iso-8859-1)
	 ( "fsedu" 
	  "http://gnufans.net/fsedu.pl"
	  "?action=browse&raw=2&id="
	  "?action=index&raw=1"
	  "?action=rc&raw=1"
	  1.0
	  swd-usemod-wiki-save
	  iso-8859-1)
	  ( "pierre" 
	  "http://pierre.gaston-karlaouzou.com/cgi-bin/en-pierre.pl"
	  "?action=browse&raw=2&id="
	  "?action=index&raw=1"
	  "?action=rc&raw=1"
	  1.1
	  swd-usemod-wiki-save
	  iso-8859-1)
 )
"\
defines the wiki you visit
the first element is the nickname.
the second is the base url
the third is possible url parameters to put before the page name
the fourth is  the possible parameters to view recentchanges
the fifth is the possible parameters to view recentchanges
the sixth is the version of the http-protocol to use 
the seventh is the save function to use for this wiki 
the eight th encoding
")


(defcustom swd-user-name nil
  "Set this to override your system username")

;;save functions
(defun swd-usemod-wiki-save ()
  "Save the current page to a UseMod wiki."
  (let ( 
	(url simple-wiki-url)
	(save-func simple-wiki-save-function))
    (switch-to-buffer
     (process-buffer
      (http-post
       (simple-wiki-save-link)
       (list (cons "title" (simple-wiki-page))
	     (cons "summary" 
		   (setq swc-summary-default
			 (read-from-minibuffer "Summary: " "*")))
	     '("raw" . "2")
	      (cons "username" 
		   (or swd-user-name
		       (apply 'concat (split-string user-full-name))))
	     (cons "text" (buffer-string))
	     (cons "recent_edit" (simple-wiki-minor-value)))
       (swd-http-coding (swd-nick simple-wiki-url))
      (swd-http-version (swd-nick simple-wiki-url)))))
    (simple-wiki-edit-mode)
    (set (make-local-variable 'simple-wiki-url) url)
    (set (make-local-variable 'simple-wiki-save-function) save-func)))


;;various utility function
(defun swd-nick (url) 
  (let  
      ((url-base (if (string-match "\\([^?]+\\)" url) (match-string 1 url)))
	 (wiki-defs-list swd-wiki-defs-list)
	 (nick nil)
	 )
    (if url-base
	(while (or wiki-defs-list (not nick))
	  (if (equal (cadar wiki-defs-list) url-base)
	      (setq nick (caar wiki-defs-list)))
	  (setq wiki-defs-list (cdr wiki-defs-list)))
      )
    nick
    ))


(defun swd-base-url (nick)  
  (second (assoc nick swd-wiki-defs-list))
  )	

(defun swd-additional-parameters (nick)
  (third (assoc nick swd-wiki-defs-list))
  )

(defun swd-index-parameters (nick)
  (fourth  (assoc nick swd-wiki-defs-list))
  )

(defun swd-rc-ptarameters (nick)
  (fifth (assoc nick swd-wiki-defs-list))
  )

(defun swd-http-version (nick)
  (sixth  (assoc nick swd-wiki-defs-list))
  )

(defun swd-http-coding (nick)
  (eighth  (assoc nick swd-wiki-defs-list))
  )

(defun swd-save-func (nick)
  (seventh  (assoc nick swd-wiki-defs-list))
  )

(provide 'simple-wiki-definitions)

;;; simple-wiki-definitions.el ends here
