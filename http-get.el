;;; http-get.el --- simple HTTP GET

;; Copyright (C) 2002, 2003 Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: D. Goel <deego@gnufans.org>
;; Modified by: D. Goel <deego@gnufans.org>
;; Version: 1.0.3
;; Keywords: hypermedia
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?HttpGet

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

;; Use `http-get' to download an URL.

;;History
;; 1.0.3
;;  -- move http-url-encode from http-post.el to http-get.el
;;  -- add a param to http-get to specify the encoding of the params in the url

;;; Code:

(require 'hexl)

(defvar http-get-version "1.0.3")


;;Proxy
(defvar http-proxy-host nil
  "*If nil dont use proxy, else name of proxy server.")

(defvar http-proxy-port nil
  "*Port number of proxy server.  Default is 80.")

;; Filtering

(defvar  http-filter-pre-insert-hook '(http-1-1-parser)
  "Hook run by the `http-filter'.
This is called whenever a chunk of input arrives, before it is
inserted into the buffer.  If you want to modify the string that gets
inserted, modify the variable `string' which is dynamically bound to
what will get inserted in the end.  The string will be inserted at
the process-mark, which you can get by calling \(process-mark proc).
`proc' is dynamically bound to the process, and the current buffer
is the very buffer where the string will be inserted. Note that if 
you use http/1.1 you probably want to leave  http-1-1-parser
as the first hook as it remove headers and unchunk the file you request"
)

(defvar http-filter-post-insert-hook  nil
  "Hook run by the `http-filter'.
This is called whenever a chunk of input arrives, after it has been
inserted, but before the process-mark has moved.  Therefore, the new
text lies between the process-mark and point.  You can get the value
s
of the process-mark by calling \(process-mark proc).  Please take care
to leave point at the right place, eg.  by wrapping your code in a
`save-excursion'.")

(defun http-filter (proc string)
  "Filter function for HTTP buffers.
See `http-filter-pre-insert-hook' and `http-filter-post-insert-hook'
for places where you can do your own stuff such as HTML rendering."
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	" Insert the text, advancing the process marker." 
	(goto-char (process-mark proc))
	(run-hooks 'http-filter-pre-insert-hook)
	(insert string)
	(run-hooks 'http-filter-post-insert-hook)
	(set-marker (process-mark proc) (point)))
	(if moving (goto-char (process-mark proc))
	))
  )
)





;; Dealing with 
(defvar http-status-code nil
  "The status code returned for the current buffer.
This is set by the function `http-headers'.")

(defvar http-reason-phrase nil
  "The reason phrase returned for the `http-status-code'.
This is set by the function `http-headers'.")

(defvar http-headers nil
  "An alist of the headers that have been parsed and removed from the buffer.
The headers are stored as an alist.
This is set by the function `http-headers'.")

(defvar  http-parser-state nil
  "parser status") 


(defvar http-unchunk-chunk-size  0
  "Size of the current unfinished chunk.")
(make-local-variable 'http-unchunk-size)
(defvar not-yet-parsed  ""
  "received bytes that have not yet been parsed")
(make-local-variable 'not-yet-parsed)			      

(defun http-1-1-parser ()
  "simple parser for http1.1 message. Parse the status line, headers and
chunk"
  (when http-parser-state
    ;; because simple-wiki-mode derived from text mode that kills all local variable
    ;;we must make http-parser-state local here
  (unless (local-variable-p http-parser-state )
    (set (make-local-variable 'http-parser-state) http-parser-state))
  (let ((parsed-string (concat not-yet-parsed string )))
    (setq string "")
    (setq not-yet-parsed "")
    (while (> (string-bytes parsed-string) 0)
      (cond 
       ((eq http-parser-state 'status-line)
	;;parsing status line
	(if (string-match "HTTP/[0-9.]+ \\([0-9]+\\) \\(.*\\)\r\n" parsed-string)
	    (progn 
	      (set (make-local-variable 'http-status-code)
		   (string-to-number (match-string 1 parsed-string)))
	      (set (make-local-variable 'http-reason-phrase)
		   (match-string 2 parsed-string))
	      (setq http-parser-state 'header)
	      (setq parsed-string (substring parsed-string (match-end 0)))
	      )
	  ;; status line not found
	    (setq not-yet-parsed parsed-string)
	    (setq parsed-string "")
	  )
	)
       ((eq http-parser-state 'header) 
	;;parsing headers
	(if (string-match "\r\n\r\n" parsed-string)
	    (let ((end-headers (match-end 0)))
	      (set (make-local-variable 'http-headers) (http-parse-headers (substring parsed-string 0 (match-beginning 0))))
	      (if (string= "chunked" 
			   (cdr (assoc "Transfer-Encoding" http-headers)))
		  (setq http-parser-state 'chunked)
		(setq http-parser-state 'dump)
		)
	      (setq parsed-string (substring parsed-string end-headers))
	      )
	  ;;we don't have all the headers yet
	  (setq not-yet-parsed parsed-string)
	  (setq parsed-string "")
	  )
	)
       ((eq http-parser-state 'chunked)
	;; parsing chunked content
	(if (> (string-bytes parsed-string) http-unchunk-chunk-size  )
	    (progn 
	      (setq string  (concat string (substring parsed-string 0 http-unchunk-chunk-size)))
	      (setq parsed-string (substring parsed-string http-unchunk-chunk-size))
	      (setq http-unchunk-chunk-size 0)
	      (message (concat "chunk start:" (substring parsed-string 0 (if (< (string-bytes parsed-string) 10)  (string-bytes parsed-string) 10))))
	      (if (string-match  "\\([0-9a-f]+\\)[^\r^\b]*\\(\r\n\\)" parsed-string )
		  (if (> (setq http-unchunk-chunk-size (hexl-hex-string-to-integer (match-string 1 parsed-string)))
			 0)
		      (setq parsed-string (substring parsed-string (match-end 2)))
		    ;; chunk 0 found we just burry it 
		    (setq parsed-string "")
		    (setq http-parser-state 'trailer)
		    ;;dirty=FIXME : we delete the process
		    (delete-process proc)
		    )
		      ;;we don't have the next chunk-size yet
		(setq not-yet-parsed parsed-string)
		(setq parsed-string "")
		)
	      )
	  ;;the current chunk is not finished yet
	  (setq string  (concat string parsed-string))
	  (setq http-unchunk-chunk-size (- http-unchunk-chunk-size (string-bytes parsed-string)))
	  (setq parsed-string "")
	  )
	)
       ((eq http-parser-state 'trailer)
	;; parsing trailer
	(setq  parsed-string "")
	)
       ((eq http-parser-state 'dump)
	 (setq  string parsed-string)
	 (setq  parsed-string "")
	)
       )
      )
    )
  ) 
  )

(defun http-parse-headers (header-string)
  "parse the header string"
  (let ((lines-list (split-string header-string "\r\n")))
    (mapcar (lambda (line)
	      (if (string-match ": " line)
		  (cons (substring line 0 (match-beginning 0))
			(substring line (match-end 0)))
		line))
	    lines-list)
       )
  )

;; URL encoding for parameters

(defun http-url-encode (str content-type)
  "URL encode STR using CONTENT-TYPE as the coding system."
  (apply 'concat
	 (mapcar (lambda (c)
		   (if (or (and (>= c ?a) (<= c ?z))
			   (and (>= c ?A) (<= c ?Z))
			   (and (>= c ?0) (<= c ?9)))
		       (string c)
		     (format "%%%02x" c)))
		 (encode-coding-string str content-type))))
;; Debugging

(defvar http-log-function 'ignore
  "Function to call for log messages.")

(defun http-log (str)
  "Log STR using `http-log-function'.
The default value just ignores STR."
  (funcall http-log-function str))


(defun http-get-debug (url &optional headers version)
  "Debug the call to `http-get'."
  (interactive "sURL: ")
  (let* ((http-log-function (lambda (str)
			      (save-excursion
				;; dynamic binding -- buf from http-get is used
				(set-buffer buf)
				(insert str))))
	 proc)
    (when (get-buffer "*Debug HTTP-GET*")
      (kill-buffer "*Debug HTTP-GET*"))
    (setq proc (http-get url headers nil version))
    (set (make-local-variable 'http-filter-pre-insert-hook) nil)
    (set (make-local-variable 'http-filter-post-insert-hook) nil)
    (rename-buffer "*Debug HTTP-GET*")))

;; The main function

;;;###autoload
(defun http-get (url &optional headers sentinel version bufname content-type )
  "Get URL in a buffer, and return the process.
You can get the buffer associated with this process using
`process-buffer'.

The optional HEADERS are an alist where each element has the form
\(NAME . VALUE).  Both must be strings and will be passed along with
the request.

With optional argument SENTINEL, the buffer is not shown.  It is the
responsability of the sentinel to show it, if appropriate.  A sentinel
function takes two arguments, process and message.  It is called when
the process is killed, for example.  This is useful when specifying a
non-persistent connection.  By default, connections are persistent.
Add \(\"Connection\" . \"close\") to HEADERS in order to specify a
non-persistent connection.  Usually you do not need to specify a
sentinel, and `ignore' is used instead, to prevent a message being
printed when the connection is closed.

If you want to filter the content as it arrives, bind
`http-filter-pre-insert-hook' and `http-filter-post-insert-hook'.

The optional argument VERSION specifies the HTTP version to use.  It
defaults to version 1.0, such that the connection is automatically
closed when the entire document has been downloaded.  This will then
call SENTINEL, if provided.  If no sentinel is provided, `ignore' will
be used in order to prevent a message in the buffer when the process
is killed.

CONTENT-TYPE is a coding system to use for the encoding of the url param value.  Its upper case print name
will be used for the server.  Possible values are `iso-8859-1' or
`euc-jp' and others.


The coding system of the process is set to `unix', because we need to
distinguish between \\r and \\n.  To correctly decode the text later,
use `decode-coding-region' and get the coding system to use from
`http-headers'."
  (interactive "sURL: ")
  (setq version (or version 1.0))
  (let* (host dir file port proc buf command start-line (message-headers "") )
    (unless (string-match "http://\\([^/]+\\)/\\(.*/\\)?\\([^:]*\\)\\(:\\([0-9]+\\)\\)?" url)
      (error "Cannot parse URL %s" url))
    (unless bufname (setq bufname 
			  (format "*HTTP GET %s *" url)))
    (setq host (match-string 1 url)
	  dir (or (match-string 2 url) "")
	  file (or (match-string 3 url) "")
	  port (or (match-string 5 url) 80)
	  buf (get-buffer-create bufname)
	  proc 
	  (open-network-stream 
	   (concat "HTTP GET " url) buf 
	   (if http-proxy-host http-proxy-host host) 
	   (if http-proxy-port http-proxy-port port) ))
    (if sentinel
	(set-buffer buf)
      (switch-to-buffer buf))
    (erase-buffer)
    (kill-all-local-variables)
    (if content-type
	(setq file
	      (replace-regexp-in-string 
	       "=[^&]+" 
	       (lambda (param) 
		 (concat "=" 
			 (http-url-encode (substring param 1) content-type)
			 ))
	       file
	       )
	      )
      )
    (cond ((= version 1.0)
	   (setq start-line (format "GET %s%s%s" (if http-proxy-host
						     (concat "http://" host "/")						   "/") dir file))
	    (setq http-parser-state nil)
	    )
	  ((= version 1.1)
	   (setq start-line (format "GET %s%s%s HTTP/1.1\r\nHost: %s"
				    (if http-proxy-host
					(concat "http://" host "/")
				      "/") dir file host))
	   (setq http-parser-state 'status-line)
	   (message (format "state :%S" http-parser-state))
	   )


	  (t
	   (error "HTTP Version %S is not supported" version)))

    (when headers
      (setq message-headers (mapconcat (lambda (pair)
					 (concat (car pair) ": " (cdr pair)))
				       headers
				       "\r\n")))
    (setq command (format "%s\r\n%s\r\n" start-line message-headers))
    (http-log (format "Connecting to %s %d\nCommand:\n%s\n" host port command))
    (set-process-sentinel proc (or sentinel 'ignore))
    (set-process-coding-system proc 'binary) ; we need \r\n in the headers!
    (set-process-filter proc 'http-filter)
    (set-marker (process-mark proc) (point-max))
    (process-send-string proc command)
   
    proc))



(provide 'http-get)

;;; http-get.el ends here
