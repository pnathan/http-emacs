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

(defvar http-filter-pre-insert-hook nil
  "Hook run by the `http-filter'.
This is called whenever a chunk of input arrives, before it is
inserted into the buffer.  If you want to modify the string that gets
inserted, modify the variable `string' which is dynamically bound to
what will get inserted in the end.  The string will be inserted at
the process-mark, which you can get by calling \(process-mark proc).
`proc' is dynamically bound to the process, and the current buffer
is the very buffer where the string will be inserted.  Note that
`http-unchunk' does not deal gracefully with changed input -- so only
change `string' if you know that `http-unchunk' will not run.")

(defvar http-filter-post-insert-hook '(http-headers http-unchunk)
  "Hook run by the `http-filter'.
This is called whenever a chunk of input arrives, after it has been
inserted, but before the process-mark has moved.  Therefore, the new
text lies between the process-mark and point.  You can get the value
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
	;; Insert the text, advancing the process marker.
	(goto-char (process-mark proc))
	(run-hooks 'http-filter-pre-insert-hook)
	(insert string)
	(run-hooks 'http-filter-post-insert-hook)
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))


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

;; Dealing with headers

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

(defun http-headers ()
  "Check the current buffer for headers.
If any are found, they are stored in the variable `http-headers'."
  (unless http-headers
    (save-excursion
      (goto-char (point-min))
      (when (looking-at "HTTP/[0-9.]+ \\([0-9]+\\) \\(.*\\)\r\n")
	(set (make-local-variable 'http-status-code)
	     (string-to-number (match-string 1)))
	(set (make-local-variable 'http-reason-phrase)
	     (match-string 2))
	(when (search-forward "\r\n\r\n")
	  (set (make-local-variable 'http-headers)
	       (mapcar (lambda (line)
			 (if (string-match ": " line)
			     (cons (substring line 0 (match-beginning 0))
				   (substring line (match-end 0)))
			   line))
		       (split-string (buffer-substring (point-min) (point)) "\r\n")))
	  (delete-region (point-min) (point))
	  (message http-reason-phrase)
	  http-status-code)))))

;; Dealing with the chunked encoding

(defvar http-unchunk-chunk-size 0
  "Size of the current unfinished chunk.")
(make-variable-buffer-local 'http-unchunk-chunk-size)

(defun http-unchunk ()
  "Undo the chunking transfer encoding.
The counter http-unchunk-chunk-size says how much content we still have to

expect.  Whenever a chunk-size is received, it is increased, whenever we insert
text into the buffer, it is decreased.  This function assumes that we are at the
beginning of the buffer when chunking starts -- as we should be, if `http-headers'
runs before us."
  (when (string= "chunked" (cdr (assoc "Transfer-Encoding" http-headers)))
    (save-excursion
      (catch 'done
	(goto-char (process-mark proc))
	(while (< (point) (point-max))
	  (when (> http-unchunk-chunk-size 0)
	    (when (> (- (buffer-size) (point)) http-unchunk-chunk-size)
	      (setq http-unchunk-chunk-size
		    (- http-unchunk-chunk-size (- (buffer-size) (point))))
	      (throw 'done t))
	    (forward-char http-unchunk-chunk-size)
	    (setq http-unchunk-chunk-size 0))
	  (when (looking-at "\\([0-9a-f]\\).*?\r\n")
	    (setq http-unchunk-chunk-size (hexl-hex-string-to-integer (match-string 1)))
	    (replace-match "")
	    (when (= 0 http-unchunk-chunk-size)
	      (setq http-headers (delete (assoc "Transfer-Encoding" http-headers)
					 http-headers))
	      (throw 'done t)))
	  ;; hm, need to deal with the case that we are still here: 
	  ;; this still leaves some output at the end.
	  ;;(message "How can we still be here in http-unchunk?")
	  (if (< (point) (point-max))
	      (forward-char 1)
	    (throw 'done t)))))))

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
  (let* (host dir file port proc buf command start-line (message-headers ""))
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
						     (concat "http://" host "/")						   "/") dir file)))
	  ((= version 1.1)
	   (setq start-line (format "GET %s%s%s HTTP/1.1\r\nHost: %s"
				    (if http-proxy-host
				    (concat "http://" host "/")
				    "/") dir file host)))
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
