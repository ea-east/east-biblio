#!/usr/bin/env sh
":"; exec emacs -Q --no-site-file --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(when noninteractive
  (defun show-usage ()
    (message
     "Usage: list-canonical.sh [options] bib-file [bib-file ...]

- bib-file is a BibLaTeX database (.bib)

The default is to filter the database for what looks like Tibetan entries.

Recognized options to change the default behaviour are:

--help: show this message

Change output format:

--lisp: output the filtered records after parsing to s-expressions
--org:  output the filtered records in an org-mode table
--html: output the filtered records as an html table (based on org-mode)
--json: output the filtered records after converting to json

Modifiers:

--debug: say what’s going on
--verify: make sure the data is as expected

Sorting:

--sort-peking: sort by Peking number"))
  
  (require 'east-biblatex (expand-file-name "./bin/east-biblatex.el"))
  (require 'json)
  
  ;; (warn (format "%s" command-line-args-left))

  (if (member "--help" command-line-args-left)
      (progn
	(show-usage)
	(setq command-line-args-left nil))
    (let* ((format
	    (cond
	     ((member "--lisp" command-line-args-left) 'lisp)
	     ((member "--json" command-line-args-left) 'json)
	     ((member "--org" command-line-args-left) 'org)
	     ((member "--html" command-line-args-left) 'html)
	     (t 'bib)))
	   (debug (member "--debug" command-line-args-left))
	   (verify (member "--verify" command-line-args-left))
	   (sort
	    (cond
	     ((member "--sort-peking" command-line-args-left)
	      #'east-biblatex-sort-peking)
	     (t nil)))
	   (bib-files (delete nil
			      (mapcar
			       (lambda (x)
				 (when (file-exists-p x) x))
			       command-line-args-left))))
      (setq command-line-args-left nil)
      (unless bib-files
	(show-usage)
	(error "*** Please pass the biblatex file as an argument ***"))
      (when debug (setq debug-on-error t))
      (mapc
       (lambda (bib-file)
	 (with-current-buffer (find-file bib-file)
	   (let ((results (east-biblatex-find-tib-canon (current-buffer))))
	     ;; (warn "Found %s canonical entries" (length results))
	     (cond
	      ((eq format 'lisp)
	       ;; (pp		    ; remove hlines and the header row
	       ;;  (delete 'hline (cdr (east-biblatex-bibs-canonical-to-org-table results))))
	       (pp (east-biblatex-bibs-to-structured-data results sort verify)))
	      ((eq format 'json)
	       ;; (pp		    ; remove hlines and the header row
	       ;;  (delete 'hline (cdr (east-biblatex-bibs-canonical-to-org-table results))))
	       (princ (json-encode (east-biblatex-bibs-to-structured-data results sort verify))))
	      ((eq format 'org)
	       (with-current-buffer (east-biblatex-bibs-canonical-to-org-table results sort verify)
		 (princ (buffer-string))))
	      ((eq format 'html)
	       (with-current-buffer (east-biblatex-bibs-canonical-to-org-table results sort verify)
		 (with-current-buffer (org-export-to-buffer 'html "* html export *")
		   (princ (buffer-string)))))
	      (t
	       ;; just for verification purposes
	       (when verify
		 (east-biblatex-bibs-canonical-to-org-table results sort verify))
	       (mapc
		(lambda (entry)
		  (princ (format "%% %s in %s\n" (car entry) (cadr entry)))
		  (princ (caddr entry))
		  (princ "\n\n"))
		results))))))
       bib-files))))


