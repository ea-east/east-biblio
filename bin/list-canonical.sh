#!/usr/bin/env sh
":"; exec emacs -Q --no-site-file --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(when noninteractive
  (require 'east-biblatex (expand-file-name "./bin/east-biblatex.el"))
  ;; (warn (format "%s" command-line-args-left))
  (let* ((format
	  (cond
	   ((member "--lisp" command-line-args-left) 'lisp)
	   ((member "--org" command-line-args-left) 'org)
	   ((member "--html" command-line-args-left) 'html)
	   (t 'bib)))
	 (debug (member "--debug" command-line-args-left))
	 (bib-files (delete nil
			    (mapcar
			     (lambda (x)
			       (when (file-exists-p x) x))
			     command-line-args-left))))
    (unless bib-files
      (error "Please call with one argument, the biblatex file"))
    (when debug (setq debug-on-error t))
    (mapc
     (lambda (bib-file)
       (with-current-buffer (find-file bib-file)
	 (let ((results (east-biblatex-find-tib-canon (current-buffer))))
	   ;; (warn "Found %s canonical entries" (length results))
	   (cond
	    ((eq format 'lisp)
	     (pp results))
	    ((eq format 'org)
	     (princ (orgtbl-to-orgtbl
		     (east-biblatex-bibs-canonical-to-org-table results)
		     (list :backend 'org))))
	    ((eq format 'html)
	     (with-current-buffer (east-biblatex-bibs-canonical-to-org-table results)
	       (with-current-buffer (org-export-to-buffer 'html "*EAST BIB Org HTML Export*")
		 (princ (buffer-string)))))
	    (t
	     (mapc
	      (lambda (entry)
		(princ (format "%% %s in %s\n" (car entry) (cadr entry)))
		(princ (caddr entry))
		(princ "\n\n"))
	      results))))))
     bib-files)))

(provide 'east-biblatex)
