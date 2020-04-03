#!/usr/bin/env sh
":"; exec emacs -Q --no-site-file --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(when noninteractive
  (require 'east-biblatex (expand-file-name "./bin/east-biblatex.el"))
  (require 'json)
  (setq debug-on-error t)
  (display-warning 'east-biblatex (format "%s" command-line-args-left) :debug)
  (let* ((format
	  (cond
	   ((member "--lisp" command-line-args-left) 'lisp)
	   ((member "--json" command-line-args-left) 'json)
	   (t 'html)))
	 (style
	  (or (cadr (member "--style" command-line-args-left))
	      "styles/chicago-author-date-east.csl"))
	 (bibliography (car (reverse command-line-args-left)))
	 (markdown-source  (east-biblatex-create-md-citations bibliography style))
	 (html (east-biblatex-md-citations-to-html markdown-source)))
    (setq command-line-args-left nil)
    (display-warning 'east-biblatex (format "Applying %s to %s" style bibliography) :debug)
    (with-current-buffer html
      ;; (write-region (point-min) (point-max) "/tmp/soup.html" nil nil nil nil)
      ;; (warn "Wrote html, now doing the rest")
      (cond
       ((eq format 'lisp)
	(print
	 (east-biblatex-parse-html-citations (current-buffer))))
       ((eq format 'json)
	(princ
	 (json-encode (east-biblatex-parse-html-citations (current-buffer)))))
       (t
	(princ (buffer-string)))))
    t))

;; pandoc -s \
;;        --bibliography "bib/east.bib" \
;;        --filter pandoc-citeproc \
;;        --csl "./styles/chicago-author-date-east.csl" \
;;        bib-formatting.md \
;;        -o "/tmp/east-formatted.html" && \
;;     echo "Formatted bib in /tmp/east-formatted.html"
