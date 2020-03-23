#!/usr/bin/env sh
":"; exec emacs -Q --no-site-file --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'bibtex)
(require 'pp)
(require 'subr-x)
(require 'org)
(require 'ert)

(defun east-biblatex-find-tib-canon (bib-buffer &optional interactive?)
  "Find entries in a biblatex buffer that are Tibetan canonical entries."
  (interactive
   (list (current-buffer) t))
  (with-current-buffer bib-buffer
    (bibtex-search-entries
     ""
     (lambda (bib-start bib-end)
       (save-excursion
	 (let ((bib (progn
		      (goto-char bib-start)
		      (bibtex-parse-entry))))
	   (and
	    (assoc "language" bib)
	    (string-match-p
	     ;; (rx-to-string '(and bow "bo" eow))
	     "\\(?:\\<bo\\>\\)"
	     (downcase (cdr (assoc "language" bib))))
	    (assoc "keywords" bib)
	    (string-match-p "canonical" (downcase (cdr (assoc "keywords" bib))))))))
     nil
     interactive?)))

(defun east-biblatex-split-canon-string (canon-string)
  "Split CANON-STRING (as contained in EAST’s series field) into a normalized list."
  (let ((fields (delete ""
			(split-string
			 ;; normalize space before splitting
			 (string-join (split-string (string-trim canon-string "[ \t\n\r{]+" "[ \t\n\r}]+")) " ")
			 ";\\s-*")))
	(expected-refs `((snar-thang . ,(rx-to-string '(and bol "snar" (1+ space) "thang")))
			 (peking . ,(rx-to-string '(and bol (zero-or-one "{") "Peking")))
			 (sde-dge . ,(rx-to-string '(and bol "sde" (1+ space) "dge")))
			 (co-ne . ,(rx-to-string '(and bol "co" (1+ space) "ne")))))
	results)
    (setf results '((snar-thang . nil)
		    (peking . nil)
		    (co-ne . nil)
		    (sde-dge . nil)))
    (mapc
     (lambda (cref)
       (let ((refs expected-refs)
	     current-ref
	     matched)
	 (while (and (not matched) (setq current-ref (pop refs)))
	   (setq matched (string-match-p (cdr current-ref) cref))
	   (when matched
	     (setf
	      (alist-get (car current-ref) results)
	      cref)))
	 (unless matched
	   (push `(weird . ,cref) results))))
     fields)
    (sort
     results
     (lambda (x y)
       (string-lessp (symbol-name (car x))
		     (symbol-name (car y)))))))




(ert-deftest test-east-biblatex-split-canon-string ()
  (let ((cases '(("{snar thang 3691 ce 1–13a; {Peking} 5700 ce 1–13a5}"
		  . ((co-ne)
		     (peking . "{Peking} 5700 ce 1–13a5")
		     (sde-dge)
		     (snar-thang . "snar thang 3691 ce 1–13a")))
		 ("{snar thang 3720 we 188b–323; sde dge 4228 tshe 178b4–295a7; co ne tshe 182b2–307a7; {Peking} 5728 we 209b8–355a6}"
		  . ((co-ne . "co ne tshe 182b2–307a7")
		     (peking . "{Peking} 5728 we 209b8–355a6")
		     (sde-dge . "sde dge 4228 tshe 178b4–295a7")
		     (snar-thang . "snar thang 3720 we 188b–323")))
		 ;; a bad case
		 ("{snar thang 3720 we 188b–323; sde dge 4228 tshe 178b4–295a7; co ne tshe 182b2–307a7; {Peking} 5728 we 209b8–355a6; dunno what this is;}" .
		  ((co-ne . "co ne tshe 182b2–307a7")
		   (peking . "{Peking} 5728 we 209b8–355a6")
		   (sde-dge . "sde dge 4228 tshe 178b4–295a7")
		   (snar-thang . "snar thang 3720 we 188b–323")
		   (weird . "dunno what this is"))))))
    (dolist (c cases)
      (should
       (equal
	(east-biblatex-split-canon-string (car c))
	(cdr c)))
      cases)))

;; (ert "test-east-biblatex-split-canon-string")

(defun east-biblatex-bibs-canonical-to-org-table (bibs &optional interactive?)
  "Print canonical entries as an org-mode table."
  (interactive
   (list
    (east-biblatex-find-tib-canon (current-buffer))
    'interactive))
  (with-current-buffer (get-buffer-create "* east biblatex stuff *")
    (erase-buffer)
    (insert
     (string-join
      (list
       ""
       "Status"
       "Title"
       "Canonical refs ..."
       "")
      "|"))
    (insert "\n|---|---|---|\n")
    (mapc
     (lambda (canon)
       (let* ((bib-parsed (with-temp-buffer
			    (insert (nth 2 canon))
			    (goto-char (point-min))
			    (bibtex-parse-entry)))
	      (canon-string  (cdr (assoc "series" bib-parsed))))
	 (cond
          ((or (null canon-string)
               (string-empty-p canon-string))
           (warn "Empty canonical entry: %s" (car canon)))
          (t
           (let ((canon-refs (east-biblatex-split-canon-string canon-string)))
             (insert "|")
             (if (and (listp canon-refs)
		      (= 4 (length canon-refs)))
		 (insert "✓ |")
               (insert "?? |"))
             (insert
              (string-join
	       (split-string
		(string-join
		 `(
                   ,(format "[[http://east.uni-hd.de/bib/%s][%s]]"
                            (cadr (split-string (cdr (assoc "=key=" bib-parsed)) ":"))
                            (string-join
                             (split-string
                              (cdr (assoc "title" bib-parsed))
                              (rx-to-string '(or "]" "[" space)))
                             " "))
                   ,@(mapcar #'cdr canon-refs))
		 " | "))
	       " "))
             (insert "|\n"))))))
     bibs)
    (when interactive?
      (let ((results (buffer-string)))
	(with-current-buffer (get-buffer-create "* east bib table *")
	  (erase-buffer)
	  (insert results)
	  (goto-char (point-min))
	  (normal-mode)
	  (set-buffer-modified-p nil)
	  (unless (eq major-mode 'org-mode)
	    (org-mode))
	  (pop-to-buffer (current-buffer)))))
    (current-buffer)))

(when noninteractive
  (warn (format "%s" command-line-args-left))
  (let* ((format
	  (cond
	   ((member "--lisp" command-line-args-left) 'lisp)
	   ((member "--org" command-line-args-left) 'org)
	   ((member "--html" command-line-args-left) 'html)
	   (t 'bib)))
	 (bib-files (delete nil
			    (mapcar
			     (lambda (x)
			       (when (file-exists-p x) x))
			     command-line-args-left))))
    (unless bib-files
      (error "Please call with one argument, the biblatex file"))
    (mapc
     (lambda (bib-file)
       (with-current-buffer (find-file bib-file)
	 (let ((results (east-biblatex-find-tib-canon (current-buffer))))
	   (cond
	    ((eq format 'lisp)
	     (pp results))
	    ((eq format 'org)
	     (with-current-buffer (east-biblatex-bibs-canonical-to-org-table results)
	       (princ (buffer-string))))
	    ((eq format 'html)
	     (with-current-buffer (east-biblatex-bibs-canonical-to-org-table results)
	       (princ (buffer-string))))
	    (t
	     (mapc
	      (lambda (entry)
		(princ (format "%% %s in %s\n" (car entry) (cadr entry)))
		(princ (caddr entry))
		(princ "\n\n"))
	      results))))))
     bib-files)))

