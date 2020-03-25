;;; east-biblatex --- Some Emacs helper functions for EAST’s biblatex database 

(require 'bibtex)
(require 'pp)
(require 'subr-x)
(require 'org)
(require 'ert)
(require 'rx)

(defvar east-biblatex-tibetan-canonical-refs nil
  "A list of accepted canonical references and an rx expression that allows their identification.")

(setq east-biblatex-tibetan-canonical-refs
      `((snar-thang . (and bos "snar" (1+ space) "thang"))
	(peking . (and bos (zero-or-one "{") "Peking"))
	(sde-dge . (and bos "sde" (1+ space) "dge"))
	(co-ne . (and bos "co" (1+ space) "ne"))))

(ert-deftest test-east-biblatex-tibetan-canonical-refs ()
  (should
   (equal
    (string-match-p
     (rx-to-string (cdr (nth 2 east-biblatex-tibetan-canonical-refs)))
     "sde dge")
    0))
  (should
   (equal
    (string-match-p
     (rx-to-string (cdr (nth 2 east-biblatex-tibetan-canonical-refs)))
     "sde")
    nil))
  (should
   (equal
    (string-match-p
     (rx-to-string (cdr (nth 3 east-biblatex-tibetan-canonical-refs)))
     "co ne")
    0)))

;; (ert "test-east-biblatex-tibetan-canonical-refs")

(defvar east-biblatex-tibetan-canonical-refs-compiled-rxs nil
  "The same as ‘east-biblatex-tibetan-canonical-refs’, but with the regular expressions compiled.")

(setq east-biblatex-tibetan-canonical-refs-compiled-rxs
      (mapcar
       (lambda (x)
	 `(,(car x) . ,(rx-to-string (cdr x))))
       east-biblatex-tibetan-canonical-refs))

;; (string-match-p
;;  (cdr (nth 2 east-biblatex-tibetan-canonical-refs-compiled-rxs))
;;  "sde dge")

(defun east-biblatex-normalize-space (string)
  "In STRING, normalize spaces like the xpath function."
  (let ((space-rx (rx-to-string '(1+ space))))
    (save-match-data
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (re-search-forward space-rx nil 'noerr)
          (replace-match " "))
        (buffer-string)))))

(ert-deftest test-east-biblatex-normalize-space ()
    (should
     (equal
      (east-biblatex-normalize-space
       "snar thang 3738 ze 215a1–237b7 | sde dge 4248 zhe
                  201a2–221b4 | co ne zhe 193b3–215a1 | {Peking} 5746 ze
                  213a4–236b1")
      "snar thang 3738 ze 215a1–237b7 | sde dge 4248 zhe 201a2–221b4 | co ne zhe 193b3–215a1 | {Peking} 5746 ze 213a4–236b1")))

;; (ert "test-east-biblatex-normalize-space")

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
	(expected-refs east-biblatex-tibetan-canonical-refs-compiled-rxs)
	results)
    (mapc
     (lambda (cref)
       (let ((refs expected-refs)
	     current-ref
	     matched)
	 (while (and (not matched) (setq current-ref (pop refs)))
	   (setq matched (string-match-p (cdr current-ref) cref))
	   (when matched
	     (push `(,(car current-ref) . ,cref) results)))
	 (unless matched
	   (push `(weird . ,cref) results))))
     fields)
    (mapc
     (lambda (necessary)
       (unless (alist-get necessary results)
	 (push `(,necessary) results)))
     '(snar-thang
       peking
       co-ne
       sde-dge))
    (sort
     results
     (lambda (x y)
       (string-lessp (symbol-name (car x))
		     (symbol-name (car y)))))))

;; (east-biblatex-split-canon-string "{snar thang 3691 ce 1–13a; {Peking} 5700 ce 1–13a5}")

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

(defun east-analyze-canonical-reference (canon-ref)
  "Split the canonical reference CANON-REF into parts."
  (let ((case-fold-search t)
	(parts (split-string
		;; Split off the collection name at the beginning
		(save-match-data
		  (let ((combined-rx
			 (rx-to-string `(or ,@(mapcar #'cdr east-biblatex-tibetan-canonical-refs)))))
		    (cond
		     ((string-match combined-rx (cdr canon-ref))
		      (substring (cdr canon-ref) (string-width (match-string 0 (cdr canon-ref)))))
		     (t
		      (warn "Unexpected start of canonical ref here: %s" (cdr canon-ref))
		      (cdr canon-ref)))))))
	(rx-number-only (rx-to-string '(and bos (1+ num) eos)))
	(rx-range (rx-to-string '(and bos (1+ any) (or "–" "-") (1+ num) (0+ any) eos)))
	(rx-snar-thang (rx-to-string '(and bos (or "snar" "thang" "sde" "dge"))))
	results)
    `(,(car canon-ref)
      (number . ,(let ((number (cl-find-if
				(lambda (part)
				  (string-match-p rx-number-only part))
				parts)))
		   (when number (string-to-number number))))
      (volume . ,(cl-find-if
		  (lambda (part)
		    (string-match-p "[a-z]+" part))
		  parts))
      (positions . ,(let ((positions (cl-find-if
				      (lambda (part)
					(string-match-p rx-range part))
				      parts)))
		      (when positions
			(let ((range (split-string positions (rx-to-string ' (or "–" "-")))))
    			  (unless (= 2 (length range))
    			    (warn "Unexpected range in canonical referene: %s" positions))
    			  `(,(car range) . ,(cadr range)))))))))

(east-analyze-canonical-reference '(sde-dge . "sde dge 4228 tshe 178b4–295a7"))
(east-analyze-canonical-reference '(co-ne . "co ne zhe 193b3–215a1"))


(ert-deftest test-east-analyze-canonical-reference ()
  (let ((cases '(((sde-dge . "sde dge 4228 tshe 178b4–295a7")
		  .
		  (sde-dge
		   (number . 4228)
		   (volume . "tshe")
		   (positions . ("178b4" . "295a7"))))
		 ((co-ne . "co ne zhe 193b3–215a1")
		  .
		  (co-ne
		   (number)
		   (volume . "zhe")
		   (positions "193b3" . "215a1")))
		 ((peking . "{Peking} 5746 ze 213a4–236b1")
		  . (peking
		     (number . 5746)
		     (volume . "ze")
		     (positions "213a4" . "236b1"))))))
    (dolist (c cases)
      (should
       (equal
	(east-analyze-canonical-reference (car c))
	(cdr c))))))

;; (ert "test-east-analyze-canonical-reference")


(defun east-biblatex-bibs-canonical-to-org-table (bibs &optional interactive?)
  "Print canonical entries as an org-mode table."
  (interactive
   (list
    (east-biblatex-find-tib-canon (current-buffer))
    'interactive))
  (let ((table '(hline
		 ("Status" "Title" "co ne" "Peking" "sde dge" "snar thang" "others..."))))
    (mapc
     (lambda (canon)
       (let* ((bib-parsed (with-temp-buffer
			    (insert (nth 2 canon))
			    (goto-char (point-min))
			    (bibtex-parse-entry)))
	      (canon-string  (cdr (assoc "series" bib-parsed)))
	      row)
	 (cond
          ((or (null canon-string)
	       (string-empty-p canon-string))
           (warn "Empty canonical entry: %s" (car canon)))
          (t
           (let ((canon-refs (east-biblatex-split-canon-string canon-string)))
             (if (and (listp canon-refs)
		      (= 4 (length (remq nil (mapcar #'cdr canon-refs)))))
		 (setf row '("✓"))
	       (setf row '("??")))
	     ;; The link to east
	     (setf row
		   `(,@row
		     ,(format "[[http://east.uni-hd.de/bib/%s][%s]]"
			      (cadr (split-string (cdr (assoc "=key=" bib-parsed)) ":"))
			      (east-biblatex-normalize-space
			       (cdr (assoc "title" bib-parsed))))))
	     ;; The refs themselves
             (setf row `(,@row
			 ,@(mapcar
			    #'cdr
			    canon-refs))))))
	 (push row table)))
     bibs)
    (setf table (nreverse table))
    (when interactive?
      (with-current-buffer (get-buffer-create "* east bib table *")
	(erase-buffer)
	(insert
	 (orgtbl-to-orgtbl
	  table
	  (list :backend 'org)))
	(goto-char (point-min))
	(org-table-align)
	(set-buffer-modified-p nil)
	(unless (eq major-mode 'org-mode)
	  (org-mode))
	(pop-to-buffer (current-buffer))))
    table))

(provide 'east-biblatex)
