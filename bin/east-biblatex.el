;;; east-biblatex --- Some Emacs helper functions for EAST’s biblatex database 

(require 'bibtex)
(require 'pp)
(require 'subr-x)
(require 'org)
(require 'ert)
(require 'rx)

(defvar east-biblatex-checks-to-do nil
  "Which checks to do on the bibliographic entries.")

(setq east-biblatex-checks-to-do (list
				  #'east-biblatex-check-tibetan-canonical-ref))

(defvar east-biblatex-rx-snar-thang '(and "snar" (1+ space) "thang"))

(defvar east-biblatex-rx-peking '(and (zero-or-one "{") "Peking"))

(defvar east-biblatex-rx-sde-dge '(and "sde" (1+ space) "dge"))

(defvar east-biblatex-rx-co-ne  '(and "co" (1+ space) "ne"))


(defvar east-biblatex-tibetan-canonical-refs nil
  "A list of accepted canonical references and an rx expression that allows their identification.")

(setq east-biblatex-tibetan-canonical-refs
      `((snar-thang . (and bos ,east-biblatex-rx-snar-thang))
	(peking . (and bos ,east-biblatex-rx-peking))
	(sde-dge . (and bos ,east-biblatex-rx-sde-dge))
	(co-ne . (and bos ,east-biblatex-rx-co-ne))))

(ert-deftest test-east-biblatex-tibetan-canonical-refs ()
  (should
   (equal
    (string-match-p
     (rx-to-string (cdr (nth 2 east-biblatex-tibetan-canonical-refs)))
     ;; "\\(?:\\`sde[[:space:]]+dge\\)"
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

(defun east-biblatex-alist-get (key alist)
  "Get values for KEY in ALIST when KEY is a string.

Just wraps alist."
  (if (stringp key)
      (alist-get key alist nil nil #'string=)
    (alist-get key alist)))

;; (alist-get "=key=" '(("=key=" . soup)))nil
;; (east-biblatex-alist-get "=key=" '(("=key=" . soup)))soup


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
	    (east-biblatex-alist-get "language" bib)
	    (string-match-p
	     ;; (rx-to-string '(and bow "bo" eow))
	     "\\(?:\\<bo\\>\\)"
	     (downcase (east-biblatex-alist-get "language" bib)))
	    (east-biblatex-alist-get "keywords" bib)
	    (string-match-p "canonical" (downcase (east-biblatex-alist-get "keywords" bib)))))))
     nil
     interactive?)))

(defun east-biblatex-split-canon-string (canon-string)
  "Split CANON-STRING (as contained in EAST’s series field) into a normalized list."
  (let ((fields (delete "" ; split and clean up fields in canon string
			(split-string
			 ;; normalize space before splitting
			 (string-join (split-string (string-trim canon-string "[ \t\n\r{]+" "[ \t\n\r}]+")) " ")
			 ";\\s-*")))
	(expected-refs east-biblatex-tibetan-canonical-refs-compiled-rxs)
	results)
    ;; For each field, find a match or default to ‘weird’
    (mapc
     (lambda (cref)
       (let ((refs expected-refs)
	     current-ref
	     matched)
	 (while (and (not matched) (setq current-ref (pop refs)))
	   (setq matched (string-match-p (cdr current-ref) cref))
	   (when matched
	     (push `(,(car current-ref) ,cref ,(east-analyze-canonical-reference (cons (car current-ref) cref))) results)))
	 (unless matched
	   (push `(weird ,cref) results))))
     fields)
    ;; Fill up the result structure (add missing canonical references)
    (mapc
     (lambda (necessary)
       (unless (alist-get necessary results)
	 (push `(,necessary) results)))
     '(snar-thang
       peking
       co-ne
       sde-dge))
    ;; Sort things: mainly by key, or, if that is equal, by content.
    (sort
     results
     (lambda (x y)
       (let ((name-x  (symbol-name (car x)))
	     (name-y (symbol-name (car y)))
	     (content-x (nth 1 x))
	     (content-y (nth 1 y)))
	 (if (string= name-x name-y)
	     (string-lessp content-x content-y)
	   (string-lessp name-x name-y)))))))

;; (east-biblatex-split-canon-string "{snar thang 3691 ce 1–13a; {Peking} 5700 ce 1–13a5}")

(ert-deftest test-east-biblatex-split-canon-string ()
  (let ((cases '(("{snar thang 3691 ce 1–13a; {Peking} 5700 ce 1–13a5}"
		  . ((co-ne)
		     (peking "{Peking} 5700 ce 1–13a5"
			     (peking
			      (number . 5700)
			      (volume . "ce")
			      (positions "1" . "13a5")))
		     (sde-dge)
		     (snar-thang "snar thang 3691 ce 1–13a"
				 (snar-thang
				  (number . 3691)
				  (volume . "ce")
				  (positions "1" . "13a")))))
		 ("{snar thang 3720 we 188b–323; sde dge 4228 tshe 178b4–295a7; co ne tshe 182b2–307a7; {Peking} 5728 we 209b8–355a6}"
		  . ((co-ne "co ne tshe 182b2–307a7"
			    (co-ne
			     (number)
			     (volume . "tshe")
			     (positions "182b2" . "307a7")))
		     (peking "{Peking} 5728 we 209b8–355a6"
			     (peking
			      (number . 5728)
			      (volume . "we")
			      (positions "209b8" . "355a6")))
		     (sde-dge "sde dge 4228 tshe 178b4–295a7"
			      (sde-dge
			       (number . 4228)
			       (volume . "tshe")
			       (positions "178b4" . "295a7")))
		     (snar-thang "snar thang 3720 we 188b–323"
				 (snar-thang
				  (number . 3720)
				  (volume . "we")
				  (positions "188b" . "323")))))
		 ;; a bad case
		 ("{snar thang 3720 we 188b–323; sde dge 4228 tshe 178b4–295a7; co ne tshe 182b2–307a7; {Peking} 5728 we 209b8–355a6; dunno what this is;}" .
		  ((co-ne "co ne tshe 182b2–307a7"
			  (co-ne
			   (number)
			   (volume . "tshe")
			   (positions "182b2" . "307a7")))
		   (peking "{Peking} 5728 we 209b8–355a6"
			   (peking
			    (number . 5728)
			    (volume . "we")
			    (positions "209b8" . "355a6")))
		   (sde-dge "sde dge 4228 tshe 178b4–295a7"
			    (sde-dge
			     (number . 4228)
			     (volume . "tshe")
			     (positions "178b4" . "295a7")))
		   (snar-thang "snar thang 3720 we 188b–323"
			       (snar-thang
				(number . 3720)
				(volume . "we")
				(positions "188b" . "323")))
		   (weird "dunno what this is")))
		 ;; Two mentions in Peking
		 ("{snar thang 3717 tshe 21b–131b; snar thang 3770 ze 65b–186a; sde dge 4239 zhe 51a3–151a6; co ne zhe 50b3–143b2; {Peking} 5725 tshe 21b2–137a8; {Peking} 5738 ze 71a5–183a7}"
		  . ((co-ne "co ne zhe 50b3–143b2"
			    (co-ne
			     (number)
			     (volume . "zhe")
			     (positions "50b3" . "143b2")))
		     (peking "{Peking} 5725 tshe 21b2–137a8"
			     (peking
			      (number . 5725)
			      (volume . "tshe")
			      (positions "21b2" . "137a8")))
		     (peking "{Peking} 5738 ze 71a5–183a7"
			     (peking
			      (number . 5738)
			      (volume . "ze")
			      (positions "71a5" . "183a7")))
		     (sde-dge "sde dge 4239 zhe 51a3–151a6"
			      (sde-dge
			       (number . 4239)
			       (volume . "zhe")
			       (positions "51a3" . "151a6")))
		     (snar-thang "snar thang 3717 tshe 21b–131b"
				 (snar-thang
				  (number . 3717)
				  (volume . "tshe")
				  (positions "21b" . "131b")))
		     (snar-thang "snar thang 3770 ze 65b–186a"
				 (snar-thang
				  (number . 3770)
				  (volume . "ze")
				  (positions "65b" . "186a"))))))))
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

;; (east-analyze-canonical-reference '(sde-dge . "sde dge 4228 tshe 178b4–295a7"))
;; (east-analyze-canonical-reference '(co-ne . "co ne zhe 193b3–215a1"))


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

(defun east-biblatex-is-canonical-ref-p (bib)
  (and
   (east-biblatex-alist-get "keywords" bib)
   (east-biblatex-alist-get "language" bib)
   (string-match-p ;; (rx-to-string '(and bow "bo" eow))
    "\\(?:\\<bo\\>\\)"
    (east-biblatex-alist-get "language" bib))
   (string-match-p "canonical" (east-biblatex-alist-get "keywords" bib))))

(defun east-biblatex-tibetan-peking-ref-is-set-p (bib)
  (string-match-p (rx (eval east-biblatex-rx-peking))
		  (or (east-biblatex-alist-get "series" bib)
		      "")))

;; (east-biblatex-tibetan-peking-ref-is-set-p '(("series" . "Peking")))

(defun east-biblatex-tibetan-snar-thang-ref-is-set-p (bib)
  (string-match-p (rx (eval east-biblatex-rx-snar-thang))
		  (or (east-biblatex-alist-get "series" bib)
		      "")))

(defun east-biblatex-tibetan-sde-dge-is-set-p (bib)
  (string-match-p (rx (eval east-biblatex-rx-sde-dge))
		  (or (east-biblatex-alist-get "series" bib)
		      "")))

(defun east-biblatex-tibetan-co-ne-ref-is-set-p (bib)
  (string-match-p (rx (eval east-biblatex-rx-co-ne))
		  (or (east-biblatex-alist-get "series" bib)
		      "")))

(defun east-biblatex-check-tibetan-canonical-ref (bib)
  (mapc
   (lambda (test)
     (unless (apply test (list bib))
       (warn "Failed test: %s returns FALSE for %s"
	     (symbol-name test)
	     (car bib))))
   (list
    #'east-biblatex-tibetan-snar-thang-ref-is-set-p
    #'east-biblatex-tibetan-peking-ref-is-set-p
    #'east-biblatex-tibetan-sde-dge-is-set-p
    #'east-biblatex-tibetan-co-ne-ref-is-set-p))
  bib)

(defun east-biblatex-do-checks (bib)
  "Run checks on BIB."
  (mapc
   (lambda (check)
     (apply check (list bib)))
   east-biblatex-checks-to-do))

(defun east-biblatex-bibs-to-structured-data (bibs &optional complain)
  "Convert BIBS to a structured set of data.

If COMPLAIN is non-nil, raise warnings about expected but absent data."
  (let (results)
    (mapc
     (lambda (bib)
       (let* ((bib-parsed (with-temp-buffer
			    (insert (nth 2 bib))
			    (goto-char (point-min))
			    (bibtex-parse-entry))))
	 ;; List checks and consequences here
	 (when (and (member '("language" . "{bo}") bib-parsed)
		    (assoc "keywords" bib-parsed)
		    (assoc "series" bib-parsed)
		    (string-match-p "canonical" (cdr (assoc "keywords" bib-parsed))))
	   (push `("series:analyzed" . ,(east-biblatex-split-canon-string (cdr (assoc "series" bib-parsed))))
		 bib-parsed))
	 (push `("east:url" . ,(format "https://east.ikga.oeaw.ac.at/bib/%s"
				       (cadr (split-string (or (east-biblatex-alist-get "=key=" bib-parsed) "east:ERROR") ":"))))
	       bib-parsed)	 
	 (push bib-parsed results)))
     bibs)
    (when complain
      (mapc
       (lambda (bib) (east-biblatex-do-checks bib))
       bibs))
    (nreverse (mapcar (lambda (bib) (sort bib (lambda (x y) (string-lessp (car x) (car y))))) results))))


(defun east-biblatex-bibs-canonical-to-org-table (bibs &optional complain interactive?)
  "Convert canonical entries BIBS to an org-mode table.

BIBS should be in the format returned by ‘east-biblatex-find-tib-canon’."
  (interactive
   (list
    (east-biblatex-find-tib-canon (current-buffer))
    nil
    'interactive))
  (let ((table '(hline
		 ("Status" "Title" "co ne" "Peking" "sde dge" "snar thang" "others...")))
	(bibs-structured (east-biblatex-bibs-to-structured-data bibs complain)))
    (mapc
     (lambda (bib-parsed)
       (let ((canon-parsed  (east-biblatex-alist-get "series:analyzed" bib-parsed))
	     row)
	 (cond
	  ((null canon-parsed)
           (warn "Empty canonical entry: %s" (car bib-parsed)))
	  (t
           ;; (warn "Looking at canon-parsed: %s" canon-parsed)
           (if (and (listp canon-parsed)
		    ;; Check if we have 4, as ideal for Tibetan
		    (= 4 (length (remq nil (mapcar #'cdr canon-parsed)))))
	       (setf row '("✓"))
	     (setf row '("??")))
	   ;; The link to east
	   (setf row
		 `(,@row
		   ,(format "[[%s][%s]]"
			    (east-biblatex-alist-get "east:url" bib-parsed)
			    (east-biblatex-normalize-space
			     (east-biblatex-alist-get "title" bib-parsed)))))
	   ;; The refs themselves
           (setf row `(,@row
		       ,@(mapcar
			  #'cadr
			  canon-parsed)))))
	 (push row table)))
     bibs-structured)
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
