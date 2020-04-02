;;; east-biblatex --- Some Emacs helper functions for EAST’s biblatex database

;; To run tests from within Emacs: M-x "^test-east"

;; Or from command line:  emacs -batch -Q -l ert -l bin/east-biblatex.el -f ert-run-tests-batch-and-exit

(require 'bibtex)
(require 'pp)
(require 'subr-x)
(require 'org)
(require 'ert)
(require 'rx)

(when noninteractive
  (setq debug-on-error t))

(defvar east-biblatex-checks-to-do nil
  "Which checks to do on the bibliographic entries.")

(setq east-biblatex-checks-to-do (list
				  #'east-biblatex-check-tibetan-canonical-ref))

(eval-when-compile
  (defvar east-biblatex-rx-snar-thang '(and "snar" (1+ space) "thang"))

  (defvar east-biblatex-rx-peking '(and (zero-or-one "{") "Peking"))

  (defvar east-biblatex-rx-sde-dge '(and "sde" (1+ space) "dge"))

  (defvar east-biblatex-rx-co-ne  '(and "co" (1+ space) "ne")))


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

(defun east-biblatex--alist-get-single-step (key alist)
  "Do a single assoc call for key in alist, adjusting for type of key (string or symbol)"
  (if (stringp key)
      (alist-get key alist nil nil #'string=)
    (alist-get key alist)))

(defun east-biblatex-alist-get (key alist)
  "Get values for KEY in ALIST when KEY is a string.

Just wraps alist, except KEY can also be a list of steps."
  (let ((key-list (if (listp key) key (list key)))
	(results alist)
	curr-key)
    (while (and results (setq curr-key (pop key-list)))
      (setq results (east-biblatex--alist-get-single-step curr-key results)))
    results))

;; (alist-get "=key=" '(("=key=" . soup)))nil
;; (east-biblatex-alist-get "=key=" '(("=key=" . soup)))soup
;; (east-biblatex-alist-get
;;  '("=key=" "potato") '(("=key=" . (("potato" . soup))))) soup

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
	(rx-number-only (rx-to-string '(and bos (1+ num) (0+ (in lower)) eos)))
	(rx-range (rx-to-string '(and bos (1+ any) (or "–" "-") (1+ num) (0+ any) eos)))
	(rx-snar-thang (rx-to-string '(and bos (or "snar" "thang" "sde" "dge"))))
	results)
    `((canon . ,(car canon-ref))
      (number . ,(let ((number (cl-find-if
				(lambda (part)
				  (string-match-p rx-number-only part))
				parts)))
		   number))
      (volume . ,(cl-find-if
		  (lambda (part)
		    (string-match-p "^[a-z]+" part))
		  parts))
      (positions . ,(let ((positions (cl-find-if
				      (lambda (part)
					(string-match-p rx-range part))
				      parts)))
		      (when positions
			(let ((range (split-string positions (rx-to-string ' (or "–" "-")))))
    			  (unless (= 2 (length range))
    			    (warn "Unexpected range in canonical referene: %s" positions))
    			  `(,(car range) ,(cadr range)))))))))

;; (east-analyze-canonical-reference '(sde-dge . "sde dge 4228 tshe 178b4–295a7"))
;; (east-analyze-canonical-reference '(co-ne . "co ne zhe 193b3–215a1"))

;; (json-encode '((canon . sde-dge) (number . 4228) (volume . "tshe") (positions "178b4" "295a7")))
;; (alist-get 'positions '((canon . sde-dge) (number . 4228) (volume . "tshe") (positions "178b4" "295a7")))

(ert-deftest test-east-analyze-canonical-reference ()
  (let ((cases '(((sde-dge . "sde dge 4228 tshe 178b4–295a7")
		  .
		  ((canon . sde-dge)
		   (number . "4228")
		   (volume . "tshe")
		   (positions . ("178b4" "295a7"))))
		 ((co-ne . "co ne zhe 193b3–215a1")
		  .
		  ((canon . co-ne)
		   (number)
		   (volume . "zhe")
		   (positions . ("193b3" "215a1"))))
		 ((peking . "{Peking} 5746 ze 213a4–236b1")
		  . ((canon . peking)
		     (number . "5746")
		     (volume . "ze")
		     (positions . ("213a4"  "236b1"))))
		 ((peking . "{Peking} 5717b che 1–390a8")
		  . ((canon . peking)
		     (number . "5717b")
		     (volume . "che")
		     (positions "1" "390a8"))))))
    (dolist (c cases)
      (should
       (equal
	(east-analyze-canonical-reference (car c))
	(cdr c))))))

;; (ert "test-east-analyze-canonical-reference")

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
	     (push `(,(car current-ref)
		     (string . ,cref)
		     ,@(east-analyze-canonical-reference (cons (car current-ref) cref)))
		   results)))
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
       (let* ((name-x  (symbol-name (car x)))
	     (name-y (symbol-name (car y)))
	     (number-x (east-biblatex-alist-get
			'number
			(cdr x)))
	     (number-y (east-biblatex-alist-get
			'number
			(cdr y))))
	 (if (string= name-x name-y)
	     (string-lessp (or number-x "") (or number-y ""))
	   (string-lessp name-x name-y)))))))

;; (east-biblatex-split-canon-string "{snar thang 3691 ce 1–13a; {Peking} 5700 ce 1–13a5}")
(ert-deftest test-east-biblatex-split-canon-string ()
  (let ((cases '(("{snar thang 3691 ce 1–13a; {Peking} 5700 ce 1–13a5}"
		  . ((co-ne)
		     (peking
		      (string . "{Peking} 5700 ce 1–13a5")
		      (canon . peking)
		      (number . 5700)
		      (volume . "ce")
		      (positions . ("1" "13a5")))
		     (sde-dge)
		     (snar-thang
		      (string . "snar thang 3691 ce 1–13a")
		      (canon . snar-thang)
		      (number . 3691)
		      (volume . "ce")
		      (positions .  ("1" "13a")))))
		 ("{snar thang 3720 we 188b–323; sde dge 4228 tshe 178b4–295a7; co ne tshe 182b2–307a7; {Peking} 5728 we 209b8–355a6}"
		  . ((co-ne
		      (string . "co ne tshe 182b2–307a7")
		      (canon . co-ne)
		      (number)
		      (volume . "tshe")
		      (positions . ("182b2" "307a7")))
		     (peking
		      (string . "{Peking} 5728 we 209b8–355a6")
		      (canon . peking)
		      (number . 5728)
		      (volume . "we")
		      (positions . ("209b8" "355a6")))
		     (sde-dge
		      (string . "sde dge 4228 tshe 178b4–295a7")
		      (canon . sde-dge)
		      (number . 4228)
		      (volume . "tshe")
		      (positions . ("178b4" "295a7")))
		     (snar-thang
		      (string . "snar thang 3720 we 188b–323")
		      (canon . snar-thang)
		      
		      (number . 3720)
		      (volume . "we")
		      (positions . ("188b" "323")))))
		 ;; a bad case
		 ("{snar thang 3720 we 188b–323; sde dge 4228 tshe 178b4–295a7; co ne tshe 182b2–307a7; {Peking} 5728 we 209b8–355a6; dunno what this is;}" .
		  ((co-ne
		    (string . "co ne tshe 182b2–307a7")
		    (canon . co-ne)
		    (number)
		    (volume . "tshe")
		    (positions . ("182b2" "307a7")))
		   (peking
		    (string . "{Peking} 5728 we 209b8–355a6")
		    (canon . peking)
		    
		    (number . 5728)
		    (volume . "we")
		    (positions . ("209b8" "355a6")))
		   (sde-dge
		    (string . "sde dge 4228 tshe 178b4–295a7")
		    (canon . sde-dge)
		    
		    (number . 4228)
		    (volume . "tshe")
		    (positions . ("178b4" "295a7")))
		   (snar-thang
		    (string . "snar thang 3720 we 188b–323")
		    (canon . snar-thang)
		    
		    (number . 3720)
		    (volume . "we")
		    (positions . ("188b" "323")))
		   (weird "dunno what this is")))
		 ;; Two mentions in Peking
		 ("{snar thang 3717 tshe 21b–131b; snar thang 3770 ze 65b–186a; sde dge 4239 zhe 51a3–151a6; co ne zhe 50b3–143b2; {Peking} 5725 tshe 21b2–137a8; {Peking} 5738 ze 71a5–183a7}"
		  . ((co-ne
		      (string . "co ne zhe 50b3–143b2")
		      (canon . co-ne)
		      
		      (number)
		      (volume . "zhe")
		      (positions . ("50b3" "143b2")))
		     (peking
		      (string . "{Peking} 5725 tshe 21b2–137a8")
		      (canon . peking)
		      
		      (number . 5725)
		      (volume . "tshe")
		      (positions . ("21b2" "137a8")))
		     (peking
		      (string . "{Peking} 5738 ze 71a5–183a7")
		      (canon . peking)
		      
		      (number . 5738)
		      (volume . "ze")
		      (positions . ("71a5" "183a7")))
		     (sde-dge
		      (string . "sde dge 4239 zhe 51a3–151a6")
		      (canon . sde-dge)
		      
		      (number . 4239)
		      (volume . "zhe")
		      (positions . ("51a3" "151a6")))
		     (snar-thang
		      (string . "snar thang 3717 tshe 21b–131b")
		      (canon . snar-thang)
		      
		      (number . 3717)
		      (volume . "tshe")
		      (positions . ("21b" "131b")))
		     (snar-thang
		      (string . "snar thang 3770 ze 65b–186a")
		      (canon . snar-thang)
		      
		      (number . 3770)
		      (volume . "ze")
		      (positions . ("65b" "186a"))))))))
    (dolist (c cases)
      (should
       (equal
	(east-biblatex-split-canon-string (car c))
	(cdr c)))
      cases)))

;; (ert "test-east-biblatex-split-canon-string")

(defun east-biblatex-is-canonical-ref-p (bib)
  "Return nil unless BIB (a biblatex entry parsed by ) is a reference to a Tibetan canonical resource.

Looks at the langid or language field, checks keywords field for
“canonical”, and then checks and returns the series field."
  (and
   (east-biblatex-alist-get "keywords" bib)
   (or (east-biblatex-alist-get "langid" bib)
       (east-biblatex-alist-get "language" bib))
   (string-match-p ;; (rx-to-string '(and bow "bo" eow))
    "\\(?:\\<bo\\>\\)"
    (or
     (east-biblatex-alist-get "langid" bib)
     (east-biblatex-alist-get "language" bib)))
   ;; (rx-to-string '(and bow "canonical" eow))   
   (string-match-p "\\(?:\\<canonical\\>\\)" (east-biblatex-alist-get "keywords" bib))
   (east-biblatex-alist-get "series" bib)))

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

(defun east-biblatex-bibs-to-structured-data (bibs &optional sort-by complain)
  "Convert BIBS to a structured set of data.

If SORT-BY is nil, return result in the same order as it was
passed in.  If non-nil, SORT-BY should be a function that can be
called on the parsed result set as an argument to ‘sort’ (must
take two arguments and return t or f depending on which should
come first).

If COMPLAIN is non-nil, raise warnings about expected but absent data."
  (let (results)
    (mapc
     (lambda (bib)
       (let* ((bib-parsed (with-temp-buffer
			    (insert (nth 2 bib))
			    (goto-char (point-min))
			    (mapcar
			     (lambda (x)
			       (cons
				(car x)
				(string-join (split-string (string-trim (cdr x) "{" "}")) " ")))
			     (bibtex-parse-entry)))))
	 ;; List checks and consequences here
	 (when (and (member '("language" . "bo") bib-parsed)
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
    ;; Sort the fields in each bibliographic record
    (setq results
	  (mapcar
	   (lambda (bib)
	     (sort bib (lambda (x y) (string-lessp (car x) (car y)))))
	   results))
    ;; Return the list of items in the order requested, or in the
    ;; order they were given.
    (cond
     ((functionp sort-by)
      (sort results (lambda (x y) (apply sort-by (list x y)))))
     (t (nreverse results)))))

(defun east-biblatex-sort-peking (a b)
  (let ((cnum-a
	 (east-biblatex-alist-get '("series:analyzed" peking number) a))
	(cnum-b
	 (east-biblatex-alist-get '("series:analyzed" peking number) b)))
    (if (and cnum-a cnum-b)
	(string-lessp cnum-a cnum-b)
      (warn "Unexpected absence (or format) of number in %s vs. %s"
	    (or (east-biblatex-alist-get '("series:analyzed" peking string) a)
		(east-biblatex-alist-get "=key=" a))
	    (or (east-biblatex-alist-get '("series:analyzed" peking string) b)
		(east-biblatex-alist-get "=key=" b)))
      (string-lessp
       (east-biblatex-alist-get '("series:analyzed" peking string) a)
       (east-biblatex-alist-get '("series:analyzed" peking string) b)))))

;; (with-current-buffer (get-buffer "east.bib")
;;   (east-biblatex-bibs-to-structured-data
;;    (east-biblatex-find-tib-canon (current-buffer))
;;    #'east-biblatex-sort-peking))

;; (east-biblatex-tibetan-peking-ref-is-set-p
;;  '(("=key=" . "east:5005")
;;    ("=type=" . "Book")
;;    ("date" . "{1100}")
;;    ("east:url" . "https://east.ikga.oeaw.ac.at/bib/5005")
;;    ("keywords" . "{canonical}")
;;    ("language" . "{bo}")
;;    ("series" . "{snar thang 3692 ce 13a–96b; {Peking} 5702 ce 93b4–177a7}")
;;    ("series:analyzed"
;;     (co-ne)
;;     (peking "{Peking} 5702 ce 93b4–177a7"
;; 	    (peking
;; 	     (number . 5702)
;; 	     (volume . "ce")
;; 	     (positions "93b4" . "177a7")))
;;     (sde-dge)
;;     (snar-thang "snar thang 3692 ce 13a–96b"
;; 		(snar-thang
;; 		 (number . 3692)
;; 		 (volume . "ce")
;; 		 (positions "13a" . "96b"))))
;;    ("title" . "{tshad ma kun las btus pa'i 'grel pa}")
;;    ("translator" . "{{Kanakavarman (gser gyi go cha)} and {(mar thung) dad pa('i) shes rab}}")))




(defun east-biblatex-bibs-canonical-to-org-table (bibs &optional sort complain interactive?)
  "Convert canonical entries BIBS to an org-mode table.

BIBS should be in the format returned by ‘east-biblatex-find-tib-canon’."
  (interactive
   (list
    (east-biblatex-find-tib-canon (current-buffer))
    nil
    nil
    'interactive))
  (let ((table '(hline
		 ("Status" "Title" "co ne" "Peking" "sde dge" "snar thang" "others...")))
	(bibs-structured (east-biblatex-bibs-to-structured-data bibs sort complain))
	footnotes)
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
	   (setq row
		 `(,@row
		   ,(let ((title (east-biblatex-normalize-space (east-biblatex-alist-get "title" bib-parsed))))
		      (push (format "[fn:%s] @@html:<form method=\"post\" action=\"https://www.istb.univie.ac.at/kanjur/rktsneu/etanjur/verif.php?p=1\"><input type=\"hidden\" name=\"coll\" value=\"derge\"/><input type=\"hidden\" name=\"nom\" value=\"%s\"/><input type=\"submit\" value=\"Search RKTS for “%s” (normalized)\"/></form>@@"
				    (length table)
				    (string-join (split-string title "’") "'")
				    title)
			    footnotes)
		      (format "[[%s][%s]][fn:%s]"
			      (east-biblatex-alist-get "east:url" bib-parsed)
			      title
			      (length table)))))
	   ;; The refs themselves
           (setq row `(,@row
		       ,@(mapcar
			  (lambda (canon-ref)
			    (let ((full-text (east-biblatex-alist-get 'string canon-ref))
				  (canon (car canon-ref))
				  (number (east-biblatex-alist-get 'number canon-ref)))
			      (cond
			       ((and (eq canon 'sde-dge)
				     number)
				(format "[[https://web1.otani.ac.jp/cri/twrpe/peking/tibet.php?re_num=-1&page=0&key=derge&word=%s][%s]]"
					number
					full-text))
			       (t full-text))))
			  canon-parsed)))))
	 
	 (push row table)))
     bibs-structured)
    (setf table (nreverse table))
    (with-current-buffer (get-buffer-create "* east bib table *")
      (erase-buffer)
      (insert
       (orgtbl-to-orgtbl
	table
	(list :backend 'org)))
      (goto-char (point-max))
      (insert "\n\n* Footnotes")
      (mapc
       (lambda (fn)
	 (insert "\n\n")
	 (insert fn))
       footnotes)
      (goto-char (point-min))
      (org-table-align)
      (set-buffer-modified-p nil)
      (when interactive?
	(unless (eq major-mode 'org-mode)
	  (org-mode))
	(pop-to-buffer (current-buffer)))
      (current-buffer))))

(provide 'east-biblatex)
