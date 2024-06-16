;;; -*- lexical-binding: t; -*-

(require 'svg)

(defun album-get-data ()
  (interactive)
  (let ((dom (with-temp-buffer
	       (insert (yank-media--get-selection 'text/html))
	       (libxml-parse-html-region (point-min) (point-max))))
	album cover year)
      (cl-loop for elem in (dom-by-class dom "textWithCoversRow")
	       for link = (dom-by-tag elem 'a)
	       for label = (dom-attr link 'aria-label)
	       when (and label (string-match "Navigate to \\(.*\\)" label))
	       do (setq album (match-string 1 label)
			cover (dom-attr (dom-by-tag link 'img) 'src))
	       when (dom-by-class elem "year_")
	       do (setq year (dom-text (dom-by-class elem "year_")))
	       (insert (format " ;%s;%s;%s\n" year album cover)))))


(defun album-download-images ()
  (with-temp-buffer
    (insert-file-contents "albums.txt")
    (while (re-search-forward "https://.*" nil t)
      (let* ((url (match-string 0))
	     (file (expand-file-name (concat (sha1 url) ".jpeg")
				     "img/")))
	(unless (file-exists-p file)
	  (with-current-buffer (url-retrieve-synchronously url)
	    (goto-char (point-min))
	    (re-search-forward "\n\n")
	    (write-region (point) (point-max) file)
	    (kill-buffer (current-buffer))))))))

(defun album-create-svg ()
  (album-download-images)
  (let* ((width 2800)
	 (height 4000)
	 (svg (svg-create width height))
	 (artnum 0)
	 (left-space 200))
    (svg-rectangle svg 0 0 width height
		   :fill "#101010")
    (cl-loop for x from 20 upto 74 by 5
	     for xp = (+ left-space (* (/ (- x 10) 65.0) (- width left-space)))
	     do (svg-line svg xp 200 xp (- height 80)
			  :stroke "#e0e0e0")
	     (svg-text svg (format "%d" x)
		       :font-family "futura"
		       :text-anchor "middle"
		       :font-size 40
		       :fill "white"
		       :x xp
		       :y 160))
    (with-temp-buffer
      (insert-file-contents "albums.txt")
      (while (not (eobp))
	(when (looking-at "\\([0-9]+\\) \\(.*\\)")
	  (let ((birth (string-to-number (match-string 1)))
		(person (match-string 2))
		(yp (+ (* artnum 120) 200)))
	    (svg-text svg person
		      :font-family "futura"
		      :text-anchor "front"
		      :font-size 40
		      :fill "white"
		      :x 80
		      :y (+ yp 60))
	    (forward-line 1)
	    (while (looking-at "\\([ *]\\);\\([0-9]+\\);[^;]+;\\(.*\\)")
	      (let* ((year (string-to-number (match-string 2)))
		     (best (match-string 1))
		     (url (match-string 3))
		     (image (expand-file-name (concat (sha1 url) ".jpeg")
					      "img/"))
		     (xp (+ left-space 
			    (* (/ (- year birth 10) 65.0)
			       (- width left-space)))))
		(if (string= best " ")
		    (svg-embed svg image "image/jpeg" nil
			       :width 100
			       :height 100
			       :x xp
			       :y yp)
		  (svg-rectangle svg xp yp 100 100
				 :fill "#00e000")
		  (svg-embed svg image "image/jpeg" nil
			     :width 80
			     :height 80
			     :x (+ xp 10)
			     :y (+ yp 10))))
	      (forward-line 1))
	    (cl-incf artnum)))
	(forward-line 1)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (svg-print svg)
      (write-region (point-min) (point-max) "artists.svg"))))
