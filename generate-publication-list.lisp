; Generate a publication list from the publications in *publications* (see bib.lisp)
; mjn, 2017

; Note: Like bibtex2web, does some magic with other files found in the output
; directory named from the basename of the citation key (see function
; publication-basename). Currently uses this to link a paper PDF and to show an
; illustrative PNG/JPG image on the abstract page (in both cases, if present).

(defvar *author-name* "Mark J. Nelson")
(defvar *output-directory* '(:relative "publications"))

(ql:quickload "spinneret")
(ql:quickload "spinneret/cl-markdown")
(use-package :spinneret)

(defmacro with-page-output ((&key filename title additional-headers) &body body)
  `(with-open-file (*html* (make-pathname :directory *output-directory* :name ,filename)
                           :direction :output :if-exists :supersede)
     (with-html
       (:doctype)
       (:html
         (:head
           (:meta :name "viewport" :content "width=device-width, initial-scale=1")
           (:link :rel "stylesheet" :href "style.css")
           ,@additional-headers
           (:title (concatenate 'string ,title " | " *author-name*)))
         (:body
           (:h1 ,title)
           ,@body)))))

(defun generate-publication-list ()
  (setq *publications* (stable-sort *publications* #'> :key (lambda (x) (getf x :year))))
  (with-page-output (:filename "index.html" :title "Publications")
    (let ((last-year))
      (dolist (publication *publications*)
        (let* ((abstract-filename (concatenate 'string (publication-basename publication) "-abstract.html"))
               (pdf (auxiliary-file publication "pdf"))
               (image (or (auxiliary-file publication "png") (auxiliary-file publication "jpg")))
               (publication-type (getf publication :publication-type))
               (authors (publication-authors publication))
               (title (getf publication :title))
               (venue (publication-venue publication))
               (volume (getf publication :volume))
               (number (getf publication :number))
               (year (getf publication :year))
               (pages (getf publication :pages)))

          ; publication's ToC entry
          (unless (eql year last-year)
            (setq last-year year)
            (:h2 year))
          (:p ; ToC citation format
            authors (:br)
            (:strong (:a :href abstract-filename title) (:br))
            (:em venue))

          ; publication's abstract page
          (let ((biblio-tags (biblio-tags publication))
                (full-venue (publication-full-venue publication))
                (links (getf-all publication :link)))
            (with-page-output (:filename abstract-filename :title title
                               :additional-headers ((dolist (biblio-tag biblio-tags)
                                                      (:meta :name (car biblio-tag) :content (cdr biblio-tag)))))
              (:p ; 'normal' citation format
                ("~a (~a). " authors year)
                (if pdf
                  (:a :href pdf (:strong ("~a." title)))
                  (:strong ("~a." title)))
                (case publication-type ((inproceedings incollection) "In"))
                ("*~a*~a~a."
                 full-venue
                 (format nil "~@[ ~a~]~@[(~a)~]" volume number)
                 (format nil "~@[, pp. ~a~]" pages)))
              (if image
                (:p (:img :src image :class "abstract-image")))
              (:h2 "Abstract")
              (:p (:raw (markdown (getf publication :abstract))))
              (when links
                (:h2 "Supplemental links")
                (:ul
                  (dolist (link links)
                    (:li (:a :href (car link) (cdr link))))))
              (:hr)
              (:p ("Back to [publications](./).")))))))
    (:hr)
    (:p (:em (:a :href "../" *author-name*)))))


(defun publication-venue (publication)
  (let* ((venue (ccase (getf publication :publication-type)
                 ((inproceedings incollection) (getf publication :booktitle))
                 (article (getf publication :journal)))))
    (if (symbolp venue)
      (cdr (assoc venue *venues*))
      venue)))

(defun publication-full-venue (publication)
  (let ((venue (publication-venue publication)))
    (if (eq (getf publication :publication-type) 'inproceedings)
      (concatenate 'string "Proceedings of the " venue)
      venue)))

(defun publication-authors (publication)
  (format nil "~{~a~^, ~}" (getf publication :author)))

(defun publication-basename (publication)
  ; follow bibtex2web's style
  (substitute #\_ #\/ (substitute #\_ #\: (getf publication :citation-key))))

(defun auxiliary-file (publication extension)
  (let ((filename (concatenate 'string (publication-basename publication) "." extension)))
    (if (probe-file (make-pathname :directory *output-directory* :name filename))
      filename)))

(defun biblio-tags (publication)
  "Bibliographic info that Google Scholar wants in meta tags."
  (let ((tags
          `(("citation_title" . ,(getf publication :title))
            ,@(mapcar (lambda (x) `("citation_author" . ,x)) (getf publication :author))
            ("citation_publication_date" . ,(getf publication :year))
            ,@(let ((full-venue (publication-full-venue publication)))
                (ccase (getf publication :publication-type)
                  (article `(("citation_journal_title" . ,full-venue)
                             ("citation_volume" . ,(getf publication :volume))
                             ("citation_issue" . ,(getf publication :number))))
                  (inproceedings `(("citation_conference_title" . ,full-venue)))
                  (incollection `(("citation_inbook_title" . ,full-venue)))))
            ("citation_pdf_url" . ,(auxiliary-file publication "pdf")))))
    (remove nil tags :key #'cdr))) ; omit any missing fields

(defun markdown (string)
  (nth-value 1 (cl-markdown:markdown string :stream nil)))

(defun getf-all (plist key)
  (loop for (k v) on plist by #'cddr
        if (string= k key)
        collect v))
