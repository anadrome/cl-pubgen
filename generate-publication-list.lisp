; Generate a publication list
; mjn, 2017-2018

; Note: Like bibtex2web, does some magic with other files found in the output
; directory named from the basename of the citation key (see function
; html-basename). Currently uses this to link a paper PDF and to show an
; illustrative PNG/JPG image on the abstract page (in both cases, if present).

(defvar *author-name* "Mark J. Nelson")
(defvar *output-directory* '(:relative "publications"))

(ql:quickload :str)
(ql:quickload :spinneret)
(ql:quickload :spinneret/cl-markdown)
(use-package :spinneret)

(defmacro with-page-output ((&key filename title additional-headers) &body body)
  `(with-open-file (*html* (make-pathname :directory *output-directory* :name ,filename)
                           :direction :output :if-exists :supersede)
     (let ((*print-pretty* t))
       (with-html
         (:doctype)
         (:html
           (:head
             (:meta :name "viewport" :content "width=device-width, initial-scale=1")
             (:link :rel "stylesheet" :href "style.css")
             ,@additional-headers
             (:title (str:concat ,title " | " *author-name*)))
           (:body
             (:h1 ,title)
             ,@body))))))

(defun generate-toc ()
  (with-page-output (:filename "index.html" :title "Publications")
    (let ((last-year))
      (dolist (publication (stable-sort (copy-list *publications*) #'> :key (lambda (x) (getf x :year))))
        (destructuring-bind (&key title year publisher &allow-other-keys) publication
          (unless (eql year last-year)
            (setq last-year year)
            (:h2 year))
          (:p :class "toc-citation" ; ToC citation format
            (publication-authors publication) (:br)
            (:strong (:a :href (abstract-filename publication) title) (:br))
            (:em (or (publication-venue publication) publisher))))))
    (:hr)
    (:p (:em (:a :href "../" *author-name*)))))

(defun generate-abstract-pages ()
  (dolist (publication *publications*)
    (destructuring-bind (&key publication-type title volume number year publisher pages note &allow-other-keys) publication
      (let ((pdf (auxiliary-file publication "pdf"))
            (image (or (auxiliary-file publication "png") (auxiliary-file publication "jpg")))
            (links (getf-all publication :link)))
        (with-page-output (:filename (abstract-filename publication) :title title
                           :additional-headers ((dolist (biblio-tag (biblio-tags publication))
                                                  (:meta :name (car biblio-tag) :content (cdr biblio-tag)))))
          (:p :class "abstract-citation" ; 'normal' citation format
            ("~a (~a). " (publication-authors publication) year)
            (if pdf
              (:a :href pdf (:strong ("~a." title)))
              (:strong ("~a." title)))
            " "
            (case publication-type ((inproceedings incollection) "In"))
            (ccase publication-type
              ((inproceedings incollection article)
               ("*~a*~a~a."
                (publication-full-venue publication)
                (format nil "~@[ ~a~]~@[(~a)~]" volume number)
                (format nil "~@[, pp. ~a~]" pages)))
              ((book) ("~a." publisher)))
            note)
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

(defun generate-publication-list ()
  (generate-toc)
  (generate-abstract-pages))

(defun html-basename (publication)
  ; follow bibtex2web's style
  (substitute #\_ #\/ (substitute #\_ #\: (getf publication :citation-key))))

(defun abstract-filename (publication)
  (str:concat (html-basename publication) "-abstract.html"))

(defun auxiliary-file (publication extension)
  (let ((filename (str:concat (html-basename publication) "." extension)))
    (if (probe-file (make-pathname :directory *output-directory* :name filename))
      filename)))

(defun biblio-tags (publication)
  "Bibliographic info that Google Scholar wants in meta tags."
  (let ((tags
          `(("citation_title" . ,(getf publication :title))
            ,@(mapcar (lambda (x) `("citation_author" . ,x)) (getf publication :author))
            ("citation_publication_date" . ,(getf publication :year))
            ,@(let ((full-venue (publication-full-venue publication)))
                (case (getf publication :publication-type)
                  (article `(("citation_journal_title" . ,full-venue)
                             ("citation_volume" . ,(getf publication :volume))
                             ("citation_issue" . ,(getf publication :number))))
                  (inproceedings `(("citation_conference_title" . ,full-venue)))
                  (incollection `(("citation_inbook_title" . ,full-venue)))))
            ("citation_pdf_url" . ,(auxiliary-file publication "pdf")))))
    (remove nil tags :key #'cdr))) ; omit any missing fields

(defun markdown (string)
  (nth-value 1 (cl-markdown:markdown string :stream nil)))
