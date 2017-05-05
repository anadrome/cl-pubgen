; Generate a publication list from the publications in *publications* (see bib.lisp)
; mjn, 2017

; Note: Expects to find PDFs named in a way derived from the citation-key a la
; bibtex2web.

(defvar *author-name* "Mark J. Nelson")

(ql:quickload "spinneret")
(ql:quickload "spinneret/cl-markdown")
(use-package :spinneret)
;(ql:quickload "multival-plist")
;(use-package :multival-plist)
(ql:quickload "cl-markdown")

(defmacro with-page-output (filename title &body body)
  `(with-open-file (*html* (make-pathname :directory '(:relative "publications") :name ,filename)
                           :direction :output :if-exists :supersede)
     (with-html
       (:doctype)
       (:html
         (:head
           (:meta :name "viewport" :content "width=device-width, initial-scale=1")
           (:link :rel "stylesheet" :href "style.css")
           (:title (concatenate 'string ,title " | " *author-name*)))
         (:body
           (:h1 ,title)
           ,@body)))))

(defun generate-publication-list ()
  (setq *publications* (stable-sort *publications* #'> :key (lambda (x) (getf x :year))))
  (with-page-output "index.html" "Publications"
    (let ((last-year))
      (dolist (publication *publications*)
        (let* ((basename (citation-key-to-basename (getf publication :citation-key)))
               (abstract-url (concatenate 'string basename "-abstract.html"))
               (pdf-url (concatenate 'string basename ".pdf"))
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
            ("~a." authors) (:br)
              (:a :href abstract-url ("~a." title)) (:br)
              (:em ("~a." venue)))

          ; publication's abstract page
          (with-page-output abstract-url title
            (let ((full-venue (if (eq publication-type 'inproceedings)
                                (concatenate 'string "Proceedings of the " venue)
                                venue)))
              (:p ; 'normal' citation format
                ("~a (~a). [~a](~a)." authors year pdf-url title)
                (case publication-type ((inproceedings incollection) "In"))
                ("*~a*~a~a."
                 full-venue
                 (format nil "~@[ ~a~]~@[(~a)~]" volume number)
                 (format nil "~@[, pp. ~a~]" pages))))
            ; TODO: generate :link's
            (:h2 "Abstract")
            (:p (:raw (markdown (getf publication :abstract))))
            (:hr)
            (:p ("Back to [publications](./)."))))))))


(defun publication-venue (publication)
  (let* ((venue (ccase (getf publication :publication-type)
                 ((inproceedings incollection) (getf publication :booktitle))
                 (article (getf publication :journal)))))
    (if (symbolp venue)
      (cdr (assoc venue *venues*))
      venue)))

(defun publication-authors (publication)
  (format nil "~{~a~^, ~}" (getf publication :author)))

(defun citation-key-to-basename (citation-key)
  ; follow bibtex2web's style
  (substitute #\_ #\/ (substitute #\_ #\: citation-key)))

(defun markdown (string)
  (nth-value 1 (cl-markdown:markdown string :stream nil)))
