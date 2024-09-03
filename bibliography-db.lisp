; Simple bibliography and projects database
; mjn, 2017-2022

(ql:quickload :str)

(defvar *publications* '()
  "Publications in the database. Each is a possibly multivalued plist
  containing BibTeX-style fields plus the pseudo-fields :citation-key,
  :publication-type, :category, and optionally one or more :link.")

(defvar *venues* '()
  "Recurring Publication venues (journals, workshops, and conferences) that we
  know about. Alist of (symbolic-name . string-name).")

(defvar *categories* '()
  "Paper categories. Alist of (symbolic-name . string-name).")

(defvar *projects* '()
  "Projects. Plist with keys: name, blurb, years, people, funding, papers.")

(defun defpub (citation-key publication-type &rest bibliographic-data &key &allow-other-keys)
  "Define a new publication. publication-type is one of: conference, workshop,
  demo, collection, journal, preprint, book. citation-key is a BibTeX-style
  citation key. The rest of the arguments are keyword parameters specifying the
  bibliographic data, such as :author, :title, etc., named as in BibTeX. In
  addition, there should be a :category and optionally one or more
  :link (text . url)."
  (push (list* :citation-key citation-key :publication-type publication-type bibliographic-data) *publications*))

(defun defvenue (symbolic-name string-name)
  "Define a new publication venue. The symbolic name can subsequently be used
  as the argument to :booktitle or :journal in place of the full name.
  For conferences, string-name should be the base name of the conference, such
  as 'IEEE Conference on Computational Intelligence and Games', and will be
  appropriately expanded."
  (push (cons symbolic-name string-name) *venues*))

(defun defcat (symbolic-name string-name)
  (push (cons symbolic-name string-name) *categories*))

(defun defproject (name &rest project-data &key &allow-other-keys)
  (push (list* :name name project-data) *projects*))

; utility functions

(defun publication-venue (publication)
  (let* ((venue (case (getf publication :publication-type)
                 ((conference workshop demo collection) (getf publication :booktitle))
                 ((journal preprint) (getf publication :journal)))))
    (if (symbolp venue)
      (cdr (assoc venue *venues*))
      venue)))

(defun publication-full-venue (publication)
  (let ((venue (publication-venue publication)))
    (case (getf publication :publication-type)
      ((conference workshop demo) (str:concat "Proceedings of the " venue))
      (t venue))))

(defun publication-authors (publication)
  (str:join ", " (getf publication :author)))

(defun getf-all (plist key)
  (loop for (k v) on plist by #'cddr
        if (string= k key)
        collect v))
