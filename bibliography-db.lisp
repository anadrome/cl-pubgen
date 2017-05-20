; Simple bibliography database
; mjn, 2017

(defvar *publications* '()
  "Publications in the database. Each is a possibly multivalued plist
  containing BibTeX-style fields plus the pseudo-fields :citation-key,
  :publication-type, :category, and optionally one or more :link.")

(defvar *venues* '()
  "Recurring Publication venues (journals, workshops, and conferences) that we
  know about. Alist of (symbolic-name . string-name).")

(defun defpub (citation-key publication-type &rest bibliographic-data &key &allow-other-keys)
  "Define a new publication. publication-type is a BibTeX publication type
  (e.g. 'inproceedings, 'article), and citation-key is a BibTeX-style citation
  key. The rest of the arguments are keyword parameters specifying the
  bibliographic data, such as :author, :title, etc., also named as in BibTeX.
  In addition, there should be a :category and optionally one or more
  :link (text . url)."
  (push (list* :citation-key citation-key :publication-type publication-type bibliographic-data) *publications*))

(defun defvenue (symbolic-name string-name)
  "Define a new publication venue. The symbolic name can subsequently be used
  as the argument to :booktitle or :journal in place of the full name.
  For conferences, string-name should be the base name of the conference, such
  as 'IEEE Conference on Computational Intelligence and Games', and will be
  appropriately expanded."
  (push (cons symbolic-name string-name) *venues*))
