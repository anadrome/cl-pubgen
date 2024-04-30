; Export publications to biblatex
; mjn, 2017-2021

(load "configuration.lisp")
(load "bibliography-db.lisp")
(load "my-papers.lisp")

(ql:quickload :str)

(defun export-biblatex ()
  (with-open-file (out *biblatex-output-file* :direction :output :if-exists :supersede)
    (dolist (publication (reverse *publications*))
      (princ
        (biblatex-entry
          ; pass through citation key
          (getf publication :citation-key)
          ; map our publication-types to biblatex ones
          (ecase (getf publication :publication-type)
            ((conference workshop demo) 'inproceedings)
            (collection 'incollection)
            ((journal preprint) 'article)
            (book 'book))
          ; filter relevant fields to pass through to biblatex, and format a few of them (e.g. "and" between authors)
          (let ((author (str:join " and " (getf publication :author))))
            (ecase (getf publication :publication-type)
              ((journal preprint)
               (append
                 (list :author author)
                 (filter-plist publication '(:title :journal :volume :number :pages :year :doi :note))))
            ((conference workshop demo collection) 
             (append
               (list :author author :booktitle (publication-full-venue publication))
               (filter-plist publication '(:title :pages :year :publisher :doi :note))))
            ((book)
             (append
               (list :author author)
               (filter-plist publication '(:title :year :publisher :isbn :note)))))))
        out))))

(defun biblatex-entry (citation-key publication-type fields)
  (with-output-to-string (entry)
    (format entry "@~a{~a,~%" publication-type citation-key)
    (loop for (k v) on fields by #'cddr
          do (format entry "  ~a = {~a},~%" k v))
    (format entry "}~%")))

(defun filter-plist (plist keep-keys)
  (loop for (k v) on plist by #'cddr
        if (member k keep-keys)
        append (list k v)))
