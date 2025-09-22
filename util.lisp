; Some utility functions
; mjn, 2017-2021

(ql:quickload :spinneret)
(ql:quickload :spinneret/cl-markdown)
(ql:quickload :str)

(defmacro with-page-output ((&key filename title subtitle directory additional-headers) &body body)
  `(with-open-file (spinneret:*html* (make-pathname :directory ,directory :name ,filename)
                                     :direction :output :if-exists :supersede)
     (let ((*print-pretty* t))
       (spinneret:with-html
         (:doctype)
         (:html
           (:head
             (:meta :name "viewport" :content "width=device-width, initial-scale=1")
             (:link :rel "stylesheet" :href "style.css")
             ,@additional-headers
             (if ,subtitle
               (:title (str:concat ,title " | " ,subtitle))
               (:title ,title)))
           (:body
             (:h1 ,title)
             ,@body))))))

(defun markdown (string)
  (nth-value 1 (cl-markdown:markdown string :stream nil)))
