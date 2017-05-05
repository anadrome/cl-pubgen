#!/bin/sh
ccl --load "bibliography-db.lisp" --load "generate-publication-list.lisp" --load "my-papers.lisp" --eval "(generate-publication-list)" --eval "(quit)"
