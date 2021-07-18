#!/bin/sh
sbcl --load "generate-publication-list.lisp" --eval "(generate-publication-list)" --eval "(quit)"
