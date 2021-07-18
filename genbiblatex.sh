#!/bin/sh
sbcl --load "export-biblatex.lisp" --eval "(export-biblatex)" --eval "(quit)"
