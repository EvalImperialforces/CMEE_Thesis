#!/usr/bin/env bash
# Author: Eva Linehan el1718@ic.ac.uk
# Script: Compile LaTeX document
# Date: April 2019

# get the filename without the extention for bibtex
path="$(dirname $1)"
file="$(basename $1)"
filename="${file//.tex/}"

# Files need to be in the correct working directory for Bibtex to work.
mv $path/$filename* .

# compile pdf, dont display standard out, but display standard error
pdflatex $file  >/dev/null
pdflatex $file  >/dev/null
bibtex $filename  >/dev/null
pdflatex $file  >/dev/null
pdflatex $file  >/dev/null

## Cleanup
rm *.aux
rm *.log
rm *.bbl
rm *.blg

# move files back into writeup and pdf to results
mv $filename.pdf ../Results/$filename.pdf
mv $filename.tex ../Writeup/$filename.tex