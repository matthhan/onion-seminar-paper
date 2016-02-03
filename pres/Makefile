all: clean initial bib another onemore
	echo 'OK'
open:
	open pres.pdf
bib: references.bib pres.aux
	bibtex pres
clean:
	rm -f *.pdf *.log *.aux *.snm *.toc *.nav *.out
initial: pres.tex
	pdflatex pres.tex
another:
	pdflatex pres.tex
onemore:
	pdflatex pres.tex
