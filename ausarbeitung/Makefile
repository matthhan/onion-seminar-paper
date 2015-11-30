all: clean initial bib another onemore
	echo 'OK'
bib: references.bib termpaper.aux
	bibtex termpaper
clean:
	rm -f termpaper.pdf termpaper.aux termpaper.log
initial: termpaper.tex
	pdflatex termpaper.tex
another:
	pdflatex termpaper.tex
onemore:
	pdflatex termpaper.tex
open:
	open termpaper.pdf
