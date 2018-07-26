.PHONY : all clean

MD   := $(wildcard *.md)
HTML := $(MD:%.md=%.html)

all: $(HTML)

clean:
	rm *.html

%.html: %.md
	pandoc $^ -f markdown -t html -s --katex -o $@
