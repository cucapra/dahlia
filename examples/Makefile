.PHONY : all clean

SEA   := $(wildcard *.sea)
CPP := $(SEA:%.sea=%.cpp)

all: $(CPP)

clean:
	rm -r out

out:
	mkdir -p out

%.cpp: %.sea | out
	seac $< > out/$@

