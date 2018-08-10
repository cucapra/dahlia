.PHONY : all clean

SEA   := $(wildcard *.sea)
CPP := $(SEA:%.sea=%.cpp)

all: $(CPP)

clean:
	rm -r out

%.cpp: %.sea
	mkdir -p out
	seac < $< > out/$@

