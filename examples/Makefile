.PHONY : all clean

SS   := $(wildcard *.ss)
CPP := $(SS:%.ss=%.cpp)

all: $(CPP)

clean:
	rm -r out

%.cpp: %.ss
	mkdir -p out
	seac < $< > out/$@

