.PHONY : all clean

SS   := $(wildcard *.ss)
CPP := $(SS:%.ss=%.cpp)

all: $(CPP)

clean:
	rm *.cpp

%.cpp: %.ss
	seac < $< > $@

