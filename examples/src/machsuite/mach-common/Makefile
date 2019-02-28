
BENCHMARKS=\
	../mach-gemm_ncubed \
	../mach-stencil2d

GLOBALPOLLY=http://gorgonzola.cs.cornell.edu:8000

.PHONY: run estimate all
#sw:\
	@( for b in $(BENCHMARKS); do cd $$b && zip -r - . | curl -F file='@-;filename=code.zip' -F software= 1 $(GLOBALPOLLY)/jobs; done )\

run:
	@( for b in $(BENCHMARKS); do cd $$b && zip -r - . | curl -F file='@-;filename=code.zip' $(GLOBALPOLLY)/jobs; done )

estimate:
	@( for b in $(BENCHMARKS); do cd $$b && zip -r - . | curl -F file='@-;filename=code.zip' -F estimate=1 $(GLOBALPOLLY)/jobs; done )

all: estimate run
