PROBZELUC=probzeluc

NUMS=1 2 3 4 5 6 7 8
SRCS=$(patsubst %, ex%_ds.zls,$(NUMS))
MLS=$(patsubst %, ex%_ds.ml,$(NUMS))
MAINS=$(patsubst %, main%,$(NUMS))
MAINS_ML=$(patsubst %, main%.ml,$(NUMS))
EXECS=$(patsubst %, main%.exe,$(NUMS))

all: build

$(MLS) $(MAINS): $(SRCS)
	$(foreach N,$(NUMS), \
		$(PROBZELUC) -s main$(N) ex$(N)_ds.zls || exit 1;)


clean:
	rm -f $(MLS) $(MAINS_ML)
	rm -f *.zci

## Generic rules

.phony: build exec exec_ds

exec: main1.ml
	dune exec ./main8.exe

build: $(MAINS)
	dune build $(EXECS)

%.zci: %.zli
	$(PROBZELUC) $<

cleanall: clean
	dune clean
	rm -f *~
