EX=coin

all:
	- $(MAKE) ex EX=coin
	- $(MAKE) ex EX=gaussian
	- $(MAKE) ex EX=gtree
	- $(MAKE) ex EX=kalman
	- $(MAKE) ex EX=outlier
	- $(MAKE) ex EX=tracker
	- $(MAKE) ex EX=slam
	- $(MAKE) ex EX=mtt

ex: $(EX)-accuracy.png $(EX)-perf.png $(EX)-perf-step.png $(EX)-mem-ideal.png


$(EX)-accuracy.png: ../$(EX)/particles/accuracy.csv ../$(EX)/ds/accuracy.csv ../$(EX)/semi_symb/accuracy.csv
	gnuplot -e "ex='$(EX)'" plot-accuracy.gp

$(EX)-perf.png: ../$(EX)/particles/perf.csv ../$(EX)/ds/perf.csv ../$(EX)/semi_symb/perf.csv
	gnuplot -e "ex='$(EX)'" plot-perf.gp

$(EX)-perf-step.png: ../$(EX)/particles/perf-step.csv ../$(EX)/ds/perf-step.csv ../$(EX)/semi_symb/perf-step.csv
	gnuplot -e "ex='$(EX)'" plot-perf-step.gp

$(EX)-mem-ideal.png: ../$(EX)/particles/mem-ideal.csv ../$(EX)/ds/mem-ideal.csv ../$(EX)/semi_symb/mem-ideal.csv
	gnuplot -e "ex='$(EX)'" plot-mem-ideal.gp


clean:

cleanall: clean
	rm -f *.png *~
