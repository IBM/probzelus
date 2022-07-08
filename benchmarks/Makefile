all:
	$(MAKE) -C harness
	$(MAKE) -C tools
	$(MAKE) -C coin
	$(MAKE) -C gaussian
	$(MAKE) -C gtree
	$(MAKE) -C kalman
	$(MAKE) -C outlier
	$(MAKE) -C slam
	$(MAKE) -C tracker
	$(MAKE) -C trackerdelay
	$(MAKE) -C mtt
	$(MAKE) -C wheels

bench: bench-coin bench-gaussian bench-gtree bench-kalman bench-outlier bench-slam bench-tracker bench-trackerdelay bench-wheels bench-mtt
bench-coin:
	$(MAKE) -C coin bench
bench-gaussian:
	$(MAKE) -C gaussian bench
bench-gtree:
	$(MAKE) -C gtree bench
bench-kalman:
	$(MAKE) -C kalman bench
bench-outlier:
	$(MAKE) -C outlier bench
bench-slam:
	$(MAKE) -C slam bench
bench-tracker:
	$(MAKE) -C tracker bench
bench-trackerdelay:
	$(MAKE) -C trackerdelay bench
bench-mtt:
	$(MAKE) -C mtt bench
bench-wheels:
	$(MAKE) -C wheels bench


bench_find_mse:
	$(MAKE) -C coin bench_find_mse
	$(MAKE) -C gaussian bench_find_mse
	$(MAKE) -C gtree bench_find_mse
	$(MAKE) -C kalman bench_find_mse
	$(MAKE) -C outlier bench_find_mse
	$(MAKE) -C slam bench_find_mse
	$(MAKE) -C tracker bench_find_mse
	$(MAKE) -C trackerdelay bench_find_mse
	$(MAKE) -C mtt bench_find_mse
	$(MAKE) -C wheels bench_find_mse

bench_per_particles:
	$(MAKE) -C coin bench_per_particles
	$(MAKE) -C gaussian bench_per_particles
	$(MAKE) -C gtree bench_per_particles
	$(MAKE) -C kalman bench_per_particles
	$(MAKE) -C outlier bench_per_particles
	$(MAKE) -C slam bench_per_particles
	$(MAKE) -C tracker bench_per_particles
	$(MAKE) -C trackerdelay bench_per_particles
	$(MAKE) -C mtt bench_per_particles
	$(MAKE) -C wheels bench_per_particles

bench_per_steps:
	$(MAKE) -C coin bench_per_steps
	$(MAKE) -C gaussian bench_per_steps
	$(MAKE) -C gtree bench_per_steps
	$(MAKE) -C kalman bench_per_steps
	$(MAKE) -C outlier bench_per_steps
	$(MAKE) -C slam bench_per_steps
	$(MAKE) -C tracker bench_per_steps
	$(MAKE) -C trackerdelay bench_per_steps
	$(MAKE) -C mtt bench_per_steps
	$(MAKE) -C wheels bench_per_steps

bench_mem_ideal:
	$(MAKE) -C coin bench_mem_ideal
	$(MAKE) -C gaussian bench_mem_ideal
	$(MAKE) -C gtree bench_mem_ideal
	$(MAKE) -C kalman bench_mem_ideal
	$(MAKE) -C outlier bench_mem_ideal
	$(MAKE) -C slam bench_mem_ideal
	$(MAKE) -C tracker bench_mem_ideal
	$(MAKE) -C trackerdelay bench_mem_ideal
	$(MAKE) -C mtt bench_mem_ideal
	$(MAKE) -C wheels bench_mem_ideal

analyze:
	$(MAKE) -C coin analyze
	$(MAKE) -C gaussian analyze
	$(MAKE) -C gtree analyze
	$(MAKE) -C kalman analyze
	$(MAKE) -C outlier analyze
	$(MAKE) -C slam analyze
	$(MAKE) -C tracker analyze
	$(MAKE) -C trackerdelay analyze
	$(MAKE) -C mtt analyze
	$(MAKE) -C wheels analyze

publish:
	$(MAKE) -C coin publish
	$(MAKE) -C gaussian publish
	$(MAKE) -C gtree publish
	$(MAKE) -C kalman publish
	$(MAKE) -C outlier publish
	$(MAKE) -C slam publish
	$(MAKE) -C tracker publish
	$(MAKE) -C trackerdelay publish
	$(MAKE) -C mtt publish
	$(MAKE) -C wheels publish

clean:
	$(MAKE) -C harness clean
	$(MAKE) -C tools clean
	$(MAKE) -C coin clean
	$(MAKE) -C gaussian clean
	$(MAKE) -C gtree clean
	$(MAKE) -C kalman clean
	$(MAKE) -C outlier clean
	$(MAKE) -C slam clean
	$(MAKE) -C tracker clean
	$(MAKE) -C trackerdelay clean
	$(MAKE) -C mtt clean
	$(MAKE) -C wheels clean

realclean cleanall:
	-rm -f *~
	$(MAKE) -C harness cleanall
	$(MAKE) -C tools cleanall
	$(MAKE) -C coin cleanall
	$(MAKE) -C gaussian cleanall
	$(MAKE) -C gtree cleanall
	$(MAKE) -C kalman cleanall
	$(MAKE) -C outlier cleanall
	$(MAKE) -C slam cleanall
	$(MAKE) -C tracker cleanall
	$(MAKE) -C trackerdelay cleanall
	$(MAKE) -C mtt cleanall
	$(MAKE) -C wheels cleanall

.PHONY: bench bench-coin bench-gaussian bench-gtree bench-kalman bench-outlier bench-slam bench-tracker bench-trackerdelay bench-mtt bench-wheels
