all:
	$(MAKE) -C harness
	$(MAKE) -C tools
	$(MAKE) -C coin
	$(MAKE) -C gaussian
	$(MAKE) -C kalman
	$(MAKE) -C outlier
	$(MAKE) -C slam
	$(MAKE) -C tracker

bench: bench-coin bench-gaussian bench-kalman bench-outlier bench-slam bench-tracker
bench-coin:
	$(MAKE) -C coin bench
bench-gaussian:
	$(MAKE) -C gaussian bench
bench-kalman:
	$(MAKE) -C kalman bench
bench-outlier:
	$(MAKE) -C outlier bench
bench-slam:
	$(MAKE) -C slam bench
bench-tracker:
	$(MAKE) -C tracker bench


bench_find_mse:
	$(MAKE) -C coin bench_find_mse
	$(MAKE) -C gaussian bench_find_mse
	$(MAKE) -C kalman bench_find_mse
	$(MAKE) -C outlier bench_find_mse
	$(MAKE) -C slam bench_find_mse
	$(MAKE) -C tracker bench_find_mses

bench_per_particles:
	$(MAKE) -C coin bench_per_particles
	$(MAKE) -C gaussian bench_per_particles
	$(MAKE) -C kalman bench_per_particles
	$(MAKE) -C outlier bench_per_particles
	$(MAKE) -C slam bench_per_particles
	$(MAKE) -C tracker bench_per_particles

bench_per_steps:
	$(MAKE) -C coin bench_per_steps
	$(MAKE) -C gaussian bench_per_steps
	$(MAKE) -C kalman bench_per_steps
	$(MAKE) -C outlier bench_per_steps
	$(MAKE) -C slam bench_per_steps
	$(MAKE) -C tracker bench_per_steps

bench_mem_ideal:
	$(MAKE) -C coin bench_mem_ideal
	$(MAKE) -C gaussian bench_mem_ideal
	$(MAKE) -C kalman bench_mem_ideal
	$(MAKE) -C outlier bench_mem_ideal
	$(MAKE) -C slam bench_mem_ideal
	$(MAKE) -C tracker bench_mem_ideal

analyze:
	$(MAKE) -C coin analyze
	$(MAKE) -C gaussian analyze
	$(MAKE) -C kalman analyze
	$(MAKE) -C outlier analyze
	$(MAKE) -C slam analyze
	$(MAKE) -C tracker analyze

publish:
	$(MAKE) -C coin publish
	$(MAKE) -C gaussian publish
	$(MAKE) -C kalman publish
	$(MAKE) -C outlier publish
	$(MAKE) -C slam publish
	$(MAKE) -C tracker publish

clean:
	$(MAKE) -C harness clean
	$(MAKE) -C tools clean
	$(MAKE) -C coin clean
	$(MAKE) -C gaussian clean
	$(MAKE) -C kalman clean
	$(MAKE) -C outlier clean
	$(MAKE) -C slam clean
	$(MAKE) -C tracker clean


realclean cleanall:
	-rm -f *~
	$(MAKE) -C harness cleanall
	$(MAKE) -C tools cleanall
	$(MAKE) -C coin cleanall
	$(MAKE) -C gaussian cleanall
	$(MAKE) -C kalman cleanall
	$(MAKE) -C outlier cleanall
	$(MAKE) -C slam cleanall
	$(MAKE) -C tracker cleanall

.PHONY: bench bench-coin bench-gaussian bench-kalman bench-outlier bench-slam bench-tracker

