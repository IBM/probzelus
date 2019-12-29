from datetime import datetime
import os

run_path="../../../_build/default/examples/benchmarks/gaussian/"
particle_exe = run_path + "particles/run.exe"
bds_exe = run_path + "ds_bounded/run.exe"
sds_exe = run_path + "ds/run.exe"

data_path = "data"

result_path="result/"
particle_dir = "particles/"
bds_dir = "ds_bounded/"
sds_dir = "ds/"
result_path_particles = result_path + particle_dir

num_trials = 500

def do_accuracy_run(num_particles, exe, fdir, subdir="tmp/"):
    acc_name = result_path + fdir + subdir + "acc" + str(num_particles) + ".csv"

    rand = os.urandom(12)
    seed = ""
    for i in range(0,12):
        seed += str(rand[i])
        if i != 11:
            seed += ","

    seed_name = result_path + fdir + subdir + "seed" + str(num_particles)
    f = open(seed_name, 'w')
    f.write(str(seed) + "\n")
    f.close()

    options = "-w 0 " + \
              "-min-particles " + str(num_particles) + " "\
              "-max-particles " + str(num_particles) + " "\
              "-num-runs " + str(num_trials) + " "\
              "-acc " + acc_name + " "\
              "-seed-long " + str(seed)
    cmd = "cat " + data_path + " | " + exe + " " + options
    print(cmd)
    os.system(cmd)


def do_accuracy_run_particles(num_particles, subdir="tmp/"):
    do_accuracy_run(num_particles, particle_exe, particle_dir, subdir)

def read_all(num_particles, fdir, subdir="tmp/"):
    import csv
    fname = result_path + fdir + subdir + "acc" + str(num_particles) + ".csv"
    f = open(fname, 'r')
    r = csv.reader(f)
    headers = r.__next__()
    values = r.__next__()
    particles, low, mid, high = int(values[0]), \
                                float(values[1]), \
                                float(values[2]), \
                                float(values[3])
    f.close()
    return (particles, low, mid, high)

def read_particles_all(num_particles, subdir="tmp/"):
    return read_all(num_particles, particle_dir, subdir)

def read_particles(num_particles, subdir="tmp/"):
    _, _, _, ret = read_particles_all(num_particles, subdir)
    return ret

def write_data(data, fn, fdir, subdir="tmp/"):
    fname = result_path + fdir + subdir + fn + ".csv"
    f = open(fname, 'w')
    for d in data:
        f.write(str(d[0]) + ", " + str(d[1]) + ", " + str(d[2]) + ", " + str(d[3]) + "\n")
    f.close()

def write_data_particles(data, fn, subdir="tmp/"):
    write_data(data, fn, particle_dir, subdir)

def write_data_bds(data, fn, subdir="tmp/"):
    write_data(data, fn, bds_dir, subdir)

def write_data_sds(data, fn, subdir="tmp/"):
    write_data(data, fn, sds_dir, subdir)
    
def read_seed(seed_num, fdir, subdir="tmp/"):
    fname = result_path + fdir + subdir + "seed" + str(seed_num)
    f = open(fname, 'r')
    return (f.read())

def read_seed_particles(seed_num, subdir="tmp/"):
    return read_seed(seed_num, particle_dir, subdir)

def write_seeds(seeds, fn, fdir, subdir="tmp/"):
    fname = result_path + fdir + subdir + fn
    f = open(fname, 'w')
    for seed in seeds:
        f.write(seed)

def write_seeds_particles(seeds, fn, subdir="tmp/"):
    write_seeds(seeds, fn, particle_dir, subdir)

def write_seeds_bds(seeds, fn, subdir="tmp/"):
    write_seeds(seeds, fn, bds_dir, subdir)

def write_seeds_sds(seeds, fn, subdir="tmp/"):
    write_seeds(seeds, fn, sds_dir, subdir)

def find_crossover(num_particles, exe, fdir):
    baseline = read_particles(num_particles, "baseline/")
    threshold = baseline * 1.1
    print("Basline: " + str(baseline) + ", Threshold:" + str(threshold))

    data = []
    seeds = []
    done = False
    cur_particles = 1
    while not done:
        do_accuracy_run(cur_particles, exe, fdir)
        parts, low, mid, high = read_all(cur_particles, fdir)
        cur_accuracy = high
        data += [(parts, low, mid, high)]
        seeds += [read_seed(cur_particles, fdir)]
        print(str(cur_particles) + ": " + str(cur_accuracy))
        if cur_accuracy < threshold:
            done = True
            print("Found crossover: " + str(cur_particles))
        else:
            next_particles = 1.1 * cur_particles
            if int(next_particles) == cur_particles:
                cur_particles = cur_particles + 1
            else:
                cur_particles = int(next_particles)

    return data, seeds

def find_crossover_particles(num_particles):
    return find_crossover(num_particles, particle_exe, particle_dir)

def find_crossover_bds(num_particles):
    return find_crossover(num_particles, bds_exe, bds_dir)

def find_crossover_sds(num_particles):
    return find_crossover(num_particles, sds_exe, sds_dir)

def run_experiment_particles(baseline_particles):
    data, seeds = find_crossover_particles(baseline_particles)
    write_data_particles(data, "acc" + str(baseline_particles), "")
    write_seeds_particles(seeds, "seeds" + str(baseline_particles), "")

def run_experiment_bds(baseline_particles):
    data, seeds = find_crossover_bds(baseline_particles)
    write_data_bds(data, "acc" + str(baseline_particles), "")
    write_seeds_bds(seeds, "seeds" + str(baseline_particles), "")

def run_experiment_sds(baseline_particles):
    data, seeds = find_crossover_sds(baseline_particles)
    write_data_sds(data, "acc" + str(baseline_particles), "")
    write_seeds_sds(seeds, "seeds" + str(baseline_particles), "")

do_accuracy_run_particles(100, "baseline/")
run_experiment_particles(100)
run_experiment_bds(100)
run_experiment_sds(100)







