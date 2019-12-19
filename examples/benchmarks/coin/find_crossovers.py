from datetime import datetime
import os

run_path="../../../_build/default/examples/benchmarks/coin/"
particle_exe = run_path + "particles/run.exe"

data_path = "data"

result_path="result/"
result_path_particles=result_path + "particles/"

num_trials = 500

def do_accuracy_run_particles(num_particles):
    acc_name = result_path_particles + "acc" + str(num_particles) + ".csv"

    seed_name = result_path_particles + "seed" + str(num_particles)
    seed = datetime.now().microsecond
    f = open(seed_name, 'w')
    f.write(str(seed) + "\n")
    f.close()

    options = "-w 0 " + \
              "-min-particles " + str(num_particles) + " "\
              "-max-particles " + str(num_particles) + " "\
              "-num-runs " + str(num_trials) + " "\
              "-acc " + acc_name + " "\
              "-seed " + str(seed)
    cmd = "cat " + data_path + " | " + particle_exe + " " + options
    print(cmd)
    os.system(cmd)

def read_particles(num_particles):
    import csv
    fname = result_path_particles + "acc" + str(num_particles) + ".csv"
    f = open(fname, 'r')
    r = csv.reader(f)
    headers = r.__next__()
    values = r.__next__()
    ret = float(values[3])
    f.close()
    return ret


def find_crossover_particles(num_particles):
    baseline = read_particles(num_particles)
    threshold = baseline * 1.1
    print("Basline: " + str(baseline) + ", Threshold:" + str(threshold))

    done = False
    cur_particles = 1
    while not done:
        do_accuracy_run_particles(cur_particles)
        cur_accuracy = read_particles(cur_particles)
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


do_accuracy_run_particles(100)
find_crossover_particles(100)




