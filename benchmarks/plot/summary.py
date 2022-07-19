import csv
import os
from math import log10, inf
from enum import Enum
import numpy as np
from pprint import pprint

class Kind(Enum):
    Acc = 'accuracy.csv'
    Mem = 'mem-ideal.csv'
    Perf = 'perf.csv'
    Per_particle = 'per-particles.csv'
    Per_step = 'per-step.csv'
    Per_step_mem = 'per-step-mem.csv'
    Perf_step = 'perf-step.csv'

class Algo(Enum):
    # DS = 'ds_nogc'
    # BDS = 'ds_bounded'
    SDS = 'ds'
    PF = 'particles'
    SSDS = 'semi_symb'

class Example(Enum):
    BetaBernoulli = 'coin'
    Gaussian = 'gaussian'
    Kalman = 'kalman'
    Outlier = 'outlier'
    Robot = 'tracker'
    Slam = 'slam'
    Mtt = 'mtt'
    Gtree = 'gtree'
    Wheels = 'wheels'
    DelayedGPS = 'trackerdelay'

class Data:
    def __init__(self, example, algo, kind):
        self.filename = os.path.join('..', example.value, algo.value, kind.value)
        self.file = None

    def __enter__(self):
        self.file = open(self.filename, 'r')
        header = [h.strip() for h in self.file.readline().split(',')]
        return csv.DictReader(self.file, fieldnames=header, quoting=csv.QUOTE_NONNUMERIC)

    def __exit__(self, exc_type, exc_value, exc_traceback):
        self.file.close()


def get_baseline(example, algo=Algo.SDS, particles=500):
    with Data(example, algo, Kind.Acc) as accuracy, Data(example, algo, Kind.Perf) as perf:
        acc = next(row['median'] for row in accuracy if row['particles'] == particles)
        perf = next(row['median'] for row in perf if row['particles'] == particles)
        return acc, perf


def get_crossing_point(example, algo, baseline={}):
    b_acc, b_perf = get_baseline(example, **baseline)
    with Data(example, algo, Kind.Acc) as accuracy, Data(example, algo, Kind.Perf) as perf:
            cond = lambda x: abs(log10(x) - log10(b_acc)) <= 0.5
            particles = next(row['particles'] for row in accuracy if cond(row['upper quantile']))
            return next(
                (particles, row['time in ms lower quantile'], row['median'], row['upper quantile'])
                for row in perf if row['particles'] == particles)



def get_results(baseline={}):
    res = {}
    for example in Example:
        res[example.name] = {}
        for algo in Algo:
            try:
                (particles, lower, median, upper) = get_crossing_point(example, algo, baseline)
            except(StopIteration, IOError):
                (particles, lower, median, upper) = (inf, 0, 0, 0)
            res[example.name][algo.name] = {
                'particles': particles,
                'lower': lower,
                'median': median,
                'upper': upper }
    return res


def to_latex(baseline):
    exs = {'BetaBernoulli': 'Beta-Bernoulli',
           'Gaussian': 'Gaussian-Gaussian',
           'Kalman': 'Kalman-1D',
           'Outlier': 'Outlier',
           'Robot': 'Robot',
           'Slam': 'SLAM',
           'Slam-2000': 'SLAM-2000',
           'Slam-4000': 'SLAM-4000',
           'Mtt': 'MTT',
           'Mtt-2000': 'MTT-2000',
           'Mtt-4000': 'MTT-4000',
           'Gtree' : 'Tree',
           'Wheels': 'Wheels',
           'DelayedGPS': 'Delayed GPS', }

    results = get_results(baseline)
    print("model & PF particles & PF time & DS particles & DS time & SSI particles & SSI time")
    print("\\\\")
    for name, ex  in results.items():
        print(exs[name], end=" ")
        for algo in ["PF", "SDS", "SSDS"]:
            results = ex[algo]
            p = results['particles']
            if p < inf:
                if p == 1: fp = "\\exact"
                else: fp = f"{int(p):d}"
            else: fp = " "

            t = results['median']
            l = results['lower']
            u = results['upper']
            if t <= 0.0: ft = "\\timeout"
            else: ft = f"{t:.2f}(-{100*(t-l)/t:.1f}+{100*(u-t)/t:.1f})"
            print(f"& {fp} & {ft}", end=" ")
        print("\\\\")

to_latex({'algo': Algo.SDS, 'particles': 1000})
