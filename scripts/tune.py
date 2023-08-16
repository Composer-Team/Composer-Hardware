import time

import optuna as opt
import sys
import os
import json
import subprocess
from filelock import FileLock
import numpy as np


def remove_leading_zeroes(a: str) -> str:
    return a[a.find('1'):]


("cwd: " + os.getcwd())
# read in main method name
# remove the last character, which is a '$'
main_method = sys.argv[1][:-1]
# tunable parameter JSON file
# each parameter has a key name, an integer range [min, max], and a default value
# hw build dir
hw_build_dir = sys.argv[2]

# get executable name
executable = sys.argv[3]
ex_opts = executable.split(".")
ex_dir = ex_opts[0]
ex_name = ex_opts[1]
ex_opts = ex_opts[2:]
# get perf counters also in JSON format from CAnnotations.json

gen_dir = os.environ['COMPOSER_ROOT'] + "/Composer-Hardware/vsim/generated-src"
with open(gen_dir + "/tunables.json", 'r') as f:
    params = json.load(f)

with open(gen_dir + "/CAnnotations.json", 'r') as f:
    perf_counters = json.load(f)

all_perf_paths = [perf_counters[p]["paths"] for p in perf_counters.keys()]
# flatten list
all_perf_paths = [item for sublist in all_perf_paths for item in sublist]
# replace / with .
all_perf_paths = ["TOP." + p.replace('/', '.') for p in all_perf_paths]

subprocess.Popen(["rm", "-rf", "lock"]).wait()

def objective(trial: opt.Trial):
    settings = [(k, trial.suggest_int(k, params[k]['range'][0], params[k]['range'][1])) for k in params.keys()]
    # build the hardware
    assignments = ' '.join([f"-D{k}={v}" for k, v in settings])
    trial_runtime_dir = "trial_" + str(trial.number)
    trial_exec_dir = "trial_e_" + str(trial.number)
    trial_hw_dir = os.getcwd() + "/trial_hw_" + str(trial.number)
    subprocess.Popen(["mkdir", "-p", trial_runtime_dir]).wait()
    subprocess.Popen(["mkdir", "-p", trial_exec_dir]).wait()
    subprocess.Popen(["mkdir", "-p", trial_hw_dir]).wait()
    # run subprocess without blocking
    # build it in custom directory because it might take a long time and we don't want to block parallel building
    # link composer.v to the generated composer.v when building the runtime because that's much quicker (block for that)
    subprocess.Popen(['sbt', 'runMain ' + main_method + " --notune --target=" + trial_hw_dir + " " + assignments,
                      "-Dsbt.io.implicit.relative.glob.conversion=allow"],
                     cwd=hw_build_dir).wait()
    # build the runtime
    # use debug build because it makes much faster and the performance isn't much different
    # build the runtime
    FileLock("lock").acquire()
    os.remove(os.environ['COMPOSER_ROOT'] + "/Composer-Hardware/vsim/generated-src/composer.v")
    subprocess.Popen(["ln", "-s", trial_hw_dir + "/composer.v",
                      os.environ['COMPOSER_ROOT'] + "/Composer-Hardware/vsim/generated-src/composer.v"]).wait()

    subprocess.Popen(["cmake", os.environ['COMPOSER_ROOT'] + "/Composer-Runtime/", "-DTARGET=sim",
                      "-DCMAKE_BUILD_TYPE=Debug", "-DUSE_DRAMSIM=1", "-DUSE_VCD=1"], cwd=trial_runtime_dir).wait()
    FileLock("lock").release()
    subprocess.Popen(["make", "-j", "8"], cwd=trial_runtime_dir).wait()
    # build the executable
    subprocess.Popen(["cmake", ex_dir] + ex_opts, cwd=trial_exec_dir).wait()
    subprocess.Popen(["make", "-j8", ex_name], cwd=trial_exec_dir).wait()
    # run runtime
    runtime = subprocess.Popen(["./ComposerRuntime"] + [f"--dump={d}" for d in all_perf_paths], cwd=trial_runtime_dir)
    time.sleep(1)
    # run the executable
    subprocess.Popen(["./" + ex_name], cwd=trial_exec_dir).wait()
    # kill the runtime if it is still running
    runtime.wait()
    # cleanse the trace
    mydel = subprocess.Popen(["sed", "1,500d"], stdin=open(trial_runtime_dir + "/trace.vcd", 'r'),
                             stdout=subprocess.PIPE)
    subprocess.Popen(["sed", "-e", "/^#.*/d", "-e", "/^\\s*$/d"],
                     stdin=mydel.stdout,
                     stdout=open(trial_runtime_dir + "/trace2.vcd", 'w'),
                     cwd=trial_runtime_dir).wait()
    # os.remove(trial_runtime_dir + "/trace.vcd")
    score = 1

    with open(trial_runtime_dir + "/trace2.vcd", 'r') as f:
        scores = [int(remove_leading_zeroes(a.split(' ')[0]), 2) for a in f.readlines()]
        scores = [a ** (1.0 / len(scores)) for a in scores]
        for s in scores:
            score *= s

    return score


study = opt.create_study()
study.optimize(objective)

print("Best params: ")
print(study.best_params)
print("Best value: ")
print(study.best_value)
print("Best trial: ")
print(study.best_trial)

