library(batchtools)

source("src/defs.R")
source("src/utils.R")
source("src/mutation.R")
source("src/runners.R")

# ATTENTION!
unlink("bt-runner-unconstrained-overlap-with-fixed-budget", recursive = TRUE, force = TRUE)

reg = batchtools::makeExperimentRegistry(
  file.dir = "bt-runner-unconstrained-overlap-with-fixed-budget",
  seed = 1L,
  packages = c("mcMST", "grapherator"),
  source = c("src/defs.R", "src/utils.R", "src/mutation.R", "src/runners.R"))

batchtools::addProblem("DUMMY", data = list())

batchtools::addAlgorithm("EA", fun = function(job, data, ...) {
  runnerUnconstrained(job, data, ...)
})

# Design
EAdesign = data.table::CJ(n = NS, mu = MUS, sampling.fun = SAMPLING.FUNS)
EAdesign = EAdesign[EAdesign$mu <= floor(EAdesign$n/2), ]
algo.designs = list(EA = EAdesign)

batchtools::addExperiments(algo.designs = algo.designs, repls = REPLS.OVERLAP.WITH.FIXED.BUDGET)

BBmisc::stopf("Registry successfully generated! :-)")

ids = findNotDone()
ids = ids[, chunk := batchtools::chunk(job.id, chunk.size = 10L)]

submitJobs(ids = ids, resources = list(walltime = 60 * 60 * 24))

stop()

# COLLECT
jt = getJobTable()
res = batchtools::reduceResultsDataTable(findDone())
res = unwrap(res)
res = dplyr::left_join(res, jt[, c("job.id", "repl")], by = "job.id")
write.table(res, file = "data/results_unconstrained_overlap_with_fixed_budget.csv", row.names = FALSE, quote = FALSE)
