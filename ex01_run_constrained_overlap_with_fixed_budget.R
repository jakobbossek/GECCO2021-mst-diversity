library(batchtools)

source("src/defs.R")
source("src/utils.R")

# ATTENTION!
unlink("bt-runner-constrained-overlap-with-fixed-budget", recursive = TRUE, force = TRUE)

reg = batchtools::makeExperimentRegistry(
  file.dir = "bt-runner-constrained-overlap-with-fixed-budget",
  seed = 1L,
  packages = c("mcMST", "grapherator"),
  source = c("src/defs.R", "src/utils.R", "src/mutation.R", "src/runners.R"))

batchtools::addProblem("DUMMY", data = list())

batchtools::addAlgorithm("EA", fun = function(job, data, ...) {
  runnerConstrained(job, data, ...)
})

# Design
graphs = list.files("instances", pattern = ".graph$", full.names = TRUE)
graphs2 = gsub(".graph", "", basename(graphs))
expl = strsplit(graphs2, "_", fixed = TRUE)
ty = sapply(expl, function(part) part[1L])
nn = as.integer(sapply(expl, function(part) part[2L]))
des1 = data.frame(graph = graphs, type = ty, n = nn)

des2 = data.table::CJ(graph = graphs, mu = MUS, sampling.fun = SAMPLING.FUNS, alpha = ALPHAS)
EAdesign = dplyr::left_join(des1, des2, by = "graph")
EAdesign = EAdesign[EAdesign$mu <= floor(EAdesign$n/2), ]
algo.designs = list(EA = data.table::data.table(EAdesign))

batchtools::addExperiments(algo.designs = algo.designs, repls = REPLS)

BBmisc::stopf("Registry successfully generated! :-)")

stop()

ids = findNotDone()
ids = ids[, chunk := batchtools::chunk(job.id, chunk.size = 20L)]
submitJobs(ids = ids, resources = list(walltime = 60 * 60 * 24, mem = "4gb"))

stop()

# COLLECT
jt = getJobTable()
res = batchtools::reduceResultsDataTable(findDone())
res = unwrap(res)
res = dplyr::left_join(res, unwrap(jt)[, c("job.id", "repl", "type")], by = "job.id")
write.table(res, file = "data/results_constrained_overlap_with_fixed_budget.csv", row.names = FALSE, quote = FALSE)
