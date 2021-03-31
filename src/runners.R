cleanSlurmOutput = function() {
  slurm.output = list.files(getwd(), pattern = "slurm.*", full.names = TRUE)
  unlink(slurm.output, force = TRUE)
}

runnerUnconstrained = function(job, data, ...) {
  args = list(...)
  grr = genCompleteGraph(args$n)
  gr  = mcMST:::grapheratorToGraph(grr)

  res = EAdiversity(
    mu = args$mu,
    graph = gr,
    sampling = args$sampling.fun,
    max.evals = getMaxEvals(args$n, args$mu),
    log.trace = TRUE,
    detailed = TRUE)

  # calculate measures
  res$n.maxdegrees = length(unique(sapply(res$P, function(p) p$getMaximumDegree())))
  res$n.leafs = length(unique(sapply(res$P, function(p) p$getNumberOfLeafs())))
  res$n.diameters = length(unique(sapply(res$P, function(p) p$getDiameter())))

  fn = sprintf("%s/unconstrained/%i.rds", STORAGE, job$id)
  saveRDS(res, file = fn)

  cleanSlurmOutput()

  res$trace = res$graph = res$P = NULL
  return(res)
}

runnerConstrained = function(job, data, ...) {
  args = list(...)
  grr = grapherator::readGP(args$graph)
  gr  = mcMST:::grapheratorToGraph(grr)

  res = EAdiversity(
    mu = args$mu,
    graph = gr,
    alpha = args$alpha,
    sampling = args$sampling.fun,
    max.evals = getMaxEvals(args$n, args$mu),
    log.trace = TRUE,
    detailed = TRUE)

  res$n.maxdegrees = length(unique(sapply(res$P, function(p) p$getMaximumDegree())))
  res$n.leafs = length(unique(sapply(res$P, function(p) p$getNumberOfLeafs())))
  res$n.diameters = length(unique(sapply(res$P, function(p) p$getDiameter())))

  fn = sprintf("%s/constrained/%i.rds", STORAGE, job$id)
  saveRDS(res, file = fn)

  cleanSlurmOutput()

  res$trace = res$graph = res$P = NULL
  return(res)
}
