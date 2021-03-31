# WORKING HORSE
# ===
EAdiversity = function(mu, init = NULL, graph, max.evals = Inf, sampling.fun = "uniform", log.trace = TRUE, detailed = FALSE, alpha = NULL, ...) {
  st = proc.time()[3L]
  m = graph$getE()
  n = graph$getV()
  sampling.fun2 = base::match.fun(sampling.fun)

  # init population
  if (is.null(init)) {
    if (is.null(alpha)) {
      init = graph$getRandomMST()
    } else {
      init = graph$getMSTByWeightedSumScalarization(1)
    }
  }

  P = lapply(1:mu, function(i) init)

  do.constrained = !is.null(alpha)
  if (!do.constrained) {
    alpha = 1 # dummy to make condition below work
  }
  mst = NA
  mst.weight = Inf
  if (do.constrained) {
    mst = graph$getMSTByWeightedSumScalarization(1)
    mst.weight = mst$getSumOfEdgeWeights()[1L]
  }

  # build overlap matrix
  OM = matrix(0, ncol = mu + 1L, nrow = mu + 1L)
  for (i in seq_len(mu)) {
    for (j in i:mu) {
      if (i == j)
        next
      OM[i, j] = OM[j, i] = P[[i]]$getNumberOfCommonEdges(P[[j]])
    }
  }

  # actually, iterations
  evals = 1

  diversity.opt = mu * (mu - 1) * (n - 1)
  PD = mu * (mu - 1) * (n - 1) - sum(OM)

  # stop()
  trace = NA
  if (log.trace) {
    trace = integer(max.evals)
    trace[1L] = PD
  }

  #print(DP)

  while (TRUE) {
    # sample individual and mutate
    x = sample(P, size = 1L)[[1L]]

    # generate new individual
    k = sampling.fun2(n = m)
    #k = if (max.k == 1L) max.k else sample(1:max.k, size = 1L)
    y = graph$getMSTByEdgeExchange(x, k, FALSE)
    y.weight = Inf

    # monitoring
    evals = evals + 1L
    if (evals %% 10000L == 0)
      BBmisc::catf("Evals: %i", evals)

    if (do.constrained) {
      y.weight = y$getSumOfEdgeWeights()[1L]
    }

    if (!do.constrained | (y.weight <= (1 + alpha) * mst.weight)) {

      # now do diversity filtering
      Q = c(P, list(y))
      # now calculate overlap to other individuals
      o = c(sapply(P, function(x) x$getNumberOfCommonEdges(y)), 0)
      #BBmisc::catf("Overlap with others: %s", collapse(o, sep = ", "))
      # add overlap values of new individual to
      OM[mu + 1L, ] = o
      OM[, mu + 1L] = o

      #print(OM)
      DIV = PD - 2 * sum(o)

      idx.best = NA
      best.div = PD
      for (i in seq_len(mu + 1L)) {
        QQ = Q[-i]
        QQD = DIV + 2 * sum(OM[i, ])
        #BBmisc::catf("Drop %i: %i", i, QQD)
        #BBmisc::pause()
        #if (isMoreDiverse(QQD, PD)) {
        if (QQD >= best.div) {
          #BBmisc::catf("Found improvement!")
          idx.best = i
          best.div = QQD
        }
      }

      if (!is.na(idx.best)) {
        #BBmisc::catf("Best idx to drop is %i", idx.best)
        P = Q[-idx.best]
        PD = DIV + 2 * sum(OM[idx.best, ])
        OM[1:mu, 1:mu] = OM[-idx.best, -idx.best]
      }
    } # if

    if (log.trace)
      trace[evals] = PD

    #BBmisc::catf("Div. is %i", PD)

    # check stopping condition
    if (PD == (mu * (mu - 1) * (n - 1))) {
    #if (isMaximallyDiverse(PD, diversity.opt)) {
      break
    }

    if (evals >= max.evals)
      break
  }

  res = list(
    n = n,
    m = m,
    mu = mu,
    sampling.fun = sampling.fun,
    alpha = alpha,
    evals = evals,
    max.evals = max.evals,
    trace = trace[seq_len(evals)],
    time.passed = as.numeric(proc.time()[3L] - st))

  if (detailed) {
    res = c(res, list(
      graph = graph,
      P = P,
      PD = PD
    ))
  }
  return(res)
}

# UTILITIES
# ===

isPairwiseEdgeDisjoint = function(P) {
  n = length(P)
  for (i in seq_len(n)) {
    for (j in i:n) {
      if (i == j) next
      if (P[[i]]$getNumberOfCommonEdges(P[[j]]) != 0)
        return(FALSE)
    }
  }
  return(TRUE)
}

genCompleteGraph = function(n, type = "random") {
  grr = graph(0, 100)
  grr = addNodes(grr, n = n, generator = addNodesUniform)
  grr = addEdges(grr, generator = addEdgesComplete)
  if (type == "random") {
    grr = addWeights(grr, generator = addWeightsRandom, method = runif, min = 10, max = 100, to.int = FALSE)
  } else if (type == "euclidean") {
    grr = addWeights(grr, generator = grapherator::addWeightsDistance, method = "euclidean", to.int = FALSE)
  }
  # second dummy weight
  grr = addWeights(grr, generator = addWeightsRandom, method = runif, min = 10, max = 100, to.int = FALSE)
  return(grr)
}

genBinString = function(n) {
  sample(c(0, 1), size = n, replace = TRUE)
}

getDiversityFun = function(x) {
  if (x == "edgefrequency")
    return(diversityEdgeFrequency)
  else if (x == "degree")
    return(diversityDegree)
  else if (x == "leaf")
    return(diversityLeaf)
  else
    BBmisc::stopf("getDiversityFun: '%s' is unknown!", x)
}

diversityDegree = function(S, gr) {
  BBmisc::stopf("diversityDegree: not yet implemented!")
}

diversityLeaf = function(S, gr) {
  BBmisc::stopf("diversityLeafs: not yet implemented!")
}

isMaximallyDiverse = function(x, opt) {
  all(x == opt)
}
