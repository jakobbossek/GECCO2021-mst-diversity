library(mcMST)
library(grapherator)
library(ggplot2)

source("src/defs.R")
source("src/utils.R")
source("src/mutation.R")
source("src/visualization.R")

# EXAMPLE
# ===

n = 50L
grr = genCompleteGraph(n, "euclidean")
gr  = mcMST:::grapheratorToGraph(grr)
mst = gr$getMSTByWeightedSumScalarization(1)

# msts = mcMST::mcMSTWeightedSum(gr, n.lambdas = 40)
# ps = lapply(msts$pareto.set, function(el) {
#   mcMST::edgeListToGraph(el, gr)
# })


m = gr$getE()
mu = 10L # < n/2

set.seed(1)

results = lapply(ALPHAS, function(alpha) {
  EAdiversity(mu = mu, graph = gr, sampling.fun = "uniform1", max.evals = n^2 * mu, detailed = TRUE, alpha = alpha)
})

sis = lapply(results, function(res) {
  plotSuperimposedEdges(grr, res$P, mst) + theme(legend.position = "none")
})
sis$nrow = 1
g = do.call(gridExtra::grid.arrange, sis)
ggsave("figures/superimposed_edges.pdf", pl = g, width = 14, height = 3.6)
stop("DONE")

# first population
P = results[[1L]]$P
G = results[[1L]]$graph

BBmisc::pause()

source("src/visualization.R")

props = lapply(P, getProperties)
captions = sapply(props, collapse, sep = " / ")

g = plotSolutions(grr, P[1:3], mst = NULL, captions = captions)
stop()

res = EAdiversity(mu = mu, graph = gr, diversity.fun = diversityEdgeFrequency,
  diversity.opt = diversity.opt, max.evals = n^2 * mu, detailed = TRUE, alpha = 1.5)
print(isPairwiseEdgeDisjoint(res$P))

#stop()
source("src/visualization.R")
si = plotSuperimposedEdges(grr, res$P, mst)#list(mst1, mst1, mst1, mst2))

print(si)

