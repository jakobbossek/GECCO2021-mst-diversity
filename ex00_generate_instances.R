library(mcMST)
library(grapherator)

source("src/defs.R")
source("src/utils.R")

set.seed(1)

for(n in NS) {
  for (type in c("euclidean", "random")) {
    graph = genCompleteGraph(n, type)
    fn = sprintf("instances/%s_%s.graph", type, n)
    grapherator::writeGP(graph, file = fn)
  }
}
