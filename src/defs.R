# SETUP
# ===

NS  = c(50, 100, 200, 400)
SAMPLING.FUNS = c("poisson", "uniform1", "uniform2", "uniform3")
MUS = c(2, 10, 25, 50, 100, 200)
ALPHAS = c(0.05, 0.1, 0.5, 1)

REPLS = 30L
REPLS.OVERLAP.WITH.FIXED.BUDGET = 30L

STORAGE = "output"

getMaxEvals = function(n, mu) {
  n^2 * mu
}
