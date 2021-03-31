uniform = function(n, max.k = 1L) {
  if (max.k == 1L)
    return(1L)
  return(sample(seq_len(max.k), size = 1L))
}

poisson = function(n) {
  extraDistr::rtpois(n = 1L, lambda = 1L, a = 1L, b = n)
}

cmut = function(n, p) {
  k = 1L
  if (runif(1) > p)
    k = sample(2:n, size = 1L)
  return(k)
}

uniform1 = function(n) {
  uniform(n, max.k = 1L)
}

uniform2 = function(n) {
  uniform(n, max.k = 2L)
}

uniform3 = function(n) {
  uniform(n, max.k = 3L)
}

cmut0.5 = function(n) {
  cmut(n, p = 0.5)
}
