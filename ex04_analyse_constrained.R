library(tidyverse)
library(ggplot2)
library(kableExtra)
library(RColorBrewer)
load_all("~/repos/software/r/tblutils")

## EDGE-DIVERSITY AND QUALITY
## ===

# import
ddivc = readr::read_delim("data/results_constrained_overlap_with_fixed_budget.csv", delim = " ")

# add maximum diversity
ddivc$MPD = ddivc$mu * (ddivc$mu - 1) * (ddivc$n - 1)
ddivc$diversity = round((ddivc$PD / ddivc$MPD) * 100, digits = 2L)

ddivc$sampling.fun2 = as.integer(factor(ddivc$sampling.fun, levels = c("uniform1", "poisson"), ordered = TRUE))
ddivc = arrange(ddivc, sampling.fun2)

cols = brewer.pal(n = 4L, name = "Dark2")
print(cols)
cols = mutators
ddivc = tblutils::test.pairwise(ddivc, by = c("type", "n", "mu", "alpha"),
  split.col = "sampling.fun2", value.col = "diversity", testresult.col = "diversity.test",
  alternative = "greater", alpha = 0.05,colors = cols[c(1,4)], show.positive.only = TRUE)
ddivc$sampling.fun2 = NULL

ddivc.aggr = ddivc %>%
  group_by(type, n, mu, alpha, sampling.fun) %>%
  dplyr::summarise(
    diversity.mean  = round(mean(diversity), 2L),
    diversity.sd = round(sd(diversity), 2L),
    diversity.test = diversity.test[1L]
  ) %>%
  ungroup() %>%
  arrange(n, mu, alpha, desc(sampling.fun))

tbl = ddivc.aggr
tbl = tblutils::highlight(tbl, by = c("type", "n", "mu", "alpha"), which = "diversity.mean",
  order.fun = "max",
  highlight.fun = tblutils::baseLaTeXHighlighter, bg.color = "gray", bg.saturation.max = 20)

tbl = tblutils::widen(tbl,
  split.col = "sampling.fun",
  widen.cols = c("diversity.mean", "diversity.sd", "diversity.test"))
tbl = tbl %>% arrange(n, mu, alpha)
tbl$sampling.fun = NULL
#tbl$type = NULL

# filtering (table is too large for PPSN)
# tbl.final = tbl %>%
#  filter(!((mu == 3) & (n == 50)) & !((n == 100) & (mu %in% c(3, 10))), type == "euclidean")

tbl.final = tbl %>%
  filter(type == "euclidean") %>%
  filter(n <= 100)
tbl.final$type = NULL

cns = c("$n$", "$\\mu$", "$\\alpha$", rep(c("\\textbf{mean}", "\\textbf{std}", "\\textbf{stat}"), 2))
align = rep("r", length(cns))
ktbl = kableExtra::kable(tbl.final, "latex", col.names = cns, align = align, longtable = FALSE, escape = FALSE, booktabs = TRUE, caption = "test") %>%
  kable_styling() %>%
  row_spec(row = c(12), extra_latex_after = "\\cline{1-9}") %>%
  row_spec(row = c(4, 8, 16, 20), extra_latex_after = "\\cline{2-9}") %>%
  collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle") %>%
  add_header_above(c(" ", " ", " ", "Uniform (1)" = 3, "Poisson (2)" = 3), bold = TRUE) %>%
  kable_styling(latex_options = c("repeat_header"))

cat(ktbl, file = "tables/constrained.tex")

#stop("DONE")


## DIVERSITY MEASURES ONLY
## ===

# import
ddivc = readr::read_delim("data/results_constrained_overlap_with_fixed_budget.csv", delim = " ")

# add maximum diversity
ddivc$MPD = ddivc$mu * (ddivc$mu - 1) * (ddivc$n - 1)
ddivc$diversity = round((ddivc$PD / ddivc$MPD) * 100, digits = 2L)

ddivc.aggr = ddivc %>%
  group_by(type, n, mu, alpha) %>%
  dplyr::filter(sampling.fun %in% c("poisson"), type == "euclidean") %>%
  dplyr::summarise(
    diversity.mean  = round(mean(diversity), 2L),
    diversity.sd = round(sd(diversity), 2L),
    maxdegrees.mean = round(mean((n.maxdegrees / mu) * 100), 2L),
    maxdegrees.sd = round(sd((n.maxdegrees / mu)  * 100), 2L),
    leafs.mean = round(mean((n.leafs / mu)  * 100), 2L),
    leafs.sd = round(sd((n.leafs / mu) * 100), 2L),
    diameters.mean = round(mean((n.diameters / mu) * 100), 2L),
    diameters.sd = round(sd((n.diameters / mu) * 100), 2L)
  ) %>%
  ungroup() %>%
  arrange(n, mu, alpha)

tbl = ddivc.aggr
tbl = tbl %>%
  filter(type == "euclidean") %>%
  filter(n <= 100)
tbl$type = NULL
cns = c("$n$", "$\\mu$", "$\\alpha$", rep(c("\\textbf{mean}", "\\textbf{std}"), 4))
align = rep("r", length(cns))
ktbl = kableExtra::kable(tbl, "latex", col.names = cns, align = align, longtable = FALSE, escape = FALSE, booktabs = TRUE, caption = "test") %>%
  kable_styling() %>%
  row_spec(row = c(4, 8, 16, 20), extra_latex_after = "\\cline{2-11}") %>%
  collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle") %>%
  #collapse_rows(columns = 3, latex_hline = "major", valign = "middle") %>%
  add_header_above(c(" ", " ", " ", "$D_o$" = 2, "Max. degree" = 2, "Leaf" = 2, "Diameter" = 2), bold = TRUE) %>%
  kable_styling(latex_options = c("repeat_header"))

cat(ktbl, file = "tables/constrained_euclidean_diversity_measures.tex")

stop("DONE")
