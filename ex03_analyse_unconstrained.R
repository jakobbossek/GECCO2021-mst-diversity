library(tidyverse)
library(ggplot2)
library(kableExtra)
library(RColorBrewer)

load_all("~/repos/software/r/tblutils")

# import
ddiv  = readr::read_delim("data/results_overlap_with_fixed_budget.csv", delim = " ")

# filter
mutators = c("uniform1", "uniform2", "uniform3", "poisson")
ddiv = dplyr::filter(ddiv, sampling.fun %in% mutators)

# add maximum diversity
ddiv$MPD = ddiv$mu * (ddiv$mu - 1) * (ddiv$n - 1)
ddiv$diversity = round((ddiv$PD / ddiv$MPD) * 100, digits = 2L)

# normalize evaluations to [0,1]
#ddiv$evals = ddiv$evals / (ddiv$mu * ddiv$n^2)

# BOXPLOTS
# ===

# all hit the time limit: not interesting
ddiv.plot = filter(ddiv, mu < (n/2))
ddiv.plot$evals = ddiv.plot$evals / (ddiv.plot$mu * ddiv.plot$n^2)

ddiv.plot$sampling.fun = factor(ddiv.plot$sampling.fun, levels = mutators, labels = c("Uniform[1]", "Uniform[2]", "Uniform[3]", "Poisson"), ordered = TRUE)
g = ggplot(ddiv.plot)
g = g + geom_boxplot(aes(x = as.factor(mu), y = evals, color = as.factor(sampling.fun)), alpha = 0.5)
g = g + facet_grid(.~n, labeller = label_both, scales = "free", space = "free")#drop = TRUE)
#g = g + scale_x_discrete(expand = c(2/5, 3/5, 4/5, 0))
#g = g + facet_grid(.~n, labeller = label_both, nrow = 1L, scales = "free", space = "free")#drop = TRUE)
g = g + scale_color_brewer(palette = "Dark2")
shares = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
g = g + theme_bw()
g = g + theme(legend.position = "top")
g = g + labs(x = expression(mu), y = "Required share of max. f-evaluations", color = "Sampling")
#print(g)
ggsave("figures/boxplots_fevals.pdf", plot = g, width = 6, height = 3, device = cairo_pdf)
#stop()

# TABLES
# ===

ddiv$sampling.fun = factor(ddiv$sampling.fun, levels = mutators, ordered = TRUE)
ddiv$sampling.fun2 = as.integer(factor(ddiv$sampling.fun, levels = mutators, ordered = TRUE))
ddiv = arrange(ddiv, sampling.fun2)

cols = brewer.pal(n = 4L, name = "Dark2")
print(cols)
cols = mutators
#ddiv = arrange(ddiv, sampling.fun2)
ddiv = tblutils::test.pairwise(ddiv, by = c("n", "mu"),,
  split.col = "sampling.fun2", value.col = "evals", testresult.col = "evals.test",
  alternative = "less", alpha = 0.05, colors = cols, show.positive.only = TRUE)
ddiv$sampling.fun2 = NULL

ddiv.aggr = ddiv %>%
  group_by(n, mu, sampling.fun) %>%
  dplyr::summarise(
    diversity  = mean(diversity),
    evals.mean = mean(evals),
    evals.sd   = sd(evals),
    evals.test = evals.test[1L]
    ) %>%
  ungroup() %>%
  arrange(n, mu, sampling.fun)

tbl = tblutils::highlight(ddiv.aggr, by = c("n", "mu"), which = "evals.mean",
  highlight.fun = tblutils::baseLaTeXHighlighter, bg.color = "gray", bg.saturation.max = 20)

tbl = tblutils::widen(tbl, split.col = "sampling.fun", widen.cols = c("diversity", "evals.mean", "evals.sd", "evals.test"))
tbl$sampling.fun = NULL

tbl = tbl %>% arrange(n, mu)

cns = c("$n$", "$\\mu$", rep(c("$D_o$", "\\textbf{mean}", "\\textbf{std}", "\\textbf{stat}"), length(mutators)))
align = rep("r", 2 + 4 * length(mutators))
ktbl = kableExtra::kable(tbl, "latex", col.names = cns, align = align, escape = FALSE, booktabs = TRUE, caption = "test") %>%
  kable_styling() %>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
  add_header_above(c(" ", " ", "Uniform[1] (1)" = 4, "Uniform[2] (2)" = 4, "Uniform[3] (3)" = 4, "Poisson (4)" = 4), bold = TRUE)

cat(ktbl, file = "tables/uniform_weights_all.tex")

tbl

#stop()

## DIFFERENT DIVERSITY MEASURES (LEAFS, DIAMTER, MAX.DEGREE)
## ===

# import
ddiv  = readr::read_delim("data/results_overlap_with_fixed_budget.csv", delim = " ")

# add maximum diversity
ddiv$MPD = ddiv$mu * (ddiv$mu - 1) * (ddiv$n - 1)
ddiv$diversity = round((ddiv$PD / ddiv$MPD) * 100, digits = 2L)

# now aggregate
ddiv.aggr = ddiv %>%
  # only uniform(1) since we have best diversity on average
  dplyr::filter(sampling.fun == "uniform1") %>%
  group_by(n, mu) %>%
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
  arrange(n, mu)

cns = c("$n$", "$\\mu$", rep(c("\\text{mean}", "\\text{std}"), 4))
align = rep("r", 10)
ktbl = kableExtra::kable(ddiv.aggr, "latex", col.names = cns, align = align, escape = FALSE, booktabs = TRUE, caption = "test") %>%
  kable_styling() %>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
  add_header_above(c(" ", " ", "Edge" = 2, "Max. degree" = 2, "Leaf" = 2, "Diameter" = 2), bold = TRUE)

cat(ktbl, file = "tables/uniform_weights_diversity_measures.tex")
