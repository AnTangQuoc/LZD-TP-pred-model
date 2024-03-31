library(BAS)
run_bas_glm <- function(data, formula, ...) {
  bas.glm(
    formula,
    data = data,
    family = binomial(),
    # MCMC.iterations = 200000,
    # 100000 takes 1.5 mins, should be NULL (run until convergence, ~2.5 hrs)
    method = "MCMC+BAS",
    laplace = TRUE,
    renormalize = TRUE,
    ...
  )
}
