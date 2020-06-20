lookup <- cbind(paste0("coef", 1:16),
                colnames(design_matrix))

dl <- lapply(draws, function (x) {
  names <- colnames(x)
  idx <- match(lookup[, 1], names)
  names[idx] <- lookup[, 2]
  colnames(x) <- names
  x
})


library(bayesplot)
bayesplot::mcmc_intervals(draws)
coda::varnames(draws)[1:17] <- colnames(design_matrix)

draws2 <- coda::as.mcmc.list(dl)

plot(draws2)

