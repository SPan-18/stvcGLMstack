rm(list = ls())

library(spStack)

data("simPoisson")
n <- 120

dat <- simPoisson[1:n, ]
dat$t_coords <- 1:n # runif(n, 0, 1)
sigma0 <- matrix(c(2, 0.1,
                   0.1, 2), nrow = 2, byrow = T)

mod.list <- candidateModels(list(phi_s = c(1, 1),
                                 phi_t = c(1, 2),
                                 boundary = c(0.5)),
                            "cartesian")

mod1 <- stvcGLMstack(y ~ x1 + (x1), data = dat, family = "poisson",
                     sp_coords = as.matrix(dat[, c("s1", "s2")]),
                     time_coords = as.matrix(dat[, "t_coords"]),
                     cor.fn = "gneiting-decay",
                     process.type = "multivariate",
                     # process.type = "independent.shared",
                     # process.type = "independent",
                     # priors = list(IW.scale = sigma0),
                     candidate.models = mod.list,
                     # loopd.controls = list(method = "exact", nMC = 500),
                     loopd.controls = list(method = "CV", CV.K = 10, nMC = 1000),
                     n.samples = 500
                     )
# mod1$loopd
loo::stacking_weights(mod1)

library(CVXR)
# library(Rmosek)
log_loopd <- mod1
log_loopd_m <- mean(log_loopd)
log_loopd <- log_loopd - log_loopd_m
loopd <- exp(log_loopd)
M <- ncol(loopd)

w <- Variable(M)
obj <- Maximize(sum(log(loopd %*% w)))
constr <- list(sum(w) == 1, w >= 0)
prob <- Problem(objective = obj, constraints = constr)
result <- solve(prob, solver = "ECOS")
round(as.numeric(result$getValue(w)), 4)
