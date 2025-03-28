rm(list = ls())

library(spStack)

rmvn <- function(n, mu = 0, V = matrix(1)) {
  p <- length(mu)
  if (any(is.na(match(dim(V), p))))
    stop("error: dimension mismatch.")
  D <- chol(V)
  t(matrix(rnorm(n * p), ncol = p) %*% D + rep(mu, rep(n, p)))
}

set.seed(1729)
n <- 500
beta <- c(5, -0.5)
p <- length(beta)
X <- cbind(rep(1, n), sapply(1:(p - 1), function(x) rnorm(n)))
X_tilde <- X
phi_s <- c(2, 3)
phi_t <- c(2, 4)
sigmasq.z <- c(0.25, 0.5)
S <- data.frame(s1 = runif(n, 0, 1), s2 = runif(n, 0, 1))
Tm <- runif(n)
dist_S <- as.matrix(dist(as.matrix(S)))
dist_T <- as.matrix(dist(as.matrix(Tm)))
Vz1 <- 1/(1 + phi_t[1] * dist_T^2) * exp(- (phi_s[1] * dist_S) / sqrt(1 + phi_t[1] * dist_T^2))
Vz2 <- 1/(1 + phi_t[2] * dist_T^2) * exp(- (phi_s[2] * dist_S) / sqrt(1 + phi_t[2] * dist_T^2))
z1 <- rmvn(1, rep(0, n), sigmasq.z[1] * Vz1)
z2 <- rmvn(1, rep(0, n), sigmasq.z[2] * Vz2)
muFixed <- X %*% beta
muSpT <- X_tilde[, 1] * z1 + X_tilde[, 2] * z2
mu <- muFixed + muSpT
y <- sapply(1:n, function(x) rpois(n = 1, lambda = exp(mu[x])))
dat <- cbind(S, Tm, X[, -1], y, z1, z2)
names(dat) <- c("s1", "s2", "t_coords", paste("x", 1:(p - 1), sep = ""), "y",
                "z1_true", "z2_true")

mod.list <- candidateModels(list(phi_s = c(2, 4, 5),
                                 phi_t = c(3, 4, 6),
                                 boundary = c(0.5)),
                            "cartesian")

# mod.list <- candidateModels(list(
#   phi_s = list(c(1, 1), c(1, 2)),
#   phi_t = list(c(1, 1), c(1, 2)),
#   boundary = c(0.5)), "cartesian")

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
                     loopd.controls = list(method = "CV", CV.K = 10, nMC = 500),
                     n.samples = 1000)

mod1 <- recoverGLMscale(mod1) 
post_samps <- stackedSampler(mod1)
post_beta <- post_samps$beta

print(t(apply(post_beta, 1, function(x) quantile(x, c(0.025, 0.5, 0.975)))))
print(mod1$run.time)

post_z <- post_samps$z

post_z1_summ <- t(apply(post_z[1:n,], 1,
                        function(x) quantile(x, c(0.025, 0.5, 0.975))))
post_z2_summ <- t(apply(post_z[n + 1:n,], 1,
                        function(x) quantile(x, c(0.025, 0.5, 0.975))))

z1_combn <- data.frame(z = dat$z1_true,
                       zL = post_z1_summ[, 1],
                       zM = post_z1_summ[, 2],
                       zU = post_z1_summ[, 3])
z2_combn <- data.frame(z = dat$z2_true,
                       zL = post_z2_summ[, 1],
                       zM = post_z2_summ[, 2],
                       zU = post_z2_summ[, 3])

library(ggplot2)
plot_z1_summ <- ggplot(data = z1_combn, aes(x = z)) +
  geom_errorbar(aes(ymin = zL, ymax = zU),
                width = 0.05, alpha = 0.15,
                color = "skyblue") +
  geom_point(aes(y = zM), size = 0.25,
             color = "darkblue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "solid") +
  xlab("True z1") + ylab("Posterior of z1") +
  theme_bw() +
  theme(panel.background = element_blank(),
        aspect.ratio = 1)

plot_z2_summ <- ggplot(data = z2_combn, aes(x = z)) +
  geom_errorbar(aes(ymin = zL, ymax = zU),
                width = 0.05, alpha = 0.15,
                color = "skyblue") +
  geom_point(aes(y = zM), size = 0.25,
             color = "darkblue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "solid") +
  xlab("True z2") + ylab("Posterior of z2") +
  theme_bw() +
  theme(panel.background = element_blank(),
        aspect.ratio = 1)

ggpubr::ggarrange(plot_z1_summ, plot_z2_summ)
