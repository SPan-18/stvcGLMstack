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
phi_s <- c(4, 3)
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
