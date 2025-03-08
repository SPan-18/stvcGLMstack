rm(list = ls())

library(spStack)
library(dplyr)

bbs <- read.csv("BBS10-19.csv")
bbs <- bbs %>%
  filter(Latitude < 55) %>%
  filter(BirdCount < 500)

bbs_coords <- as.matrix(bbs[, c("Longitude", "Latitude")])
bbs_time <- as.matrix(bbs[, "Time"])

sigma0 <- matrix(c(2, 0.5, 0.5,
                   0.5, 2, 0.5,
                   0.5, 0.5, 2),
                 nrow = 3, byrow = T)

mod1 <- stvcGLMexact(BirdCount ~ NCar + Noise + (NCar + Noise),
                     data = bbs, family = "poisson",
                     sp_coords = bbs_coords,
                     time_coords = bbs_time,
                     cor.fn = "gneiting-decay",
                     process.type = "multivariate",
                     priors = list(IW.scale = sigma0),
                     sptParams = list(phi_s = 800, phi_t = 1),
                     n.samples = 1000, verbose = TRUE)

post_beta <- mod1$samples$beta
print(t(apply(post_beta, 1, function(x) quantile(x, c(0.025, 0.5, 0.975)))))
print(mod1$run.time)

post_z <- mod1$samples$z

write.table(post_beta,
            file = "beta.txt",
            col.names = FALSE, row.names = FALSE)
write.table(post_z,
            file = "z.txt",
            col.names = FALSE, row.names = FALSE)
