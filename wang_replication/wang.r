library('np')
# Start timing
start_time <- Sys.time()

# Print out basic information on the model
cat("==================================================================================\n")
cat(paste("Running", basename(sys.frame(1)$ofile), "at", Sys.time(), "\n"))
cat("==================================================================================\n")
# Read data
data <- readxl::read_excel("GSS2014.xlsx")

# Data processing
x <- data[[2]]
v <- data[[22]]
v0 <- data[[28]]
v1 <- data[[29]]
z <- data[[4]]
y <- data[[24]]
datan <- cbind(y, x, z, v0, v1, v)
datan <- datan[complete.cases(datan), ]

yn <- datan[, 1]
xn <- datan[, 2]
zn <- datan[, 3]
v0n <- datan[, 4]
v1n <- datan[, 5]
vn <- datan[, 6]
N <- nrow(datan)

# Kernel bandwidth estimation
hnv0 <- 1.06 * sd(v0n) * N^(-1/5)
hnv1 <- 1.06 * sd(v1n) * N^(-1/5)
hnz <- 1.06 * sd(zn) * N^(-1/5)
hnx <- 1.06 * sd(xn) * N^(-1/5)

# Estimation
b <- seq(-0.1, 0.5, by = 0.01)
c <- seq(0, 1, by = 0.02)
d <- seq(0.5, 6, by = 0.02)
a <- seq(5, 50, by = 0.5)
coeff <- expand.grid(b, c, d, a)
Lc <- nrow(coeff)

coeff1 <- coeff[, 1]
coeff2 <- coeff[, 2]
coeff3 <- coeff[, 3]
coeff4 <- coeff[, 4]

w1 <- as.integer(yn > 1)
w2 <- as.integer(yn == 3)

# Kernel estimation
ker <- outer(v0n, v0n, function(a, b) dnorm((a - b) / hnv0)) *
       outer(v1n, v1n, function(a, b) dnorm((a - b) / hnv1)) *
       outer(zn, zn, function(a, b) dnorm((a - b) / hnz)) *
       outer(xn, xn, function(a, b) dnorm((a - b) / hnx))

plot(ker, xlim=c(-0.01, 0.02))
plot(density(pnw2))

pnw1 <- colSums(w1 * ker) / colSums(ker)
pnw2 <- colSums(w2 * ker) / colSums(ker)

lam1 <- as.integer(pnw1 > 0.5)
lam2 <- as.integer(pnw2 > 0.5)

snb <- numeric(Lc)

for (lc in seq_len(Lc)) {
  xzv1 <- xn * coeff1[lc] + zn * coeff2[lc] + v1n * coeff3[lc]
  xzv0 <- xn * coeff1[lc] + zn * coeff2[lc] + v0n * coeff3[lc]
  
  snb[lc] <- sum(
    ((w1 - 0.5) * lam1) * ((xzv1 + 0.2 > 0) - (xzv1 + 0.2 <= 0)) +

      ((w1 - 0.5) * (1 - lam1)) * ((xzv0 + 0.2 > 0) - (xzv0 + 0.2 <= 0)) +
                ((w2 - 0.5) * lam2) * ((xzv1 + 0.2 - coeff4[lc] > 0) - (xzv1 + 0.2 - coeff4[lc] <= 0)) +
                ((w2 - 0.5) * (1 - lam2)) * ((xzv0 + 0.2 - coeff4[lc] > 0) - (xzv0 + 0.2 - coeff4[lc] <= 0))) / N
  print(paste0(lc, '/', seq_len(Lc), ' is done!'))
}

ind <- (snb >= max(snb) - 0.1 * N^(-2/3) * sqrt(log(log(N))))
coefset <- coeff[ind, ]

# Confidence intervals using bootstrap
REP <- 100
boot_results <- replicate(REP, {
  sample_indices <- sample(seq_len(N), N, replace = TRUE)
  yn_sample <- yn[sample_indices]
  xn_sample <- xn[sample_indices]
  zn_sample <- zn[sample_indices]
  v0n_sample <- v0n[sample_indices]
  v1n_sample <- v1n[sample_indices]
  
  w1_sample <- as.integer(yn_sample > 1)
  w2_sample <- as.integer(yn_sample == 3)
  
  hnv0_sample <- 1.06 * sd(v0n_sample) * N^(-1/5)
  hnv1_sample <- 1.06 * sd(v1n_sample) * N^(-1/5)
  hnz_sample <- 1.06 * sd(zn_sample) * N^(-1/5)
  hnx_sample <- 1.06 * sd(xn_sample) * N^(-1/5)
  
  ker_sample <- outer(v0n_sample, v0n_sample, function(a, b) dnorm((a - b) / hnv0_sample)) *
                outer(v1n_sample, v1n_sample, function(a, b) dnorm((a - b) / hnv1_sample)) *
                outer(zn_sample, zn_sample, function(a, b) dnorm((a - b) / hnz_sample)) *
                outer(xn_sample, xn_sample, function(a, b) dnorm((a - b) / hnx_sample))
  
  pnw1_sample <- colSums(w1_sample * ker_sample) / colSums(ker_sample)
  pnw2_sample <- colSums(w2_sample * ker_sample) / colSums(ker_sample)
  
  lam1_sample <- as.integer(pnw1_sample > 0.5)
  lam2_sample <- as.integer(pnw2_sample > 0.5)
  
  snb_sample <- numeric(Lc)
  for (lc in seq_len(Lc)) {
    xzv1_sample <- xn_sample * coeff1[lc] + zn_sample * coeff2[lc] + v1n_sample * coeff3[lc]
    xzv0_sample <- xn_sample * coeff1[lc] + zn_sample * coeff2[lc] + v0n_sample * coeff3[lc]
    
    snb_sample[lc] <- sum(((w1_sample - 0.5) * lam1_sample) * ((xzv1_sample + 0.2 > 0) - (xzv1_sample + 0.2 <= 0)) +
                           ((w1_sample - 0.5) * (1 - lam1_sample)) * ((xzv0_sample + 0.2 > 0) - (xzv0_sample + 0.2 <= 0))) / N
  }
  min(snb_sample)
})

# Quantiles for confidence intervals
ci_95 <- quantile(boot_results, probs = c(0.05, 0.95))

# Calculate elapsed time
time_spent <- difftime(Sys.time(), start_time, units = "mins")
cat(sprintf("\nSimulation Time since Start: %14.4g Mins\n", as.numeric(time_spent)))

