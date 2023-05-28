#exercitiul 1
#90%
alfa <- 1 - 0.9
sample_mean <- 138
n <- 10
sigma <- 11
critical_z <- qnorm(1 - alfa/2, 0, 1)
a <- sample_mean - critical_z * sigma / sqrt(n)
b <- sample_mean + critical_z * sigma / sqrt(n)
interval <- c(a,b)
interval
#95%
alfa <- 1 - 0.95
sample_mean <- 138
n <- 10
sigma <- 11
critical_z <- qnorm(1 - alfa/2, 0, 1)
a <- sample_mean - critical_z*sigma / sqrt(n)
b <- sample_mean + critical_z*sigma / sqrt(n)
interval <- c(a,b)
interval
#99%
alfa <- 1 - 0.99
sample_mean <- 138
n <- 10
sigma <- 11
critical_z <- qnorm(1 - alfa/2, 0, 1)
a <- sample_mean - critical_z * sigma / sqrt(n)
b <- sample_mean + critical_z * sigma / sqrt(n)
interval <- c(a,b)
interval

#exercitiul 2
alfa <- 1 - 0.95
sample_mean <- 18
n <- 256
s <- 1.2
se <- s/sqrt(n)
critical_t <- qt(1 - alfa/2, n - 1)
a <- sample_mean - critical_t*se
b <- sample_mean + critical_t*se
interval <- c(a,b)
interval

#exercitiul 3
#1%
alfa <- 0.01
n <- 153
succese <- 17
p_prim <- succese/n
p_0 <- 0.12
z_score <- (p_prim - p_0) / sqrt(p_0 * (1 - p_0) / n)
critical_z <- qnorm(1 - alfa, 0, 1)
z_score
critical_z
#5%
alfa <- 0.05
n <- 153
succese <- 17
p_prim <- succese/n
p_0 <- 0.12
z_score <- (p_prim - p_0) / sqrt(p_0 * (1 - p_0) / n)
critical_z <- qnorm(1 - alfa, 0, 1)
z_score
critical_z