#exercitiul 1
LNM <- function(n, p)
{
  for(i in length(n))
  {
    for(j in length(p))
    {
      s <- rgeom(n, prob = p)
      s_m <- mean(s)
      e_mean <- 1/p
      print(paste("Sample mean", s_m))
      print(paste("Expected mean", e_mean))
    }
  }
}
n <- c(5000, 10000, 100000, 500000)
p <- c(0.2, 0.6, 0.6, 0.8)
LNM(n, p)

#exercitiul 2
TLC <- function(r, n, N, z)
{
  errors = matrix(nrow = length(N), ncol = length(z))
  for(i in 1:length(N))
  {
    for(j in 1:length(z))
    {
      sample = vector("numeric", n)
      for(k in 1:N[i])
      {
        x = rt(n,r)
        sample[k] = mean(x)
      }
      estimate_mean = mean(sample)
      estimate_variance = var(sample)
      estimate_standard_deviation = sqrt(estimate_variance)
      actual_mean = 0
      actual_standard_deviation = sqrt(r/(r-2))
      relative_error = abs(estimate_mean - actual_mean) / actual_standard_deviation
      errors[i, j] = relative_error
    }
  }
  return(errors)
}
n <- 50
r <- 5
N <- c(5000, 10000, 20000)
z <- c(-1.5, 0, 1.5)
TLC(r,n,N,z)

#exercitiul 3
aprox_prob <- function(n, p, h, k)
{
  q1 = (k - 0.5 - n*p)/sqrt(n*p*(1-p))
  q2 = (h + 0.5 - n*p)/sqrt(n*p*(1-p))
  prob = 1 - pnorm(q1 - q2)
  return(prob)
}
n <- 20
p <- 0.4
h <- 3
k <- 4
aprox_prob(n, p, h, k)