#exercitiul 1
#a)
plot_probs <- function (l, p, n, k)
{
  x <- k:n
  poisson_prob <- dpois(x, lambda = l)
  geometric_prob <- dgeom(x, prob = p)
  binomial_prob <- dbinom(x, size = n - k, prob = p)
  plot(x, poisson_prob, type = "b", xlab = "k", ylab = "Probabilities")
  lines(x, geometric_prob, type = "b")
  lines(x, binomial_prob, type = "b")
}

l <- 3
p <- 0.5
n <- 20
k <- 2
plot_probs(l, p, n, k)

#b)
p <- 0.4
k <- seq(1, 1000, by = 2) #nr impare
prob_impar <- sum(dgeom(k, prob = p))
k <- seq(4, 1000)
prob_mare_egal_4 <- sum(dgeom(k, prob = p))
k <- seq(0, 20)
prob_mic_egal_20 <- sum(dgeom(k, prob = p))
prob_impar
prob_mare_egal_4
prob_mic_egal_20

#c
lambda <- 2
k <- 0
limit <- 10^(-7)
prob <- 1
while(prob >= limit)
{
  k <- k + 1
  prob <- 1 - ppois(k - 1, lambda) # 1-probabilitatea ca variabila lambda sa fie mai mica decat k-1
}
k

#exercitiul 2
#a)
statistics <- function(filename)
{
  data <- read.csv(filename, header = T, sep = ",", colClasses=c('numeric','numeric'))
  P_data <- data[['P']]
  S_data <- data[['S']]
  median_value_P <- median(P_data)
  median_value_S <- median(S_data)
  mean_value_P <- mean(P_data)
  mean_value_S <- mean(S_data)
  sd_P <- sd(P_data)
  sd_S <- sd(S_data)
  quantiles_P <- as.vector(quantile(P_data))
  quantiles_S <- as.vector(quantile(S_data))
  print(paste("Median P", median_value_P))
  print(paste("Median S", median_value_S))
  print(paste("Mean P", mean_value_P))
  print(paste("Mean S", mean_value_S))
  print(paste("Standard deviation P", sd_P))
  print(paste("Standard deviation S", sd_S))
  print(paste("Quantiles P", quantiles_P))
  print(paste("Quantiles S", quantiles_S))
}
statistics("note.csv")

#b)
outliers <- function(filename, sample_name)
{
  data <- read.csv(filename, header = T, sep = ",", colClasses=c('numeric','numeric'))
  if ( sample_name == "P")
  {
    vector <- v[['P']]
  }
  else vector <- v[['S']]
  sd <- sd(vector)
  mean <- mean(vector)
  outliers <- vector()
  j <- 1
  for(i in 1:length(vector))
  {
    if(vector[i] >= mean-2*sd & vector[i] <= mean+2*sd)
    {
      outliers[j] <- vector[i]
      j <- j+1
    }
  }
 return(outliers)
}
final_data_P <- outliers("note.csv", "P")
final_data_S <- outliers("note.csv", "S")

#c)
distribution <- function(filename)
{
  data <- read.csv(filename, header = T, sep = "")
  data <- data[['data2']]
  a = 9
  hist(data, breaks = a,  main = "Frequency distribution", xlab = "Intervals", ylab = "Frequency")
}
distribution("note.csv")