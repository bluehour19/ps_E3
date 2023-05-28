#exercitiul 1
#a)
find_M <- function(x, k)
{
  n <- length(x)
  count_if_element <- floor(n/2) + 1
  for (i in 1:k)
  {
    index <- sample(1:n, 1)
    value <- x[index]
    value_count <- sum(x == value)
    if(value_count >= count_if_element)
    {
      return(value)
    }
  }
  return ("x does not have M-element")
}
x <- c(1, 2, 3, 4)
k <- 100
find_M(x, k)
#b) 1/2^k < 10^(-7), k > log(1/10^(-7))/ log(2), putem uti;iza funtia ceiling pt a obtine cea mai mica valoare intreaga ai mare decat log(1/10^(-7))/ log(2), astfel k=24

#exercitiul 2
element_ith <- function(i,A)
{
  n <- length(A)
  if(n == 1)
  {
    return (A)
  }
  z <- sample(A,size = 1)
  less <- A[A < z]
  more <- A[A > z]
  if (length(less) >= i)
  {
    return(element_ith(i,less))
  }
  else if (n > i + length(more))
  {
    return(z)
  }
  else 
  {
    return(element_ith(i - n + length(more), more))
  }
}
A <- c(1, 2, 3, 4)
i <- 2
element_ith(i, A)

#exercitiul 3
#a)
monte_carlo <- function(S, a)
{
  n <- length(S)
  m <- floor(a * log(n))
  S1 <- sample(S, m)
  S2 <- sort(S1)
  m_index <- ceiling(m / 2)
  m_value <- S2[m_index]
  return(m_value)
}
S <- c(1, 2, 3, 4)
a <- 0.4
monte_carlo(S, a)

#b)
monte_carlo <- function(S, a)
{
  n <- length(S)
  m <- floor(a * log(n))
  S1 <- sample(S, m)
  S2 <- sort(S1)
  m_index <- ceiling(m / 2)
  m_value <- S2[m_index]
  return(m_value)
}
min <- function(a, tprob)
{
  n <- 1
  prob <- 0
  while(prob < tprob)
  {
    n <- n+1
    S <- 1:n
    median <- monte_carlo (S, a)
    prob <- sum(S <= median) / n^2
  }
  return(n)
}
a <- 0.4
tprob <- 1 - 10^(-7)
min(a, tprob)