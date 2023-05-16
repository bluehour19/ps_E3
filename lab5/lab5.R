random_variable <- function(values, probabilities)
{
  sum <- 0
  index <- 1
  U<-runif(1)
  while(index <= length(probabilities))
  {
    sum <- sum + probabilities[index]
    if(sum <= U && U < sum + probabilities[index+1])
    {
      return(values[index+1])
    }
    index <- index + 1
  }
  return(NULL)
}

V<-c(1,2,3)
P<-c(0.2,0.3,0.5)
result <-random_variable(V,P)
print(result)