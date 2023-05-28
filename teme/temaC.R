#exercitiul 1
volume <- function(N, a)
{
  errors = vector()
  for(i in 1:length(a))
  {
    actual_value = pi*(a[i]^2)/2
    for(j in 1:length(N))
    {
      N_C = 0;
      for(k in 1:N[j])
      {
        x1 = runif(1, -sqrt(a[i]), sqrt(a[i]))
        x2 = runif(1, -sqrt(a[i]), sqrt(a[i]))
        x3 = runif(1, 0, a[i])
        if( x3 >= x1^2 + x2^2 & x3 <= a[i] )
        {
          N_C = N_C + 1
        }
      }
      estimate_value = (4*a^3*N_C)/N[j]
      relative_error = abs(estimate_value - actual_value)/actual_value
      errors[j] = relative_error
    }
    print(errors)
  }
  
}
a <- c(2, 4, 10)
N <- c(100000, 200000, 500000)
volume(N,a)
#exercitiul 2
# 3y <= x+6, y<=(x+6)/3, y >=(12-3x), (x+6)/3 = 12 -3x, x + 6 = 36 - 9x, 10x = 30, x = 3
# y = (3 + 6)/3 = 3
area <- function(N)
{
  N_C=0
  for(i in 1:N)
  {
    x = runif(1, 0, 3)
    y = runif(1, 0, 3)
    if( 3*y <= x + 6 & y <= 12 - 3*x)
    {
      N_C = N_C + 1
    }
  }
  actual_value = 9
  estimated_value = actual_value * N_C / N
  print(estimated_value)
}
area(20000)
#exercitiul 3
#a)
MC_improved_integration = function() 
{
  s = 0;
  x = runif(1, -1, 1);
  s = s + ((x + 1)/sqrt(4-x*x));
  return(s);
}
value <- pi/3
value
MC_improved_integration()
#b)
MC_improved_integration = function() 
{
  s = 0;
  x = runif(1, -10, 0);
  s = s + (1/(x*x+4));
  return(s);
}
value <- pi/4
value
MC_improved_integration()
#c)
MC_improved_integration = function() 
{
  s = 0;
  x = runif(1, -10, 0);
  s = s + x*exp(x);
  return(s);
}
value <- -1
value
MC_improved_integration()

#exercitiul 4
#a)
m <- 100000 #rn de conturi false
p <- 0.5 #probabilitatea de succes
q <- 0.1 #probabilitatea de dezactivare a conturilor
number_days <- m / (p * (1 - q))
number_days
#b)
n <- 500
p <- 0.5
prob <- pbinom(50000, 40 , p)
prob
#c)
prob <- pbinom(50000, 40 , p)
tprob <- prob
error <- 0.01
c <- 0.99
z <- qnorm((1 + c)/2)
n_max <- 10000
while (prob < tprob && n < n_max)
{
  n <- n + 1
  prob <- pbinom(50000, 40 , p)
}
n
prob