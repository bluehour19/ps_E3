#ex1
parabola_area = function(N) {
  count = 0;
  for (i in 1:N) {
    x = runif(1, 0, 2);
    y = runif(1, 0, 9/8);
    if (y <= -2*x^2 + 5*x - 2)
      count = count + 1;
  }
  return (2*9/8*count/N);
}
#ex2
MC_improved_integration = function(N) 
  {
  sum = 0;
  for(i in 1:N) {
    u = rexp(1, 3);
    sum = sum + exp(-2*u*u*2);
  }
  return(sum/N);
}
MC_imprvd_integr_average= function(k, N) 
{
  estimates = 0;
  for(i in 1:k)
    estimates[i] = MC_improved_integration(N);
  print(mean(estimates));
  print(sd(estimates));
}

#ex3
lambda1 = 4
lambda2 = 12
U = runif(1)
if (U <= 3/4)
{
  X = rexp(1, lambda2)
} else
{
  X = rexp(1, lambda1)
}
expectation = 1/X
print(cat("Estimated expectation of X", expectation, "\n"))
